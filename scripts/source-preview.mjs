import {readFile, writeFile, mkdir, readdir} from 'node:fs/promises';
import {execFileSync} from 'node:child_process';
import path from 'node:path';
import {createHash} from 'node:crypto';

// Corrections checked against the rendered pages, scoped to these exact sample PDFs.
const corrections = JSON.parse(await readFile(new URL('./source-preview-corrections.json', import.meta.url), 'utf8'));

// Offline editorial experiment: reuse saved previews, without API or account access.
const source = path.resolve(process.argv[2] || 'reports/ai-preview');
const destination = path.resolve(process.argv[3] || 'reports/source-preview');
const normalize = text => text.normalize('NFKC').replace(/\u00ad/g, '')
  .replace(/([\p{L}])[-‐]\s*\n\s*([\p{L}])/gu, '$1$2').replace(/\s+/g, ' ').trim();
function section(pages, title) {
  for (const [index, text] of pages.entries()) {
    // Require heading and number on the same line, excluding contents entries.
    const heading = new RegExp(`^\\d+(?:\\.\\d+)* +(?:${title}) *$`, 'mu');
    const match = heading.exec(text);
    if (!match) continue;
    let rest = text.slice(match.index + match[0].length);
    const next = rest.search(/^\d+(?:\.\d+)+ +\p{L}/mu);
    if (next >= 0) rest = rest.slice(0, next);
    // Preview only the prose, excluding figures, participant lists and page furniture.
    rest = rest.split(/\n(?:Abbildung \d+|An dem Ereignis Beteiligte|Seite \d+ von)/u)[0];
    if (normalize(rest).length < 40 || /^\s*\d+\s*$/u.test(rest)) continue;
    return {text: normalize(rest), page: index + 1};
  }
  return null;
}
const reports = [];
for (const name of await readdir(source)) {
  let previous;
  try { previous = JSON.parse(await readFile(path.join(source, name, 'comparison.json'), 'utf8')); }
  catch (error) { if (['ENOENT', 'ENOTDIR'].includes(error.code)) continue; throw error; }
  const pdf = path.join(source, name, 'report.pdf');
  const pages = execFileSync('pdftotext', [pdf, '-'], {encoding: 'utf8', maxBuffer: 20_000_000}).split('\f');
  const summary = section(pages, 'Zusammenfassung des Ereignisses|Kurzbeschreibung des Ereignisses');
  const consequences = section(pages, 'Folgen');
  const repair = corrections[name];
  if (repair && createHash('sha256').update(await readFile(pdf)).digest('hex') === repair['pdf-sha256']) {
    for (const passage of [summary, consequences].filter(Boolean)) {
      passage.extracted = passage.text;
      for (const [from, to] of repair.replacements) passage.text = passage.text.replaceAll(from, to);
    }
  }
  if (summary) {
    summary.extracted ||= summary.text;
    summary.text = summary.text.replace(/, siehe Abbildung \d+\./gu, '.')
      .replace(/ In Abbildung \d+ ist .*? dargestellt\./gu, '');
  }
  if (!summary) throw new Error(`No event description found: ${name}`);
  const relative = filename => path.relative(destination, filename).split(path.sep).join('/');
  reports.push({id: name, title: previous.report.post.title, report: previous.report,
    limits: previous.limits, summary, consequences, old: previous.ai.posts,
    pdf: relative(pdf), previous: relative(path.join(source, name, 'index.html')),
    images: previous.ai.pages.map(page => ({page: page.page, reason: page.reason,
      src: relative(path.resolve(page['image-path']))}))});
}
if (!reports.length) throw new Error('No saved report previews found');
await mkdir(destination, {recursive: true});
await writeFile(path.join(destination, 'sources.json'), JSON.stringify(reports, null, 2));
const template = await readFile(new URL('./source-preview.html', import.meta.url), 'utf8');
await writeFile(path.join(destination, 'index.html'), template.replace('/*REPORT_DATA*/[]', JSON.stringify(reports).replace(/</g, '\\u003c')));
console.log(`Saved ${reports.length} comparisons: ${path.join(destination, 'index.html')}`);
