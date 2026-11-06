#!/usr/bin/env python3
"""Reproduce the extraction comparison on locally saved report PDFs."""
import argparse
import difflib
import hashlib
import html
import json
from pathlib import Path
import re
import subprocess

import pymupdf
from pdfminer.high_level import extract_text
from pypdf import PdfReader

from repair_pdf_text import repair


def poppler(path):
    return subprocess.check_output(['pdftotext', str(path), '-'], text=True)


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--input-dir', type=Path, default=Path('reports/ai-preview'))
    parser.add_argument('--out-dir', type=Path, required=True)
    args = parser.parse_args()
    args.out_dir.mkdir(parents=True, exist_ok=True)
    results, comparisons, seen = [], [], set()
    for source in sorted(args.input_dir.glob('*/report.pdf')):
        sha = hashlib.sha256(source.read_bytes()).hexdigest()
        if sha in seen:
            continue
        seen.add(sha)
        folder = args.out_dir / source.parent.name
        folder.mkdir(exist_ok=True)
        after_pdf = folder / 'repaired.pdf'
        diagnostics = repair(source, after_pdf)
        before, after = poppler(source), poppler(after_pdf)
        (folder / 'before.txt').write_text(before)
        (folder / 'after.txt').write_text(after)
        before_pages, after_pages = before.split('\f'), after.split('\f')
        index = next(i for i, text in enumerate(before_pages)
                     if re.search(r'^\d+(?:\.\d+)* +(?:Kurzbeschreibung|Zusammenfassung) des Ereignisses\s*$', text, re.M))
        samples = {
            'poppler': before_pages[index],
            'mutool': subprocess.check_output(['mutool', 'draw', '-q', '-F', 'txt', '-o', '-', str(source), str(index + 1)], text=True),
            'pypdf': PdfReader(source).pages[index].extract_text(),
            'pymupdf': pymupdf.open(source)[index].get_text(),
            'pdfminer': extract_text(source, page_numbers=[index]),
        }
        repaired_sample = extract_text(after_pdf, page_numbers=[index])
        # Check every page, not just the example page. ToUnicode edits must not change pixels.
        original_doc, repaired_doc = pymupdf.open(source), pymupdf.open(after_pdf)
        assert len(original_doc) == len(repaired_doc)
        for original_page, repaired_page in zip(original_doc, repaired_doc):
            assert original_page.get_pixmap().samples == repaired_page.get_pixmap().samples
        diagnostics.update({'example-page': index + 1, 'all-pages-pixel-identical': True,
                            'page-missing-before': len(re.findall(r'\(cid:\d+\)', samples['pdfminer'])),
                            'page-missing-after': len(re.findall(r'\(cid:\d+\)', repaired_sample))})
        (folder / 'diagnostics.json').write_text(json.dumps(diagnostics, ensure_ascii=False, indent=2) + '\n')
        (folder / 'extractor-comparison.json').write_text(json.dumps(samples, ensure_ascii=False, indent=2) + '\n')
        results.append(diagnostics)
        comparisons.append('<h2>' + html.escape(source.parent.name) + ' · PDF page ' + str(index + 1) + '</h2>' +
                           '<p>' + str(diagnostics['mappings-added']) + ' mappings added; all pages render identically. ' +
                           'Missing-glyph markers on example page: ' + str(diagnostics['page-missing-before']) + ' → ' + str(diagnostics['page-missing-after']) + '</p>' +
                           difflib.HtmlDiff(wrapcolumn=90).make_table(before_pages[index].splitlines(), after_pages[index].splitlines(), 'Original Poppler extraction', 'After font-map repair'))
        print(source.parent.name, len(original_doc), 'pages checked;', diagnostics['mappings-added'], 'mappings added;',
              diagnostics['page-missing-before'], '→', diagnostics['page-missing-after'], 'missing on example page', flush=True)
    (args.out_dir / 'audit.json').write_text(json.dumps(results, ensure_ascii=False, indent=2) + '\n')
    (args.out_dir / 'index.html').write_text('<!doctype html><html lang="en"><meta charset="utf-8"><title>PDF extraction audit</title>'
        '<style>body{font:16px system-ui;margin:2rem}table{border-collapse:collapse;width:100%;font:13px monospace}td{vertical-align:top;padding:3px}.diff_add{background:#cfc}.diff_sub{background:#fcc}.diff_chg{background:#ffa}.diff_next{display:none}</style>'
        '<h1>PDF extraction: before and after</h1><p>Only exact known glyph shapes were mapped. No words were guessed. '
        'The original PDFs remain untouched. Unknown fonts and glyphs can still contain extraction errors.</p>' + ''.join(comparisons) + '</html>')


if __name__ == '__main__':
    main()
