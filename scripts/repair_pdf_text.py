#!/usr/bin/env python3
"""Deterministic BEU PDF text repair; never edits the input.

Repairs exact, visually verified ligature and bullet glyph mappings.
No spelling rules, global CID replacements, OCR, or AI. See docs/pdf-text-extraction.md.
"""
import argparse
import hashlib
import io
import json
import re
from pathlib import Path
import subprocess

from fontTools.pens.recordingPen import DecomposingRecordingPen
from fontTools.ttLib import TTFont
from pypdf import PdfReader, PdfWriter
# Pinned to pypdf 7.0.0: this private parser preserves existing bfchar/bfrange entries.
from pypdf._cmap import _parse_to_unicode
from pypdf.generic import DecodedStreamObject, NameObject

REGISTRY = Path(__file__).with_name('pdf-ligature-glyphs.json')


def glyph_fingerprint(font, name):
    glyphs = font.getGlyphSet()
    pen = DecomposingRecordingPen(glyphs)
    glyphs[name].draw(pen)
    value = {'units': font['head'].unitsPerEm,
             'advance': font['hmtx'][name][0], 'outline': pen.value}
    return hashlib.sha256(json.dumps(value, separators=(',', ':')).encode()).hexdigest()


def unicode_cmap(mapping):
    entries = [f'<{code:04X}> <{text.encode("utf-16-be").hex().upper()}>'
               for code, text in sorted(mapping.items())]
    lines = ['/CIDInit /ProcSet findresource begin', '12 dict begin', 'begincmap',
             '/CIDSystemInfo << /Registry (Adobe) /Ordering (UCS) /Supplement 0 >> def',
             '/CMapName /BEULigatureRepair def', '/CMapType 2 def',
             '1 begincodespacerange', '<0000> <FFFF>', 'endcodespacerange']
    for start in range(0, len(entries), 100):
        block = entries[start:start + 100]
        lines.extend([f'{len(block)} beginbfchar', *block, 'endbfchar'])
    lines.extend(['endcmap', 'CMapName currentdict /CMap defineresource pop', 'end', 'end'])
    return '\n'.join(lines).encode('ascii')


def font_repairs(resource, registry):
    """Return a replacement map only for the supported, unambiguous encoding."""
    if resource.get('/Subtype') != '/Type0':
        return None
    if resource.get('/Encoding') != '/Identity-H' or '/ToUnicode' not in resource:
        return None
    descendants = resource.get('/DescendantFonts')
    if not descendants or len(descendants) != 1:
        return None
    descendant = descendants[0].get_object()
    if descendant.get('/Subtype') != '/CIDFontType2' or descendant.get('/CIDToGIDMap') != '/Identity':
        return None
    descriptor = descendant.get('/FontDescriptor', {})
    font_file = descriptor.get('/FontFile2')
    if font_file is None:
        return None
    original_stream = resource['/ToUnicode']
    if original_stream.get('/UseCMap') or b'usecmap' in original_stream.get_data():
        return None
    parsed, _ = _parse_to_unicode(resource)
    ranges = re.findall(rb"begincodespacerange(.*?)endcodespacerange", original_stream.get_data(), re.S)
    if not ranges or any(not re.fullmatch(rb"\s*<[0-9a-fA-F]{4}>\s*<[0-9a-fA-F]{4}>\s*", block)
                         for block in ranges):
        return None
    mapping = {}
    for code, text in parsed.items():
        if code == -1:
            continue
        if not isinstance(code, str) or len(code) != 1 or not isinstance(text, str) or not text:
            return None
        mapping[ord(code)] = text
    font = TTFont(io.BytesIO(font_file.get_data()))
    if not all(table in font for table in ('glyf', 'head', 'hmtx')):
        return None
    changes = []
    for gid, name in enumerate(font.getGlyphOrder()):
        if gid == 0 or font['glyf'][name].numberOfContours == 0:
            continue
        existing = mapping.get(gid)
        # Existing text is preserved unless this exact outline AND previous value
        # have an explicitly verified replacement (e.g. SymbolMT's PUA bullet).
        if existing is not None and not any(existing in item.get('replace', []) for item in registry.values()):
            continue
        fingerprint = glyph_fingerprint(font, name)
        entry = registry.get(fingerprint)
        if entry is not None and (existing is None or existing in entry.get('replace', [])):
            mapping[gid] = entry['text']
            changes.append({'cid': gid, 'text': entry['text'], 'previous-text': existing, 'glyph-sha256': fingerprint})
    return (mapping, changes) if changes else None


def repair(source, destination, registry_path=REGISTRY):
    source, destination = Path(source), Path(destination)
    if source.resolve() == destination.resolve():
        raise ValueError('Input and output PDF must be different files')
    if destination.exists():
        raise FileExistsError(f'Refusing to overwrite {destination}')
    registry = json.loads(Path(registry_path).read_text())['glyphs']
    reader = PdfReader(source)
    writer = PdfWriter(reader, incremental=True)
    changes = []
    # Include font objects used in Form XObjects and diagrams, not just page resources.
    for resource in list(writer._objects):
        if resource is None or not hasattr(resource, 'get') or resource.get('/Type') != '/Font':
            continue
        candidate = font_repairs(resource, registry)
        if candidate:
            mapping, additions = candidate
            stream = DecodedStreamObject()
            stream.set_data(unicode_cmap(mapping))
            resource[NameObject('/ToUnicode')] = writer._add_object(stream)
            changes.append({'font': str(resource.get('/BaseFont')), 'mappings': additions})
    destination.parent.mkdir(parents=True, exist_ok=True)
    # Exclusive create also protects against another process creating the output meanwhile.
    with destination.open('xb') as output:
        writer.write(output)
    return {'source': str(source), 'source-sha256': hashlib.sha256(source.read_bytes()).hexdigest(),
            'repaired-pdf': str(destination), 'pages': len(reader.pages), 'fonts-repaired': changes,
            'mappings-added': sum(item['previous-text'] is None for font in changes for item in font['mappings']),
            'mappings-corrected': sum(item['previous-text'] is not None for font in changes for item in font['mappings']),
            'scope': 'Exact known glyphs only; zero additions does not prove extraction is correct.'}


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('pdf', type=Path)
    parser.add_argument('--out-dir', type=Path, required=True)
    args = parser.parse_args()
    diagnostics = repair(args.pdf, args.out_dir / 'repaired.pdf')
    for source, output in [(args.pdf, 'before.txt'), (args.out_dir / 'repaired.pdf', 'after.txt')]:
        subprocess.run(['pdftotext', str(source), str(args.out_dir / output)], check=True)
    (args.out_dir / 'diagnostics.json').write_text(json.dumps(diagnostics, ensure_ascii=False, indent=2) + '\n')
    print(json.dumps(diagnostics, ensure_ascii=False, indent=2))


if __name__ == '__main__':
    main()
