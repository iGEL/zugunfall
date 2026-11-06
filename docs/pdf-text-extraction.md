# BEU PDF text extraction investigation

The missing letters come from incomplete font-to-text mappings in the source PDFs.
They can be repaired deterministically for recognized glyphs. Changing extractors
alone did not recover them in the tested documents.

## What is broken

In the Emmerich report (`cw_uyfxzZ1`), the regular Calibri CID font has an
`Identity-H` encoding and an identity CID-to-glyph mapping. A CID identifies a glyph;
it is **not** a Unicode character. Its `ToUnicode` stream maps only three codes:

```text
0003 → U+00A0 (nonbreaking space)
012B → U+FB00 (ff ligature)
0372 → U+2010 (hyphen)
```

The PDF also uses glyphs 297, 332, 415 and 425 for `fb`, `ft`, `ti` and `tt`.
Their outlines are present and render correctly, but their Unicode mappings are
missing. This explains “Ro erdam”, “Hauptlu leitung” and “Informa onen”. These are
missing text-layer mappings, not OCR mistakes or ordinary word spacing.

The embedded subset fonts have no `cmap`, `post` or `GSUB` tables. They contain the
shapes, metrics and font family name, but lack usable glyph names and the rules
that could identify their letter sequences automatically. Generic font-name or
GSUB-based recovery cannot recover what has been stripped. In an intact font,
[GSUB describes how letter sequences become ligatures](https://learn.microsoft.com/en-us/typography/opentype/otspec182/gsub).
A [similar Calibri extraction issue](https://github.com/yfedoseev/pdf_oxide/issues/444)
is documented upstream; that is corroboration, not evidence about how these
particular BEU PDFs were produced.

## Other tools tested

All five extractors were tested on the same event-description pages.
Versions were Poppler 24.02.0, mutool 1.23.10, pypdf 7.0.0, PyMuPDF 1.26.7,
and pdfminer.six 20260107. PyMuPDF 1.26.7 was used as a separate comparison;
it is not a claim about the latest available version.

| Extractor | Emmerich result | Assessment |
| --- | --- | --- |
| `pdftotext` | `Ro erdam` | Drops the unmapped glyph; the remaining text hides its identity. |
| `mutool draw -F txt` | `Ro�erdam` | Exposes a decoding failure, but cannot identify the letters. |
| pypdf | `RoƩerdam` | Falls back to CID 425 as a character value, producing plausible-looking garbage. |
| PyMuPDF | `RoƩerdam` | Same problem with its default CID fallback. |
| pdfminer.six | `Ro(cid:425)erdam` | Useful for diagnostics because the missing glyph stays explicit. |

MuPDF exposes [options for handling unknown Unicode mappings](https://mupdf.readthedocs.io/en/1.28.6/reference/common/stext-options.html),
but preserving a CID does not establish its meaning. Likewise, switching
`pdftotext` to `-layout` preserves more positioning but does not supply missing
Unicode mappings. A newer extractor might improve layout or diagnostics; it
cannot reliably infer absent arbitrary glyph meanings from this stripped font.
OCR is a possible fallback because it reads the visible shapes, but it introduces
another recognition step and was not needed or evaluated in this experiment.

## Tested repair

The prototype in `scripts/repair_pdf_text.py` recognizes **exact glyph outlines**.
A rendered [glyph atlas](../reports/text-repair/verified/glyph-atlas.png) was used
to verify 13 regular-Calibri ligatures: `fb`, `ffb`, `ff`, `fh`, `fk`, `ffl`, `ft`,
`fft`, `tf`, `ti`, `tt`, `ttf`, and `tti`.

The registry fingerprints each glyph using its units per em, advance width,
and decomposed drawing commands. Glyph numbers and font subset names are not
used as the identity: a different glyph at CID 425 will not match. The fingerprints
were identical across the six distinct reports inspected. Two saved report IDs
contained the same PDF, so that duplicate was tested once.

The tool adds only absent `ToUnicode` entries for recognized shapes. It preserves
existing mappings, including any legitimate Unicode characters resembling CIDs,
with the narrowly verified SymbolMT bullet exception described below.
It requires the tested two-byte Identity-H / identity CID-to-glyph encoding and
skips unsupported encodings and unrecognized outlines. It never substitutes
fragments such as `ha e` or globally replaces `Ʃ` with `tt`.

It writes an incremental update to a separate PDF. The original PDF remains
untouched, and its exact bytes remain the prefix of the repaired copy. The
prototype then uses the existing `pdftotext` tool to extract that repaired copy.

| Report | Physical pages checked for identical rendering | Font mappings added | Missing-glyph occurrences on the event-description example page, before → after |
| --- | ---: | ---: | ---: |
| Rosenheim–Pfraundorf | 28 | 18 | 4 → 0 |
| Geseke | 83 | 27 | 8 → 0 |
| Hamburg-Harburg–Hittfeld | 64 | 9 | 10 → 0 |
| Bremen | 42 | 40 | 9 → 0 |
| Emmerich | 5 | 4 | 7 → 0 |
| Nordstemmen | 25 | 28 | 2 → 0 |
| Total | 247 | 126 | 40 → 0 |

Every physical page was rasterized before and after with PyMuPDF at its default
resolution; pixel bytes matched exactly. The repair removes all pdfminer missing-CID
markers on the six example pages and restores the observed words in Poppler's
output. This is not a claim that every character in every report is now correct.
Bold/italic variants and other fonts are not covered by the current registry;
unknown glyphs elsewhere can still need review. Visual equality alone does not
prove a mapping is semantically right; that depends on the visually verified
registry. The five unit tests also check that unknown/altered shapes are not
repaired, existing mappings survive, unsupported encodings are skipped, Unicode
sequences round-trip, and existing files cannot be overwritten.

Open the [before/after HTML comparison](../reports/text-repair/verified/index.html).
Its folder contains `audit.json`, plus each report's `before.txt`, `after.txt`,
`repaired.pdf`, `diagnostics.json`, and `extractor-comparison.json`.

## Bullet lists

The Rosenheim–Pfraundorf report contains a SymbolMT bullet glyph (CID 120).
Its existing `ToUnicode` entry points to the private-use value U+F0B7 rather than
U+2022 (`•`). The registry now includes the visually verified outline and an
explicit `replace: ["\uF0B7"]` allowance. Replacing an existing value requires both
that exact glyph fingerprint and that exact previous value; other mappings and
unrecognized private-use characters remain untouched. Diagnostics distinguish
`mappings-added` from `mappings-corrected`.

Poppler also emits the bullet alone on a line, often followed by a blank line
before the item text. Preview formatting attaches such markers to the next item
line. Full-page alt text retains the other page content and line wrapping. Post
composition joins wrapped item text but keeps each item on its own line, preserving
paragraph breaks after lists. Known numbered section headings are not attached to
orphan bullet markers. Inline mathematical dots are left in place. No AI is used
for these repairs; the shortening prompt is also instructed to preserve lists.

Regression checks on physical page 9 of the Rosenheim PDF confirm seven readable
bullet items in both the copied Mastodon text and full alt text. All 28 physical
pages render identically after the font-map repair. The preview composition version
was incremented so the earlier flattened-text cache is not reused.

## Reproduce

The repair tool needs Python 3.11+, Poppler, pypdf and fontTools. It needs no API key.
Use a fresh output directory; existing output PDFs are never overwritten.

```sh
python3 -m venv /tmp/beu-pdf-tools
/tmp/beu-pdf-tools/bin/pip install -r scripts/requirements-pdf-repair.txt
/tmp/beu-pdf-tools/bin/python scripts/repair_pdf_text.py \
  reports/ai-preview/cw_uyfxzZ1/report.pdf --out-dir /tmp/beu-emmerich-repaired
/tmp/beu-pdf-tools/bin/python -m unittest discover -s scripts -p 'test_pdf_text_repair.py'
```

For the five-extractor audit, including render comparisons on every page:

```sh
/tmp/beu-pdf-tools/bin/pip install -r scripts/requirements-pdf-audit.txt
/tmp/beu-pdf-tools/bin/python scripts/audit_pdf_extraction.py \
  --input-dir reports/ai-preview --out-dir /tmp/beu-pdf-audit
```

The prototype pins its dependencies because it uses private pypdf CMap parsing
and object APIs. Publishing and the preview tool now share this repair through a dedicated Python
environment (`npm run setup:pdf`, with `setup:preview` as an alias) before composing
text and image alt text. Their shared generation cache includes repaired page text
and a composition version.

## Implications for programmatic posts

A promising pipeline is: repair recognized mappings in a working PDF copy, run
Poppler, then select the original event-description and consequences sections.
Preserve the original PDF and extraction for comparison, and flag unresolved
missing glyphs rather than silently guessing words. pdfminer is useful as an
independent check for unmapped glyphs, though absence of its markers does not
prove the full text is correct.

Layout cleanup remains a separate task. Expand the standard Unicode ligatures
(`ﬀ`, `ﬁ`, `ﬂ`, etc.) to their letter sequences, collapse PDF line wrapping, and
remove repeated headers/footers using layout information. Only remove line-end
hyphens when a word is actually split. “Container-\ntragwagen” can become
“Containertragwagen”, but “Leit- und Sicherungstechnik” must retain its hyphen.
Avoid broad compatibility normalization when symbols or units matter: it can also
change text such as superscripts. These formatting rules cannot repair missing
font mappings after Poppler has discarded their glyph identity.

This supports deterministic composition for the examples: generate the headline,
copy the recovered source sections, and reserve AI for necessary shortening or
explicitly flagged extraction failures.
