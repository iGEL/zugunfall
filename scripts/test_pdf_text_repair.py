import io
from pathlib import Path
import tempfile
import unittest

from fontTools.fontBuilder import FontBuilder
from fontTools.pens.ttGlyphPen import TTGlyphPen
from fontTools.ttLib import TTFont
from pypdf._cmap import _parse_to_unicode
from pypdf.generic import ArrayObject, DecodedStreamObject, DictionaryObject, NameObject

from repair_pdf_text import font_repairs, glyph_fingerprint, repair, unicode_cmap


def fixture(existing=None, altered=False):
    builder = FontBuilder(1000, isTTF=True)
    order = ['.notdef', 'ordinary', 'ligature', 'unknown']
    builder.setupGlyphOrder(order)
    glyphs = {}
    for index, name in enumerate(order):
        pen = TTGlyphPen(None)
        pen.moveTo((0, 0))
        pen.lineTo((100 + index * 40 + (1 if altered and index == 2 else 0), 0))
        pen.lineTo((0, 500))
        pen.closePath()
        glyphs[name] = pen.glyph()
    builder.setupGlyf(glyphs)
    builder.setupHorizontalMetrics({name: (500, 0) for name in order})
    builder.setupHorizontalHeader(ascent=800, descent=-200)
    builder.setupCharacterMap({65: 'ordinary'})
    builder.setupNameTable({'familyName': 'Test', 'styleName': 'Regular'})
    builder.setupOS2()
    builder.setupPost()
    binary = io.BytesIO()
    builder.save(binary)
    font = TTFont(io.BytesIO(binary.getvalue()))
    file = DecodedStreamObject()
    file.set_data(binary.getvalue())
    cmap = DecodedStreamObject()
    cmap.set_data(unicode_cmap({1: 'X', **(existing or {})}))
    resource = DictionaryObject({
        NameObject('/Type'): NameObject('/Font'),
        NameObject('/Subtype'): NameObject('/Type0'),
        NameObject('/Encoding'): NameObject('/Identity-H'),
        NameObject('/ToUnicode'): cmap,
        NameObject('/DescendantFonts'): ArrayObject([DictionaryObject({
            NameObject('/Subtype'): NameObject('/CIDFontType2'),
            NameObject('/CIDToGIDMap'): NameObject('/Identity'),
            NameObject('/FontDescriptor'): DictionaryObject({NameObject('/FontFile2'): file}),
        })]),
    })
    return resource, {glyph_fingerprint(font, 'ligature'): {'text': 'tt'}}


class RepairTest(unittest.TestCase):
    def test_only_exact_known_outline_gets_missing_mapping(self):
        resource, registry = fixture()
        mapping, changes = font_repairs(resource, registry)
        self.assertEqual(mapping, {1: 'X', 2: 'tt'})
        self.assertEqual([change['cid'] for change in changes], [2])
        altered, _ = fixture(altered=True)
        self.assertIsNone(font_repairs(altered, registry))

    def test_preserve_existing_mapping_even_if_registry_disagrees(self):
        resource, registry = fixture({2: 'Ʃ'})
        self.assertIsNone(font_repairs(resource, registry))

    def test_verified_symbol_replacement_requires_shape_and_previous_value(self):
        resource, registry = fixture({2: '\uf0b7'})
        self.assertIsNone(font_repairs(resource, registry))
        entry = next(iter(registry.values()))
        entry.update({'text': '•', 'replace': ['\uf0b7']})
        mapping, changes = font_repairs(resource, registry)
        self.assertEqual(mapping, {1: 'X', 2: '•'})
        self.assertEqual(changes[0]['previous-text'], '\uf0b7')
        altered, _ = fixture({2: '\uf0b7'}, altered=True)
        self.assertIsNone(font_repairs(altered, registry))
        legitimate, _ = fixture({2: 'X'})
        self.assertIsNone(font_repairs(legitimate, registry))
        resource['/ToUnicode'].set_data(unicode_cmap(mapping))
        self.assertIsNone(font_repairs(resource, registry))

    def test_encoding_and_cid_to_gid_must_be_supported(self):
        resource, registry = fixture()
        resource[NameObject('/Encoding')] = NameObject('/Identity-V')
        self.assertIsNone(font_repairs(resource, registry))
        resource[NameObject('/Encoding')] = NameObject('/Identity-H')
        resource['/DescendantFonts'][0][NameObject('/CIDToGIDMap')] = DecodedStreamObject()
        self.assertIsNone(font_repairs(resource, registry))

    def test_preserve_bfrange_and_unicode_sequences(self):
        resource, registry = fixture()
        source = resource['/ToUnicode'].get_data().replace(
            b'1 beginbfchar\n<0001> <0058>\nendbfchar',
            b'1 beginbfrange\n<0001> <0001> <0058>\nendbfrange')
        resource['/ToUnicode'].set_data(source)
        mapping, _ = font_repairs(resource, registry)
        self.assertEqual(mapping[1], 'X')
        mapping[3] = '😀'
        resource['/ToUnicode'].set_data(unicode_cmap(mapping))
        parsed, _ = _parse_to_unicode(resource)
        self.assertEqual(parsed['\x02'], 'tt')
        self.assertEqual(parsed['\x03'], '😀')
        self.assertIsNone(font_repairs(resource, registry))

    def test_refuse_to_overwrite_input_or_existing_output(self):
        with tempfile.TemporaryDirectory() as folder:
            source = Path(folder) / 'source.pdf'
            source.write_bytes(b'untouched')
            with self.assertRaises(ValueError):
                repair(source, source)
            output = Path(folder) / 'out.pdf'
            output.write_bytes(b'existing')
            with self.assertRaises(FileExistsError):
                repair(source, output)
            self.assertEqual(source.read_bytes(), b'untouched')
            self.assertEqual(output.read_bytes(), b'existing')


if __name__ == '__main__':
    unittest.main()
