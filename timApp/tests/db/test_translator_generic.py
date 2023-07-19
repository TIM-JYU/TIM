"""
Contains prototypal testing for a generic model of translating from language
to another.
"""

__authors__ = [
    "Noora Jokela",
    "Riku Lehkonen",
    "Vili Moisala",
    "Juho Tarkkanen",
    "Sami Viitanen",
]
__license__ = "MIT"
__date__ = "25.4.2022"


from timApp.document.translation.language import Language
from timApp.document.translation.translationparser import (
    Translate,
    NoTranslate,
)
from timApp.document.translation.reversingtranslator import ReversingTranslationService
from timApp.tests.server.test_translation import TimTranslationTest
from timApp.tests.unit.test_translator_parser import TimTranslationParserTest


# NOTE Needs to inherit from TimDbTest in order to create Language-objects.
class TestGenericTranslator(TimTranslationTest, TimTranslationParserTest):
    def test_mixed(self):
        none_lang = Language(lang_code="", lang_name="", autonym="")
        translator = ReversingTranslationService()
        # TODO A more extensive test input might need to be used here.
        parts = self.parser.get_translate_approvals(
            "Here I am [**rocking**]{.red} [like](www.example.com) "
            "a ![hurricane](/imgs/hurricane.png)!"
        )

        # Sanity checks.
        self.assertEqual(13, len(parts))
        for x in parts:
            self.assertIsInstance(x.text, str)

        self.assertEqual(
            # TODO Implement tag-protection on the ReversingTranslator in
            #  order to better simulate an XML-handling translation service.
            [
                " ma I ereH\n[>b/<gnikcor>b<]{.red} [ekil](www.example.com) "
                "a ![enacirruh](/imgs/hurricane.png)\n!"
            ],
            translator.translate([parts], none_lang, self.reverselang),
        )

    def test_all_translate(self):
        none_lang = Language(lang_code="", lang_name="", autonym="")
        translator = ReversingTranslationService()
        # TODO A more extensive test input might need to be used here.
        parts = self.parser.get_translate_approvals("Here I am")

        # Sanity checks.
        self.assertEqual(1, len(parts))
        for x in parts:
            self.assertIsInstance(x, Translate)
            self.assertIsInstance(x.text, str)

        self.assertEqual(
            ["\nma I ereH\n"],
            translator.translate([parts], none_lang, self.reverselang),
        )

    def test_all_notranslate(self):
        none_lang = Language(lang_code="", lang_name="", autonym="")
        translator = ReversingTranslationService()
        # TODO A more extensive test input might need to be used here.
        parts = self.parser.get_translate_approvals("[]()[]{}![]()")

        # Sanity checks.
        self.assertEqual(3, len(parts))
        # The first and last item are Translate("\n").
        for x in parts[1:-1]:
            self.assertIsInstance(x, NoTranslate)
            self.assertIsInstance(x.text, str)

        self.assertEqual(
            # NOTE "{}" get omitted by parsing (maybe because they are
            # optional in Markdown?)
            ["\n[]()[]![]()\n"],
            translator.translate([parts], none_lang, self.reverselang),
        )

    def test_empty(self):
        none_lang = Language(lang_code="", lang_name="", autonym="")
        translator = ReversingTranslationService()
        self.assertEqual(
            [],
            translator.translate([], none_lang, self.reverselang),
        )
