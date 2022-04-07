import unittest

from timApp.document.translation.language import Language
from timApp.document.translation.translationparser import (
    TranslateApproval,
    Translate,
    NoTranslate,
    get_translate_approvals,
)
from timApp.tests.db.timdbtest import TimDbTest

from timApp.document.translation.translator import (
    TranslationService,
    LanguagePairing,
    Usage,
)


class ReversingTranslationService(TranslationService):
    """Translator to test the if the TranslateApproval-type is generic enough to implement for new machine translators."""

    def translate(
        self,
        texts: list[TranslateApproval],
        _src_lang: Language,
        _target_lang: Language,
    ) -> list[TranslateApproval]:
        """
        Reverse the translatable text given.
        NOTE The algorithm here for combining translation results back to original structure might be integrated into the actual TranslationService-implementation.
        :param texts: Texts to reverse
        :param _src_lang: Unused
        :param _target_lang: Unused
        :return: Texts where translatable ones have been reversed.
        """

        # Leave non-translatable values out of the call to machine translation and pick their string-valued texts to a list
        only_translates = list(
            map(
                lambda x: x.text,
                filter(lambda x: isinstance(x, Translate), texts),
            )
        )
        # Perform the translation
        translation_results = self._translate(only_translates)
        # Insert translated parts back into their original objects
        # NOTE This only works, if self._translate returns the values in the same order as it got them!!
        i = 0
        for obj in texts:
            if isinstance(obj, Translate):
                obj.text = translation_results[i]
                i += 1

        # TODO See comment about the return value on TranslationService.translate
        return texts

    def usage(self) -> Usage:
        """Infinite quota"""
        import sys

        return Usage(character_count=0, character_limit=sys.maxsize)

    def languages(self) -> LanguagePairing:
        raise NotImplementedError

    def _translate(_self, texts: list[str]) -> list[str]:
        """
        Reverses texts.
        Simulates the call to a machine translator.
        :param texts: List of translatable text.
        :return: List of translated text
        """
        return [x[::-1] for x in texts]

    @classmethod
    def init_translate(cls) -> "ReversingTranslationService":
        # TODO importing here is dumb -> add factory method to the TranslationService-interface
        from timApp.document.translation.translator import init_translate

        none_lang = Language(lang_code="", lang_name="", autonym="")
        return init_translate(ReversingTranslationService(), none_lang, none_lang)

    __mapper_args__ = {"polymorphic_identity": "Reversing"}


TR = Translate
NT = NoTranslate


# NOTE Needs to inherit from TimDbTest in order to create Language-objects
class TestGenericTranslator(TimDbTest):
    def test_mixed(self):
        none_lang = Language(lang_code="", lang_name="", autonym="")
        translator = ReversingTranslationService()
        # TODO A more extensive test input might need to be used here
        parts = get_translate_approvals(
            "Here I am [**rocking**]{.red} [like](www.example.com) a ![hurricane](/imgs/hurricane.png)!"
        )

        # Sanity checks
        self.assertEqual(len(parts), 1)
        for x in parts[0]:
            self.assertIsInstance(x.text, str)

        self.assertEqual(
            [
                TR(" ma I ereH"),
                NT("[**"),
                TR("gnikcor"),
                NT("**]{.red}"),
                TR(" "),
                NT("["),
                TR("ekil"),
                NT("](www.example.com)"),
                TR(" a "),
                NT("!["),
                TR("enacirruh"),
                NT("](/imgs/hurricane.png)"),
                TR("!"),
            ],
            translator.translate(parts[0], none_lang, none_lang),
        )

    def test_all_translate(self):
        none_lang = Language(lang_code="", lang_name="", autonym="")
        translator = ReversingTranslationService()
        # TODO A more extensive test input might need to be used here
        parts = get_translate_approvals("Here I am")

        # Sanity checks
        self.assertEqual(len(parts), 1)
        for x in parts[0]:
            self.assertIsInstance(x, Translate)
            self.assertIsInstance(x.text, str)

        self.assertEqual(
            [TR("ma I ereH")],
            translator.translate(parts[0], none_lang, none_lang),
        )

    def test_all_notranslate(self):
        none_lang = Language(lang_code="", lang_name="", autonym="")
        translator = ReversingTranslationService()
        # TODO A more extensive test input might need to be used here
        parts = get_translate_approvals("[]()[]{}![]()")

        # Sanity checks
        self.assertEqual(len(parts), 1)
        for x in parts[0]:
            self.assertIsInstance(x, NoTranslate)
            self.assertIsInstance(x.text, str)

        self.assertEqual(
            # NOTE that {} as optional(?) get omitted by parsing
            [NT("[]()[]![]()")],
            translator.translate(parts[0], none_lang, none_lang),
        )

    def test_empty(self):
        none_lang = Language(lang_code="", lang_name="", autonym="")
        translator = ReversingTranslationService()
        self.assertEqual(
            [],
            translator.translate([], none_lang, none_lang),
        )
