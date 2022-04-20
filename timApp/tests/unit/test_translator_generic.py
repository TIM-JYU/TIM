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
        texts: list[list[TranslateApproval]],
        src_lang: Language,
        target_lang: Language,
    ) -> list[str]:
        """
        Reverse the translatable text given.
        NOTE The algorithm here for combining translation results back to original structure might be integrated into
         the actual TranslationService-implementation.

        :param texts: Texts to reverse
        :param src_lang: Unused
        :param target_lang: Unused
        :return: Texts where translatable ones have been reversed.
        """

        # Iterate the blocks of texts
        translation_subjects: list[list[str]] = list()
        for xs in texts:
            # Leave non-translatable values out of the call to machine translation
            # and pick their string-valued texts to a list
            just_translates = list(
                map(
                    lambda x: x.text,
                    filter(lambda x: isinstance(x, Translate), xs),
                )
            )
            translation_subjects.append(just_translates)

        # Perform the translation on all or maximum amount of blocks at a time
        for i, xs in enumerate(translation_subjects):
            translation_subjects[i] = self._translate(xs)

        # Insert translated parts back into their original objects
        # NOTE This only works, if self._translate returns the values in the same order as it got them!!
        for text, translated_text in zip(texts, translation_subjects):
            i = 0
            for part in text:
                if isinstance(part, Translate):
                    part.text = translated_text[i]
                    i += 1

        # Concatenate the strings inside texts' list of items together
        return ["".join(map(lambda x: x.text, xs)) for xs in texts]

    def usage(self) -> Usage:
        """Infinite quota"""
        import sys

        return Usage(character_count=0, character_limit=sys.maxsize)

    def languages(self) -> LanguagePairing:
        raise NotImplementedError

    def supports(self, source_lang: Language, target_lang: Language) -> bool:
        return target_lang.lang_code.lower() == "rev-erse"

    def _translate(_self, texts: list[str]) -> list[str]:
        """
        Reverses texts.
        Simulates the call to a machine translator.
        :param texts: List of translatable text.
        :return: List of translated text
        """
        return [x[::-1] for x in texts]

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
