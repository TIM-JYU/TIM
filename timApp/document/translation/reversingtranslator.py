"""
Contains the implementation of ReversingTranslationService, which is used
in (NOTE:) unit-tests for translation routes.
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
    TranslateApproval,
    Translate,
)
from timApp.document.translation.translator import (
    TranslationService,
    LanguagePairing,
    Usage,
)


class ReversingTranslationService(TranslationService):
    """Translator to test if the list[list[TranslateApproval]]-structure is
    generic enough to (easily) use for integrating new machine translators
    into TIM.
    """

    def translate(
        self,
        texts: list[list[TranslateApproval]],
        src_lang: Language,
        target_lang: Language,
        *,
        tag_handling: str = "",
    ) -> list[str]:
        """
        Reverse the translatable text given.
        NOTE The algorithm here for combining translation results back to
        original structure might be integrated into the actual
        TranslationService-implementation.
        Note This implementation does not fully follow the needed interface.

        :return:
        :param texts: Texts to reverse
        :param src_lang: Any.
        :param target_lang: Only rev-Erse is supported.
        :param tag_handling: tags to intelligently handle during translation
         TODO XML-handling.
        :return: Texts where translatable ones have been reversed.
        """

        if target_lang.lang_code.lower() != "rev-erse":
            raise Exception(
                f"Bad target language. Expected rev-Erse, got {target_lang}."
            )

        # Iterate the blocks of texts
        translation_subjects: list[list[str]] = list()
        for xs in texts:
            # Leave non-translatable values out of the call to machine
            # translation and pick their string-valued texts to a list.
            just_translates: list[str] = list(
                map(
                    lambda y: y.text,
                    filter(lambda x: isinstance(x, Translate), xs),
                )
            )
            translation_subjects.append(just_translates)

        # Perform the translation on all or maximum amount of blocks at a time.
        for i, ys in enumerate(translation_subjects):
            translation_subjects[i] = self._translate(ys)

        # Insert translated parts back into their original objects.
        # NOTE This only works, if self._translate returns the values in the
        # same order as it got them!!
        for text, translated_text in zip(texts, translation_subjects):
            i = 0
            for part in text:
                if isinstance(part, Translate):
                    part.text = translated_text[i]
                    i += 1

        # Concatenate the strings inside texts' list of items together.
        return ["".join(map(lambda x: x.text, xs)) for xs in texts]

    def usage(self) -> Usage:
        """Infinite quota"""
        import sys

        return Usage(character_count=0, character_limit=sys.maxsize)

    def languages(self) -> LanguagePairing:
        # TODO
        raise NotImplementedError

    def supports(self, source_lang: Language, target_lang: Language) -> bool:
        """
        Check if language pairing is supported.

        :param source_lang: Language to translate from.
        :param target_lang: Only the rev-Erse -language code is supported.
        :return: True, if target_lang is rev-Erse.
        """
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
