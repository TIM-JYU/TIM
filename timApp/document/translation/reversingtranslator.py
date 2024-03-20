"""
Contains the implementation of ReversingTranslationService and its target
language, which are used in (NOTE:) unit-tests for translation routes.
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

from typing import Optional

import langcodes

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

REVERSE_LANG = {
    "lang_code": langcodes.standardize_tag("rev-erse"),
    "lang_name": "Reverse",
    "autonym": "esreveR",
}
"""Language that the ReversingTranslationService translates text into. To use
in tests.
"""


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
        max_workers: Optional[int] = None,
        max_retries: Optional[int] = None,
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
        :param target_lang: Only REVERSE_LANG["lang_code"] is supported.
        :param tag_handling: tags to intelligently handle during translation
         TODO XML-handling.
        :param max_workers: Maximum number of workers to use for sending translation requests
        :param max_retries: Maximum number of retries on a failed translation request
        :return: Texts where translatable ones have been reversed.
        """

        if target_lang.lang_code.lower() != REVERSE_LANG["lang_code"].lower():
            raise Exception(
                f"Bad target language. Expected '{REVERSE_LANG['lang_code']}', got '{target_lang}'."
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
        """
        :return: Mapping from all languages in database into the reversed language.
        """
        lang = Language.query_by_code(REVERSE_LANG["lang_code"])
        if lang is not None:
            return LanguagePairing(
                value={source.lang_code: [lang] for source in Language.query_all()}
            )
        raise Exception(
            f"Test-language is not found in database with the code '{REVERSE_LANG['lang_code']}'."
        )

    def supports_tag_handling(self, tag_type: str) -> bool:
        return False

    def supports(self, source_lang: Language, target_lang: Language) -> bool:
        """
        Check if language pairing is supported.

        :param source_lang: Language to translate from.
        :param target_lang: Only the REVERSE_LANG -language-code is supported.
        :return: True, if target_lang is rev-Erse.
        """
        return target_lang.lang_code.lower() == REVERSE_LANG["lang_code"].lower()

    def _translate(self, texts: list[str]) -> list[str]:
        """
        Reverses texts.
        Simulates the call to a machine translator.

        :param texts: List of translatable text.
        :return: List of translated text
        """
        return [x[::-1] for x in texts]

    def get_languages(self, source_langs: bool) -> list[Language]:
        """
        Reverse-language is supported as the only target language.

        :param source_langs: See documentation on TranslationService.
        :return: See documentation on TranslationService.
        """
        if source_langs:
            return Language.query_all()
        else:
            reverse_lang = Language.query_by_code(REVERSE_LANG["lang_code"])
            if reverse_lang is not None:
                return [reverse_lang]
            raise Exception(
                f"Test-language is not found in database with the code '{REVERSE_LANG['lang_code']}'."
            )

    __mapper_args__ = {"polymorphic_identity": "Reversing"}
