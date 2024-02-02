"""
This module contains most notably the TranslationService-interface that
different machine translators must implement in order to be integrated into
TIM's machine translation feature.

Other notable things include a database model for the API-keys of machine
translator services and a processor/wrapper by which the different
translators can be used to translate text from one language to another.
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

from dataclasses import dataclass
from typing import Optional

import pypandoc
from sqlalchemy import select, ForeignKey
from sqlalchemy.orm import with_polymorphic, mapped_column, Mapped, relationship

from timApp.document.docparagraph import DocParagraph
from timApp.document.translation.language import Language
from timApp.document.translation.translationparser import (
    NoTranslate,
    TranslateApproval,
    TranslationParser,
    Table,
    Translate,
)
from timApp.timdb.sqa import run_sql, db
from timApp.user.usergroup import UserGroup
from timApp.util import logger
from timApp.util.flask.requesthelper import RouteException

TranslateBlock = list[TranslateApproval]
"""Typedef to represent logically connected parts of non- and translatable text.
"""


@dataclass
class Usage:
    """Contains information about the usage of a translator service."""

    character_count: int
    character_limit: int


@dataclass
class LanguagePairing:
    """Maps standardized codes of (source) Languages to lists of (target)
    Language objects.
    """

    value: dict[str, list[Language]]

    def __getitem__(self, item: str) -> list[Language]:
        """
        Implement the indexing operator [] on LanguagePairing.

        :param item: The key to index with.
        :return: The value corresponding to item.
        """
        return self.value[item]


class TranslationService(db.Model):
    """Represents the information and methods that must be available from all
    possible machine translators.
    """

    id: Mapped[int] = mapped_column(primary_key=True)
    """Translation service identifier."""

    service_name: Mapped[str] = mapped_column(unique=True)
    """Human-readable name of the machine translator. Also used as an
    identifier."""

    def translate(
        self,
        texts: list[TranslateBlock],
        source_lang: Language,
        target_lang: Language,
        *,
        tag_handling: str = "",
    ) -> list[str]:
        """
        Translate texts from source to target language.

        The implementor of this method should return the (translated) text in
        the same order as found in the input `texts`-parameter originally.

        :param texts: The texts marked for translation or not. A convention
         would be to pass as much of the translatable text as possible in this
         parameter in order to minimize the amount of separate
         translation-calls.
        :param source_lang: Language to translate from.
        :param target_lang: Language to translate into.
        :param tag_handling: Tag representing a way to separate or otherwise
         control translated text with the translation service. A HACKY way to
         handle special case with translating (html) tables.
        :return: List of strings found inside the items of `texts`-parameter,
         in the same order and translated.
        """

        raise NotImplementedError

    def usage(self) -> Usage:
        """
        Get the service's usage status.

        :return: The current usage of this TranslationService (for example
         status of an API-key).
        """
        raise NotImplementedError

    def languages(self) -> LanguagePairing:
        """
        Get the language-combinations for translations supported with the
        service.

        :return: The supported mapping of languages to translate to and from
         with this TranslationService.
        """
        raise NotImplementedError

    def supports(self, source_lang: Language, target_lang: Language) -> bool:
        """
        Check if the service supports a language-combination.

        :param source_lang: Language to translate from.
        :param target_lang: Language to translate into.
        :return: True, if the service can translate from `source_lang` to
         `target_lang`.
        """
        raise NotImplementedError

    def supports_tag_handling(self, tag_type: str) -> bool:
        """
        Check if the service supports tag handling in translations. For example
        using XML-tags, some services offer controlling parts of the text, that
        should be kept as-is and not be affected by the machine translation:
        "My name is Dr. <protect>Oak</protect>."

        NOTE this is related to the kinda HACKY way of handling Markdown-tables
        in DeepL-translation.

        :param tag_type: Type of the tag. Some services for example support
         "xml" or "html".
        :return: True, if the tag type is supported.
        """
        raise NotImplementedError

    def get_languages(self, source_langs: bool) -> list[Language]:
        """
        Return languages supported by the TranslationService.

        :param source_langs: Whether source languages must be returned.
        :return: The list of supported source or target languages.
        """
        raise NotImplementedError

    def ensure_compatible_source_lang_variant(self, lang_code: str) -> str:
        """
        Return compatible source language code for this translation service.
        This is a workaround for translation services that do not accept certain
        language variants as the source (such as en-GB, en-US, etc.).
        :param lang_code: Language code to check and possibly modify
        :return: compatible language code
        """
        # TODO fetch up-to-date supported source languages from DeepL API, and
        #      do the conversion on the fly instead of hard-coded values
        if "DeepL" in self.service_name:
            # Variants for English
            if lang_code in ["en-GB", "en-US"]:
                return "en"
            # Variants for Portuguese
            if lang_code in ["pt-BR", "pt-PT"]:
                return "pt"

    # Polymorphism allows querying multiple objects by their class e.g.
    # `TranslationService.query`.
    __mapper_args__ = {"polymorphic_on": "service_name"}


class TranslationServiceKey(db.Model):
    """Represents an API-key (or any string value) that is needed for using a
    machine translator and that one or more users are in possession of.
    """

    id: Mapped[int] = mapped_column(primary_key=True)
    """Key identifier."""

    # TODO Come up with a better name?
    api_key: Mapped[str]
    """The key needed for using related service."""

    group_id: Mapped[int] = mapped_column(ForeignKey("usergroup.id"))
    group: Mapped[UserGroup] = relationship()
    """The group that can use this key."""

    service_id: Mapped[int] = mapped_column(ForeignKey("translationservice.id"))
    service: Mapped[TranslationService] = relationship()
    """The service that this key is used in."""

    @staticmethod
    def get_by_user_group(
        user_group: UserGroup | None,
    ) -> Optional["TranslationServiceKey"]:
        """
        Query a key based on a group that could have access to it.

        :param user_group: The group that wants to use a key.
        :return: The first matching TranslationServiceKey instance, if one is
         found.
        """
        return (
            run_sql(
                select(TranslationServiceKey)
                .filter(TranslationServiceKey.group_id == user_group)
                .limit(1)
            )
            .scalars()
            .first()
        )

    def to_json(self) -> dict:
        """
        Create a JSON representation of data related to the
        TranslationServiceKey instance.

        :return: The TranslationServiceKey instance's needed fields in a dict.
        """
        return {
            "translator": self.service.service_name,
            "APIkey": self.api_key,
        }


# PyCharm would otherwise want this class to implement the superclass methods.
# noinspection PyAbstractClass
class RegisteredTranslationService(TranslationService):
    """A translation service whose use is constrained by user group."""

    __abstract__ = True

    def register(self, user_group: UserGroup) -> None:
        """
        Set some state to the service object based on user group.

        :param user_group: The somehow related user group.
        :return: None.
        """
        raise NotImplementedError


@dataclass
class TranslationTarget:
    """Type that can be passed around in translations."""

    value: str | DocParagraph

    def get_text(self) -> str:
        if isinstance(self.value, str):
            return self.value
        elif isinstance(self.value, DocParagraph):
            return self.value.md
        else:
            raise Exception("Translation target had unexpected type")


class TranslateProcessor:
    def __init__(
        self,
        translator_code: str,
        s_lang: str,
        t_lang: str,
        user_group: UserGroup | None,
    ):
        """
        Based on a name, get the correct TranslationService from database and
        perform needed initializations on it.

        :param translator_code: Name that identifies the
         TranslationService being used.
        :param s_lang: Source language of translatable text.
        :param t_lang: Target language to translate text into.
        :param user_group: Identification of user, that can be allowed to use
         some TranslationServices (for example DeepL requires an API-key that
         the user sets to their account).
        """

        translator = (
            run_sql(
                select(with_polymorphic(TranslationService, "*")).filter(
                    TranslationService.service_name == translator_code
                )
            )
            .scalars()
            .one()
        )

        if user_group is not None and isinstance(
            translator, RegisteredTranslationService
        ):
            translator.register(user_group)

        source_language = translator.ensure_compatible_source_lang_variant(s_lang)
        source_lang_ = Language.query_by_code(source_language)
        target_lang_ = Language.query_by_code(t_lang)

        if not translator.supports(source_lang_, target_lang_):
            raise RouteException(
                description=f"The language pair from {source_lang_} to {target_lang_} is not supported with {translator.service_name}"
            )

        self.translator = translator
        self.parser = TranslationParser()
        self.source_lang = source_lang_
        self.target_lang = target_lang_

    def _translate_raw_texts(self, mds: list[str]) -> list[str]:
        """
        Most primitive of the translate-methods to translate texts between
        languages.

        :param mds: The texts to translate.
        :return: The translated texts in same order as input.
        """
        # Turn the text into lists of objects that describe whether they
        # can be translated or not.
        # TODO The flattening (calling `chain.from_iterable`) could
        #  probably be done in parser
        blocks: list[list[TranslateApproval]] = list(
            map(lambda x: self.parser.get_translate_approvals(x), mds)
        )

        # Map over blocks, picking the tables out for special translation
        # and handle the rest normally.
        for block in blocks:
            for i in range(len(block)):
                elem = block[i]
                if isinstance(elem, Table):
                    if self.translator.supports_tag_handling("html"):
                        # Special (HACKY) case, where md-tables are
                        # translated as html (if supported).
                        # TODO Actually implement table_collect at
                        #  translationparser.py so that non-html-handling
                        #  translators can be used as well
                        # Turn the markdown into html.
                        table_html: str = pypandoc.convert_text(
                            elem.text, to="html", format="md"
                        )
                        # Translate as HTML. NOTE Requires translator to
                        # support tag handling in HTML.
                        # TODO All document's tables could potentially be
                        #  send to translator at once instead of one by
                        #  one as done here.
                        table_html_tr = self.translator.translate(
                            [[Translate(table_html)]],
                            self.source_lang,
                            self.target_lang,
                            tag_handling="html",
                        )
                        # Turn the html back into md.
                        table_md_tr = pypandoc.convert_text(
                            table_html_tr[0], to="md", format="html"
                        )
                        # Now mark the table as NoTranslate, so it doesn't
                        # get translated when the list is passed on to
                        # mass-translation.
                        # TODO Adding this newline is kinda HACKY and not
                        #  thought out.
                        block[i] = NoTranslate("\n" + table_md_tr)
                    else:
                        # The table cannot be translated and is handled as
                        # is.
                        block[i] = NoTranslate(elem.text)

        # Pass object-lists with translatable text to the machine
        # translator object.
        # If supported, the translator protects and removes the protection
        # from the text (for example adding XML-ignore-tags in DeepL's
        # case).
        translated_mds = self.translator.translate(
            blocks, self.source_lang, self.target_lang
        )
        # TODO what are the paragraphs separated by? "\n\n"? Seems like
        #  this would need more handling in regard to TIM's block
        #  separation and id's etc.
        # TODO Do some MD-elements (from parser) not include newline
        #  postfix and should this newline-addition then be placed into
        #  parser-module?
        return translated_mds

    def _translate_paragraphs(self, targets: list[TranslationTarget]) -> list[str]:
        """
        Translate pieces of Markdown roughly the size of a generic
        paragraph.

        :param targets: The list of objects whose Markdown-text-value to
         parse.
        :return: List containing the translated pieces of markdown in same
         order as input.
        """

        mds = []

        for target in targets:
            md = target.get_text()
            if isinstance(target.value, DocParagraph) and target.value.is_plugin():
                # Add the attributes to the content so that parser can
                # identify the code block as a plugin.
                # NOTE that the parser should only use the attributes for
                # identification and deletes them from the translated
                # result ie. this is a special case!

                # Form the Pandoc abstract syntax tree -representation of a
                # code-block's Attr and glue the parts returned as is back
                # together into a string of Markdown.
                taskid = (
                    target.value.attrs.get("taskId", "") if target.value.attrs else ""
                )
                classes: list[str] = (
                    x
                    if target.value.attrs
                    and isinstance(x := target.value.attrs.get("classes"), list)
                    else []
                )
                kv_pairs = (
                    [(k, v) for k, v in target.value.attrs.items() if k != "taskId"]
                    if target.value.attrs
                    else []
                )
                attr_str = "".join(
                    map(
                        lambda y: y.text,
                        self.parser.attr_collect([taskid, classes, kv_pairs])[0],
                    )
                )
                md = md.replace("```\n", f"``` {attr_str}\n", 1)

            mds.append(md)

        try:
            return self._translate_raw_texts(mds)
        except Exception as e:
            raise RouteException("Automatic translation failed: " + str(e))

    def translate(self, pars: list[TranslationTarget]) -> list[str]:
        """
        Translate a list of text-containing items using the
        TranslationService-instance and languages set at initialization.

        :param pars: TIM-paragraphs containing Markdown to translate.
        :return: The translatable text contained in input paragraphs
         translated according to the processor-state (languages and the
         translator).
        """
        translated_texts = self._translate_paragraphs(pars)

        for i, part in enumerate(translated_texts):
            logger.log_debug(
                f"==== Part {i} ({len(part)} characters): ================================"
                f"{part}"
                "================================================"
            )

        usage = self.translator.usage()
        logger.log_debug(
            "Current usage: "
            + str(usage.character_count)
            + "/"
            + str(usage.character_limit)
        )

        return translated_texts


def replace_md_aliases(text: str) -> str:
    """
    Replace the aliases that are used in place of Markdown-syntax-characters.

    On some machine translators (tested with DeepL) the Markdown syntax
    characters break easier compared to their HTML-style counterparts. This is
    baked into the translation-parser, but must be converted back to
    Markdown-style in order to follow TIM's preferences.
    :param text: Text to replace the HTML-tags of.
    :return: Text with the HTML-tags replaced.
    """
    # TODO Map these replacements somehow from translationparser.py instead of
    #  hard-coding here (and there).
    return (
        text.replace("<i>", "*")
        .replace("</i>", "*")
        .replace("<b>", "**")
        .replace("</b>", "**")
    )
