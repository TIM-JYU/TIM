import langcodes
import requests
import pypandoc

from dataclasses import dataclass
from typing import Dict, Callable
from timApp.timdb.sqa import db
from timApp.user.usergroup import UserGroup
from timApp.document.docparagraph import DocParagraph
from timApp.document.translation.language import Language
from timApp.document.translation.translationparser import (
    NoTranslate,
    TranslateApproval,
    get_translate_approvals,
    attr_collect,
    Table,
    Translate,
)
from timApp.util import logger
from timApp.util.flask.requesthelper import NotExist, RouteException


@dataclass
class Usage:
    character_count: int
    character_limit: int


# TODO make dataclass?
class LanguagePairing(Dict[str, list[Language]]):
    """
    Holds the list of supported source language codes mapping to target language codes
    """

    ...


class TranslationService(db.Model):
    """Represents the information that must be available from all possible machine translators."""

    __tablename__ = "translationservice"

    id = db.Column(db.Integer, primary_key=True)
    """Translation service identifier."""

    service_name = db.Column(db.Text, unique=True, nullable=False)
    """Human-readable name of the machine translator."""

    def translate(
        self,
        texts: list[TranslateApproval],
        src_lang: Language,
        target_lang: Language,
        *,
        tag_handling: str = "",
    ) -> list[TranslateApproval]:
        """
        The implementor should return the translated text in the same order as found in the `texts` parameter.
        :param texts: The texts marked for translation or not.
        :param src_lang: Language to from.
        :param target_lang: Language to translate into.
        :param tag_handling: TODO HACKy way to handle special case with (html) table-translations
        :return: List of the translated FIXME WARNING changing the values of object could be wrong and act funny on upper level. In other words: returning this should not be needed, as the transformations are done on the input list of objects (which are references(?))
        """

        raise NotImplementedError

    def usage(self) -> Usage:
        raise NotImplementedError

    def languages(self) -> LanguagePairing:
        raise NotImplementedError

    def supports(self, source_lang: Language, target_lang: Language) -> bool:
        raise NotImplementedError

    def supports_tag_handling(self, tag_type: str) -> bool:
        """
        Returns whether the TranslationService supports tag handling in translations
        for example with XML or HTML as input.
        TODO This is related to the kinda hacky way of handling Markdown-tables in translation

        :param tag_type: Type of the tag.
        :return: True, if tag type is supported.
        """
        raise NotImplementedError

    # Polymorphism allows querying multiple objects by their class e.g. TranslationService.query
    __mapper_args__ = {"polymorphic_on": service_name}


class TranslationServiceKey(db.Model):
    """Represents an API-key (or any string value) that is needed for using a machine translator and that one or more
    users are in possession of."""

    __tablename__ = "translationservicekey"

    id = db.Column(db.Integer, primary_key=True)
    """Key identifier."""

    # TODO better name?
    api_key = db.Column(db.Text, nullable=False)
    """The key needed for using related service."""

    group_id = db.Column(db.Integer, db.ForeignKey("usergroup.id"), nullable=False)
    group: UserGroup = db.relationship("UserGroup", uselist=False)
    """The group that can use this key."""

    service_id = db.Column(
        db.Integer,
        db.ForeignKey("translationservice.id"),
        nullable=False,
    )
    service: TranslationService = db.relationship("TranslationService", uselist=False)
    """The service that this key is used in."""

    @staticmethod
    def get_by_user_group(
        user_group: UserGroup | None,
    ) -> "TranslationServiceKey":
        return TranslationServiceKey.query.get(
            TranslationServiceKey.group_id == user_group
        )


class RegisteredTranslationService(TranslationService):
    """A translation service whose use is constrained by user group."""

    def supports(self, source_lang: Language, target_lang: Language) -> bool:
        raise NotImplementedError

    def supports_tag_handling(self, tag_type: str) -> bool:
        raise NotImplementedError

    def register(self, user_group: UserGroup) -> None:
        raise NotImplementedError

    def translate(
        self,
        texts: list[TranslateApproval],
        src_lang: Language,
        target_lang: Language,
        tag_handling: str = "xml",
    ) -> list[TranslateApproval]:
        raise NotImplementedError

    def usage(self) -> Usage:
        raise NotImplementedError

    def languages(self) -> LanguagePairing:
        raise NotImplementedError


class DeeplTranslationService(RegisteredTranslationService):
    service_url = db.Column(
        db.Text, default="https://api-free.deepl.com/v2", nullable=False
    )
    """The url base for the API calls (free version)."""

    ignore_tag = db.Column(db.Text, default="x", nullable=False)
    """The XML-tag name that is used for ignoring pieces of text."""

    headers: dict[str, str]

    source_Language_code: str
    """The source language's code (helps handling regional variants that DeepL doesn't differentiate)"""

    def register(self, user_group: UserGroup) -> None:
        """Set headers to use the user group's API-key ready for translation calls

        :param user_group: The user group whose API key will be used.
        """
        # One key should match one service per one user group TODO is that correct?
        api_key = TranslationServiceKey.query.filter(
            TranslationServiceKey.service_id == self.id,
            TranslationServiceKey.group_id == user_group.id,
        ).all()
        if len(api_key) == 0:
            raise NotExist(
                "Please add a DeepL API key that corresponds the chosen plan into your account"
            )
        if len(api_key) > 1:
            # TODO Does telling this information compromise security in any way?
            raise RouteException(
                "A user should not have more than one (1) API-key per service."
            )
        self.headers = {"Authorization": f"DeepL-Auth-Key {api_key[0].api_key}"}

    # TODO Change the dicts to DeepLTranslateParams and DeeplResponse or smth
    def _post(self, url_slug: str, data: dict | None = None) -> dict:
        """
        Perform a authorized post-request to the DeepL-API
        :param url_slug: The last part of URL-path for the API function _without_ the starting '/' slash
        :param data: Data to be transmitted along the request
        :return: The response's JSON TODO Is dict allowed?
        """
        resp = requests.post(
            self.service_url + "/" + url_slug, data=data, headers=self.headers
        )

        if resp.ok:
            try:
                return resp.json()
            except requests.exceptions.JSONDecodeError as e:
                raise Exception(f"DeepL API returned malformed JSON: {e}")
        else:
            status_code = resp.status_code

            # Handle the status codes given by DeepL API
            # Using Python 3.10's match-statement would be cool here but Black did not support it

            if status_code == 400:
                debug_exception = Exception(
                    f"The request to the DeepL API was bad. Please check your parameters."
                )
            elif status_code == 403:
                debug_exception = Exception(
                    f"Authorization failed. Please check your DeepL API key for typos."
                )
            elif status_code == 404:
                debug_exception = Exception(
                    f"The requested translator could not be found. Please try again later."
                )
            elif status_code == 413:
                debug_exception = Exception(
                    f"The request size exceeds the API's limit. Please try again with a smaller document."
                )
            elif status_code == 414:
                debug_exception = Exception(
                    f"The request URL is too long. Please contact TIM support."
                )
            elif status_code == 429:
                debug_exception = Exception(
                    f"Too many requests were sent. Please wait and resend the request later."
                )
            elif status_code == 456:
                debug_exception = Exception(
                    f"You have exceeded your character quota. Please try again when your quota has reset."
                )
            elif status_code == 503:
                debug_exception = Exception(
                    f"Translator currently unavailable. Please try again later."
                )
            elif status_code == 529:
                debug_exception = Exception(
                    f"Too many requests were sent. Please wait and resend the request later."
                )
            elif status_code >= 500 & status_code < 600:
                debug_exception = Exception(
                    f"An internal error occurred on the DeepL server. Please try again."
                )
            else:
                debug_exception = Exception(
                    f"DeepL API / {url_slug} responded with {resp.status_code}"
                )

            raise RouteException(
                description="The request failed. Error message: " + str(debug_exception)
            )

    def _translate(
        self,
        text: list[str],
        source_lang: str | None,
        target_lang: str,
        *,
        split_sentences: str | None = None,
        preserve_formatting: str | None = None,
        tag_handling: str | None = None,
        non_splitting_tags: list[str] = [],
        splitting_tags: list[str] = [],
        ignore_tags: list[str] = [],
    ) -> dict:
        """
        Supports most of the parameters of a DeepL API translate call.
        See https://www.deepl.com/docs-api/translating-text/request/ for valid parameter values and more information.
        :param text: Text to translate that can contain XML
        :param source_lang: Language of the text
        :param target_lang: Language to translate the text into
        :param split_sentences: Is text split before translation
        :param preserve_formatting: Is formatting preserved during translation
        :param tag_handling: XML and HTML are currently supported
        :param non_splitting_tags: Tags that never split sentences (eg. for the tag "<x>" the parameter should be "x")
        :param splitting_tags: Tags that always split sentences
        :param ignore_tags: Tags to ignore when translating
        :return: The DeepL API response JSON
        """
        # TODO Limit the amount of `text` parameters according to DeepL spec (50 per request?)

        src_lang = source_lang

        if source_lang is not None and (
            source_lang.lower() == "en-gb" or source_lang.lower() == "en-us"
        ):
            src_lang = "en"

        data = {
            "text": text,
            "source_lang": src_lang,
            "target_lang": target_lang,
            "split_sentences": split_sentences,
            "preserve_formatting": preserve_formatting,
            "tag_handling": tag_handling,
            "non_splitting_tags": ",".join(non_splitting_tags),
            "splitting_tags": ",".join(splitting_tags),
            "ignore_tags": ",".join(ignore_tags),
        }

        return self._post("translate", data)

    # TODO Cache this
    def _languages(self, *, is_source: bool) -> dict:
        """
        Get languages supported by the API
        :param is_source: Flag to query for supported source-languages
        :return: Languages supported in translations by type (source or target)
        """
        return self._post(
            "languages", data={"type": "source" if is_source else "target"}
        )

    def preprocess(self, elem: TranslateApproval) -> None:
        """
        Protect the text inside element with XML-tags from mangling in translation.
        :param elem: The element to add XML-protection-tags to.
        """
        if type(elem) is NoTranslate:
            elem.text = f"<{self.ignore_tag}>{elem.text}</{self.ignore_tag}>"

    def postprocess(self, text: str) -> str:
        """
        Remove unnecessary protection tags from the text.
        :param text: The text to remove XML-protection-tags from.
        :return: Text without the previously added protecting XML-tags.
        """
        return text.replace(f"<{self.ignore_tag}>", "").replace(
            f"</{self.ignore_tag}>", ""
        )

    def translate(
        self,
        texts: list[TranslateApproval],
        source_lang: Language | None,
        target_lang: Language,
        tag_handling: str = "xml",
    ) -> list[TranslateApproval]:
        """
        Uses the DeepL API for translating text between languages
        :param texts: Text to be translated TODO This should probably be a list of lists (each sublist is a paragraph(?))
        :param source_lang: Language of input text. None value makes DeepL guess it from the text.
        :param target_lang: Language for target language
        :param tag_handling: See superclass comment
        :return: The input text translated into the target language
        """
        source_lang_code = source_lang.lang_code if source_lang else None

        # Get the translatable text of objects and add protection to them
        if tag_handling == "xml":
            for elem in texts:
                self.preprocess(elem)
        protected_texts = list(map(lambda x: x.text, texts))

        # Translate texts using XML-protection
        resp_json = self._translate(
            protected_texts,
            # Send uppercase, because it is used in DeepL documentation
            source_lang_code.upper(),
            target_lang.lang_code.upper(),
            split_sentences="1",  # "1" (for example) keeps original document's empty newlines
            # NOTE Preserve formatting=1 might remove punctuation
            preserve_formatting="0",  # "1" DeepL does not make guesses of the desired sentence
            tag_handling=tag_handling,
            ignore_tags=[self.ignore_tag],
        )

        # Insert the text-parts sent to the API into correct places in original elements
        for translated_text, element in zip(resp_json["translations"], texts):
            clean_text = (
                self.postprocess(translated_text["text"])
                if tag_handling == "xml"
                else translated_text["text"]
            )
            element.text = clean_text

        return texts

    def usage(self) -> Usage:
        resp_json = self._post("usage")
        return Usage(
            character_count=int(resp_json["character_count"]),
            character_limit=int(resp_json["character_limit"]),
        )

    def get_languages(self, source_langs: bool) -> list[Language]:
        """
        Fetches the source or target languages from DeepL.
        :param source_langs: Whether source languages must be fetched
        :return: The list of source of target languages from DeepL.
        """

        def get_langs_from_db(deepl_lang: dict) -> Language | None:
            try:
                language = deepl_lang["language"]
                code = langcodes.get(language).to_tag()

                # This is needed because DeepL's source languages only include English (EN) and not regional variants
                if code.lower() == "en":
                    code = self.source_Language_code
                return Language.query_by_code(code)
            except LookupError:
                return None

        self.source_Language_code = "en-GB"
        langs = self._languages(is_source=source_langs)
        return_langs = list(filter(None, map(get_langs_from_db, langs)))
        if source_langs:
            self.source_Language_code = "en-US"
            en = Language(
                flag_uri="",
                lang_code="",
                lang_name="",
                autonym="",
            )
            for lang in langs:
                if lang.get("language").lower() == "en":
                    en = get_langs_from_db(lang)
            if en is not None:
                return_langs = return_langs + [en]
        return return_langs

    # TODO Cache this maybe?
    def languages(self) -> LanguagePairing:
        """
        Asks the DeepL API for the list of supported languages (Note: the supported language _pairings_ are not explicitly specified) and turns the returned language codes to Languages found in the database.
        :return: Dictionary of source langs to lists of target langs, that are supported by the API and also found in database.
        """

        def get_lang(deepl_lang: dict) -> Language | None:
            try:
                language = deepl_lang["language"]
                code = langcodes.get(language).to_tag()

                # This is needed because DeepL's source languages only include English (EN) and not regional variants
                if code.lower() == "en":
                    code = self.source_Language_code
                return Language.query_by_code(code)
            except LookupError:
                return None

        # Query API for supported source and target languages and transform them into suitable format
        resp_json_src = self._languages(is_source=True)
        resp_json_target = self._languages(is_source=False)
        db_langs_src: list[Language] = list(filter(None, map(get_lang, resp_json_src)))
        db_langs_target: list[Language] = list(
            filter(None, map(get_lang, resp_json_target))
        )
        langs_map = {lang.lang_code: db_langs_target for lang in db_langs_src}
        return LanguagePairing(langs_map)

    def supports(self, source_lang: Language, target_lang: Language) -> bool:
        """
        Check that the source language can be translated into target language by the translation API
        :param source_lang: Language of original text
        :param target_lang: Language to translate into
        :return: True, if the pairing is supported
        """

        self.source_Language_code = source_lang.lang_code

        try:
            supported_languages: list[Language] = self.languages()[
                self.source_Language_code
            ]
        except KeyError as e:
            raise RouteException(
                f"The language code {e} was not found in supported source languages."
            )

        # The target language is found by the primary key
        # TODO is this too much? Can't strings be just as good?
        #  Maybe better would be to handle Languages by their database id's?
        return any(x.lang_code == target_lang.lang_code for x in supported_languages)

    def supports_tag_handling(self, tag_type: str) -> bool:
        return tag_type in ["xml", "html"]

    # TODO Make the value an enum like with Verification?
    __mapper_args__ = {"polymorphic_identity": "DeepL Free"}


class DeeplProTranslationService(DeeplTranslationService):
    # TODO Make the value an enum like with Verification?
    __mapper_args__ = {"polymorphic_identity": "DeepL Pro"}


def init_translate(
    translator: TranslationService,
    source_lang: Language,
    target_lang: Language,
) -> Callable[[list[str]], list[str]]:
    # TODO This helper-function would be better if there was some way to hide the
    #  translate-methods on ITranslators. Maybe save for the eventual(?) TranslatorSelector?
    """
    Use the specified TranslationService to initialize machine translation
    :param translator: The translator to use
    :param source_lang: The language that text is in
    :param target_lang: The language that text will be translated into
    :return: A partially applied function for translating text with the specified languages using the specified TranslationService
    """


# TODO Remove this when crisis is over
class DeeplPlaceholderTranslationService(DeeplTranslationService):

    # TODO Make the value an enum like with Verification?
    __mapper_args__ = {"polymorphic_identity": "DeepL"}


@dataclass
class TranslationTarget:
    value: str | DocParagraph
    """
    Type that can be passed around in translations.
    """

    def get_text(self) -> str:
        if isinstance(self.value, str):
            return self.value
        elif isinstance(self.value, DocParagraph):
            return self.value.md
        else:
            assert False, "Translatable paragraph had unexpected type"


class TranslateMethodFactory:
    @staticmethod
    def create(
        translator_code: str, s_lang: str, t_lang: str, user_group: UserGroup | None
    ) -> Callable[[list[TranslationTarget]], list[str]]:
        """
        Based on a name, get the correct TranslationService from database and perform needed initializations on it.

        :param translator_code: Name that separates the different TranslationServices.
        :param s_lang: Source language of translatable text.
        :param t_lang: Target language to translate text into.
        :param user_group: Identification of user, that can be allowed to use some TranslationServices (for example
        DeepL requires an API-key)
        :return: Function that when called, uses the selected parameters in translating text.
        """

        # TODO Find out if this is used correctly. Would rather use Query.get, but did not work...
        translator = TranslationService.query.with_polymorphic("*").filter(
            TranslationService.service_name == translator_code
        )[0]

        if user_group is not None and isinstance(
            translator, RegisteredTranslationService
        ):
            translator.register(user_group)

        source_lang = Language.query_by_code(s_lang)
        target_lang = Language.query_by_code(t_lang)

        if not translator.supports(source_lang, target_lang):
            raise RouteException(
                description=f"The language pair from {source_lang} to {target_lang} is not supported with {translator.service_name}"
            )

        # TODO This helper-function would be better if there was some way to hide the translate-methods on ITranslators. Maybe save for the eventual(?) TranslatorSelector?
        """
        Use the specified TranslationService to initialize machine translation
        :param translator: The translator to use
        :param source_lang: The language that text is in
        :param target_lang: The language that text will be translated into
        :return: A partially applied function for translating text with the specified languages using the specified TranslationService
        """

        def translate_raw_text(md: str) -> str:
            """
            Most primitive translate-func -version to translate text between languages.

            :param md: The text to translate.
            :return: The translated text.
            """
            # Turn the text into lists of objects that describe whether they can be translated or not
            elements: list[list[TranslateApproval]] = get_translate_approvals(md)
            # Pass object-lists with translatable text to the machine translator object.
            # If supported, the translator protects and removes the protection from the text (for example adding XML-ignore-tags in DeepL's case).
            translated_elements: list[list[TranslateApproval]] = list()

            for xs in elements:
                # Pick the table out for special translation and handle the rest normally
                tr_sublist: list[TranslateApproval] = list()
                for x in xs:
                    if isinstance(x, Table):
                        if translator.supports_tag_handling("html"):
                            # Special (HACKY) case, where md-tables are translated as html (if supported)
                            # TODO Actually implement table_collect at translationparser.py so that
                            #  non-html-handling translators can be used as well
                            # Turn the markdown into html
                            table_html: str = pypandoc.convert_text(
                                x.text, to="html", format="md"
                            )
                            # Translate as HTML
                            table_html_tr = translator.translate(
                                [Translate(table_html)],
                                source_lang,
                                target_lang,
                                tag_handling="html",
                            )[0]
                            # Turn the html back into md
                            table_md_tr = pypandoc.convert_text(
                                table_html_tr.text, to="md", format="html"
                            )
                            # Now mark the table as NoTranslate, so it doesn't get translated when
                            # the list is passed on to mass-translation
                            tr_sublist.append(NoTranslate(table_md_tr))
                        else:
                            # The table cannot be translated and is handled as is
                            tr_sublist.append(NoTranslate(x.text))
                    else:
                        tr_sublist.append(x)
                translated_elements.append(
                    translator.translate(tr_sublist, source_lang, target_lang)
                )
            # The translator object returns the same structure as input but their content has been translated accordingly
            # Transform the objects back to a Markdown string
            translated_md = ""
            for paragraph in translated_elements:
                for elem in paragraph:
                    translated_md += elem.text
                # TODO what are the paragraphs separated by? "\n\n"? Seems like this would need more handling in regard
                #  to TIM's block separation and id's etc
                # TODO Do some MD-elements (from parser) not include newline postfix and should this newline-addition
                #  then be placed into parser-module?
                translated_md += "\n\n"
            translated_md = translated_md.strip()

            return translated_md

        def translate_paragraph(target: TranslationTarget) -> str:
            """
            Translate a single piece of Markdown roughly the size of a generic paragraph.
            :param target: The object whose Markdown-text-value to parse.
            :return: The markdown elements that are contained in the text.
            """

            md = target.get_text()

            if isinstance(target.value, DocParagraph) and target.value.is_plugin():
                # Add the attributes to the content so that parser can identify the code block as a plugin
                # NOTE that the parser should only use the attributes for identification and deletes them from the translated result ie. this is a special case!

                # Form the Pandoc-AST representation of a code-block's Attr and glue the parts returned as is back together into a string of Markdown
                # TODO Is par.attrs trusted to not be None?
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
                        attr_collect([taskid, classes, kv_pairs])[0],
                    )
                )
                md = md.replace("```\n", f"``` {attr_str}\n", 1)

            return translate_raw_text(md)

        def generic_translate(paras: list[TranslationTarget]) -> list[str]:
            """
            Wraps the TranslationService, source and target languages into a function that can be used to call a translation on different TranslationService-instances.
            :param paras: TIM-paragraphs containing Markdown to translate.
            :return: The translatable text contained in input paragraphs translated according to the outer functions inputs (the languages).
            """
            # TODO Translator should be able to translate multiple texts at once (ie. DeepL request can have 50 text-params)
            translated_texts = [translate_paragraph(x) for x in paras]

            # TODO Maybe log the length of text or other shorter info?
            logger.log_info("\n".join(translated_texts))

            usage = translator.usage()
            logger.log_info(
                "Current DeepL API usage: "
                + str(usage.character_count)
                + "/"
                + str(usage.character_limit)
            )

            return translated_texts

        return generic_translate


def get_lang_lists(translator: str, source_langs: bool) -> list[Language]:
    if translator.lower() == "deepl free":
        tr = DeeplTranslationService.query.first()
    elif translator.lower() == "deepl pro":
        tr = DeeplProTranslationService.query.first()

    return tr.get_languages(source_langs)
