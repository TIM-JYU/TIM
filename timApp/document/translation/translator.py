import langcodes
import requests

from dataclasses import dataclass
from typing import Dict, Callable, Any
from timApp.timdb.sqa import db
from timApp.user.usergroup import UserGroup
from timApp.document.docparagraph import DocParagraph
from timApp.document.translation.language import Language
from timApp.document.translation.translationparser import (
    NoTranslate,
    TranslateApproval,
    get_translate_approvals,
    attr_collect,
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
        self, texts: list[TranslateApproval], src_lang: Language, target_lang: Language
    ) -> list[TranslateApproval]:
        """
        The implementor should return the translated text in the same order as found in the `texts` parameter.
        :param texts: The texts marked for translation or not.
        :param src_lang: Language to from.
        :param target_lang: Language to translate into.
        :return: List of the translated FIXME WARNING changing the values of object could be wrong and act funny on upper level. In other words: returning this should not be needed, as the transformations are done on the input list of objects (which are references(?))
        """

        raise NotImplementedError

    def usage(self) -> Usage:
        raise NotImplementedError

    def languages(self) -> LanguagePairing:
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


class DeeplTranslationService(TranslationService):
    service_url = db.Column(
        db.Text, default="https://api-free.deepl.com/v2", nullable=False
    )
    """The url base for the API calls (free version)."""

    ignore_tag = db.Column(db.Text, default="x", nullable=False)
    """The XML-tag name that is used for ignoring pieces of text."""

    headers: dict[str, str]

    source_Language_code: str
    """The source language's code (helps handling regional variants that DeepL doesn't differentiate)"""

    # TODO Register by API-key; Translator should not care about users
    def register(self, user_group: UserGroup) -> None:
        """Set headers to use the user group's API-key ready for translation calls
        :param user_group: The user group whose API key will be used
        """
        # One key should match one service per one user group TODO is that correct?
        api_key = TranslationServiceKey.query.filter(
            TranslationServiceKey.service_id == self.id,
            TranslationServiceKey.group_id == user_group.id,
        ).first()
        if api_key is None:
            raise NotExist("Please add a DeepL API key into your account")
        self.headers = {"Authorization": f"DeepL-Auth-Key {api_key.api_key}"}

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
                description="Automatic translation failed. Error message: "
                + str(debug_exception)
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

        if source_lang.lower() == "en-gb" or source_lang.lower() == "en-us":
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
    ) -> list[TranslateApproval]:
        """
        Uses the DeepL API for translating text between languages
        :param texts: Text to be translated TODO This should probably be a list of lists (each sublist is a paragraph(?))
        :param source_lang: Language of input text. None value makes DeepL guess it from the text.
        :param target_lang: Language for target language
        :return: The input text translated into the target language
        """
        source_lang_code = source_lang.lang_code if source_lang else None

        # Get the translatable text of objects and add protection to them
        for elem in texts:
            self.preprocess(elem)
        protected_texts = list(map(lambda x: x.text, texts))

        # Translate texts using XML-protection
        resp_json = self._translate(
            protected_texts,
            # Send uppercase, because it is used in DeepL documentation
            source_lang_code.upper(),
            target_lang.lang_code.upper(),
            # TODO keep original formatting especially related to empty space and newlines (for example translation still breaks md lists)
            split_sentences="1",  # "1" (for example) keeps original document's empty newlines
            # TODO Preserve formatting=1 might remove punctuation
            preserve_formatting="0",  # "1" DeepL does not make guesses of the desired sentence
            tag_handling="xml",
            ignore_tags=[self.ignore_tag],
        )

        # Insert the text-parts sent to the API into correct places in original elements
        for translated_text, element in zip(resp_json["translations"], texts):
            clean_text = self.postprocess(translated_text["text"])
            element.text = clean_text

        return texts

    def usage(self) -> Usage:
        resp_json = self._post("usage")
        return Usage(
            character_count=int(resp_json["character_count"]),
            character_limit=int(resp_json["character_limit"]),
        )

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
        :param source_lang: Language of original text TODO In what format?
        :param target_lang: Language to translate into TODO In what format?
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
        # TODO is this too much? Can't strings be just as good? Maybe better would be to handle Languages by their database id's?
        return any(x.lang_code == target_lang.lang_code for x in supported_languages)

    # TODO Make the value an enum like with Verification?
    __mapper_args__ = {"polymorphic_identity": "DeepL Free"}


class DeeplProTranslationService(DeeplTranslationService):

    # TODO Make the value an enum like with Verification?
    __mapper_args__ = {"polymorphic_identity": "DeepL Pro"}


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

    def is_plugin(self) -> bool:
        if isinstance(self.value, DocParagraph):
            return self.value.is_plugin()
        else:
            return False


def init_translate(
    translator: TranslationService,
    source_lang: Language,
    target_lang: Language,
) -> Callable[[list[TranslationTarget]], list[str]]:
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
        translated_elements: list[list[TranslateApproval]] = [
            translator.translate(xs, source_lang, target_lang) for xs in elements
        ]
        # The translator object returns the same structure as input but their content has been translated accordingly
        # Transform the objects back to a Markdown string
        translated_md = ""
        for paragraph in translated_elements:
            for elem in paragraph:
                translated_md += elem.text
            # TODO what are the paragraphs separated by? "\n\n"? Seems like this would need more handling in regard to
            #  TIM's block separation and id's etc
            # TODO Do some MD-elements (from parser) not include newline postfix and should this newline-addition then
            #  be placed into parser-module?
            translated_md += "\n"
        translated_md = translated_md.strip()

        return translated_md

    def translate_paragraph(target: TranslationTarget) -> str:
        """
        Translate a single piece of Markdown roughly the size of a generic paragraph.
        :param target: The object whose Markdown-text-value to parse.
        :return: The markdown elements that are contained in the text.
        """

        md = target.get_text()
        is_plugin = target.is_plugin()

        if is_plugin:
            # Add the attributes to the content so that parser can identify the code block as a plugin
            # NOTE that the parser should only use the attributes for identification and deletes them from the translated result ie. this is a special case!

            # Form the Pandoc-AST representation of a code-block's Attr and glue the parts returned as is back together into a string of Markdown
            # TODO Is par.attrs trusted to not be None?
            taskid = target.value.attrs.get("taskId", "") if target.value.attrs else ""
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
                # TODO Making the list into dict like this to appease mypy looks bad
                map(
                    lambda x: x.text,
                    attr_collect({0: taskid, 1: classes, 2: kv_pairs})[0],
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


def init_deepl_translate(
    user_group: UserGroup,
    source_lang: Language,
    target_lang: Language,
) -> Callable[[list[TranslationTarget]], list[str]]:
    """
    Initialize the deepl translator using the API-key from user's configuration and return a partially applied function for translating
    :param user_group: User whose API-key will be used to make translations TODO Make just the key
    :param source_lang: Language that is requested to translate from
    :param target_lang: Language that is requested to translate into
    :return: A function for translating text with the specified languages using a DeepLTranslator instance.
    """
    # Get the API-key from database
    # TODO Is this cool or should the service be its own class separate from the db model?
    translator = DeeplTranslationService.query.first()
    translator.register(user_group)

    if not translator.supports(source_lang, target_lang):
        raise RouteException(
            description=f"The language pair from {source_lang} to {target_lang} is not supported with DeepL"
        )

    translate_func = init_translate(translator, source_lang, target_lang)
    return translate_func


# TODO Is it a bad idea to just make a copy of the free DeepL functions for the Pro side?
def init_deepl_pro_translate(
    user_group: UserGroup, source_lang: Language, target_lang: Language
) -> Callable[[list[TranslationTarget]], list[str]]:
    """
    Initialize the deepl translator using the API-key from user's configuration and return a partially applied function for translating
    :param user_group: User whose API-key will be used to make translations TODO Make just the key
    :param source_lang: Language that is requested to translate from
    :param target_lang: Language that is requested to translate into
    :return: A function for translating text with the specified languages using a DeepLTranslator instance.
    """
    # Get the API-key from database
    # TODO Is this cool or should the service be its own class separate from the db model?
    translator = DeeplProTranslationService.query.first()
    translator.register(user_group)

    if not translator.supports(source_lang, target_lang):
        raise RouteException(
            description=f"The language pair from {source_lang} to {target_lang} is not supported with DeepL"
        )

    translate_func = init_translate(translator, source_lang, target_lang)
    return translate_func
