"""
Contains implementation of the TranslationService-interface for the DeepL
machine translator: https://www.deepl.com/translator.

Both DeepL API Free and DeepL API Pro -versions.
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
import requests.adapters
from requests import post, Response
from requests.exceptions import JSONDecodeError
from sqlalchemy import select
from sqlalchemy.orm import Mapped
from werkzeug.exceptions import HTTPException

from timApp.document.translation.language import Language
from timApp.document.translation.translationparser import TranslateApproval, NoTranslate
from timApp.document.translation.translator import (
    RegisteredTranslationService,
    TranslationServiceKey,
    TranslateBlock,
    Usage,
    LanguagePairing,
    replace_md_aliases,
)
from timApp.timdb.sqa import run_sql
from timApp.user.usergroup import UserGroup
from timApp.util import logger
from timApp.util.flask.cache import cache
from timApp.util.flask.requesthelper import NotExist, RouteException
from tim_common.vendor.requests_futures import (
    FuturesSession,
    Future,
    DEFAULT_MAX_WORKERS,
)

LANGUAGES_CACHE_TIMEOUT = 3600 * 24  # seconds

# Limit for how big a request the DeepL API accepts
# Exact limit is 128KiB (see https://developers.deepl.com/docs/resources/usage-limits#api-limits),
# but due to request headers/overhead we will have to stick to a lower limit.
# The limit corresponds roughly to the same amount of characters in the request payload.
DEEPL_REQUEST_SIZE_LIMIT = 100_000
# How many text 'packets' or parameters the DeepL API accepts in one request
DEEPL_REQUEST_MAX_TEXT_PARAMS = 50

# Error messages for exceptions on DeepL translation requests
DEEPL_EXCEPTION_MESSAGE: dict[int, str] = {
    400: f"The request to the DeepL API was bad. Please check your parameters.",
    401: f"Authorization failed. Please check your DeepL API key for typos.",
    403: f"You lack permission to use this service.",
    404: f"The requested translator could not be found. Please try again later.",
    413: f"The request size exceeds the API's limit. Please try again with a smaller document.",
    414: f"The request URL is too long. Please contact TIM support.",
    429: f"Too many requests were sent. Please wait and resend the request later.",
    456: f"You have exceeded your character quota. Please try again when your quota has reset.",
    500: f"An internal error occurred on the DeepL server. Please try again.",
    503: f"Translator currently unavailable. Please try again later.",
    529: f"Too many requests were sent. Please wait and resend the request later.",
}


class DeepLException(HTTPException):
    description: str


class DeeplTranslationService(RegisteredTranslationService):
    """Translation service using the DeepL API Free."""

    def __init__(self, values: dict):
        """
        Constructor for setting all the needed DeepL-specific fields.

        :param values: Contains values for the fields.
        """
        self.ignore_tag = values["ignore_tag"]
        self.service_url = values["service_url"]

    # TODO: Would be better as non-optional, but that prevents creating
    #  non-DeeplTranslationService -subclasses of TranslationService.
    service_url: Mapped[Optional[str]]
    """The url base for the API calls."""

    # TODO: Would be better as non-optional, but that prevents creating
    #  non-DeeplTranslationService -subclasses of TranslationService.
    ignore_tag: Mapped[Optional[str]]
    """The XML-tag name to use for ignoring pieces of text when XML-handling is
    used. Should be chosen to be some uncommon string not found in many texts.
    """

    def register(self, user_group: UserGroup) -> None:
        """
        Set headers to use the user group's API-key ready for translation
        calls.

        :param user_group: The user group whose API key will be used.
        :raises NotExist: If no API key is found.
        :raises RouteException: If more than one key is found from user.
        """
        # One user group should match one service per one key.
        api_key = (
            run_sql(
                select(TranslationServiceKey).filter(
                    TranslationServiceKey.service_id == self.id,
                    TranslationServiceKey.group_id == user_group.id,
                )
            )
            .scalars()
            .all()
        )
        if len(api_key) == 0:
            raise NotExist(
                "Please add a DeepL API key that corresponds the chosen plan into your account"
            )
        if len(api_key) > 1:
            # TODO Does revealing this info compromise security in any way?
            raise RouteException(
                "A user should not have more than one (1) API-key per service."
            )
        self.headers = {"Authorization": f"DeepL-Auth-Key {api_key[0].api_key}"}

    @property
    def headers(self) -> dict[str, str]:
        """Request-headers needed for authentication with the API-key."""
        return getattr(self, "_headers") if hasattr(self, "_headers") else {}

    @headers.setter
    def headers(self, value: dict[str, str]) -> None:
        setattr(self, "_headers", value)

    @property
    def source_Language_code(self) -> str:
        """The source language's code (helps handling regional variants that DeepL
        doesn't differentiate).
        """
        return (
            getattr(self, "_source_Language_code")
            if hasattr(self, "_source_Language_code")
            else None
        )

    @source_Language_code.setter
    def source_Language_code(self, value: str) -> None:
        setattr(self, "_source_Language_code", value)

    # TODO Change the dicts to DeepLTranslateParams and DeeplResponse or smth
    def _post(self, url_slug: str, data: dict | None = None) -> dict:
        """
        Perform an authorized POST-request to the DeepL-API.

        :param url_slug: The last part of URL-path for the API function without
         the starting '/' slash.
        :param data: Data to be transmitted along the request.
        :return: The JSON-response returned by the API.
        """
        resp = post(f"{self.service_url}/{url_slug}", data=data, headers=self.headers)

        return self._handle_post_response(resp)

    def _handle_post_response(self, resp: Response) -> dict:
        """
        Handle converting successful response into JSON or raise an exception
        with a fitting message.

        :param resp: The DeepL API -response to handle.
        :return: The JSON-response returned by the API.
        :raises RouteException: If DeepL API returned an error.
        :raises Exception: If DeepL API returned an unknown or unexpected error.
        """
        if resp.ok:
            try:
                return resp.json()
            except JSONDecodeError as e:
                raise Exception(f"DeepL API returned malformed JSON: {e}")
        else:
            status_code = resp.status_code
            if status_code not in DEEPL_EXCEPTION_MESSAGE:
                # TODO Do not show this to user. Confirm, that wuff is sent.
                raise DeepLException(
                    description=f"'{resp.url}' responded with: {status_code}",
                )
            else:
                raise DeepLException(
                    description=DEEPL_EXCEPTION_MESSAGE[status_code],
                )

    def _translate(
        self,
        session: FuturesSession,
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
    ) -> Future:
        """
        Supports most of the parameters of a DeepL API translate call.
        See https://www.deepl.com/docs-api/translating-text/request/ for valid
        parameter values and more information.

        With tag handling for example to handle the tag "<x>" the parameter
        value should be "x".

        :param session: Object to use in constructing the single DeepL API
         translate-call.
        :param text: Text to translate that can contain XML.
        :param source_lang: Language of the text.
        :param target_lang: Language to translate the text into.
        :param split_sentences: Is text split before translation.
        :param preserve_formatting: Is formatting preserved during translation.
        :param tag_handling: Are tags intelligently handled. XML and HTML are
         currently supported.
        :param non_splitting_tags: Tags that never split sentences.
        :param splitting_tags: Tags that always split sentences.
        :param ignore_tags: Tags to ignore when translating.
        :return: A Future-object of the DeepL API translate-call.
        """

        src_lang = source_lang

        if source_lang is not None and (
            source_lang.lower() == "en-gb" or source_lang.lower() == "en-us"
        ):
            src_lang = "en"

        logger.log_debug(f"Amount of separate translatable texts: {str(len(text))}/50")

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

        return session.post(
            f"{self.service_url}/translate", data=data, headers=self.headers
        )

    @cache.memoize(timeout=LANGUAGES_CACHE_TIMEOUT, args_to_ignore=["self"])
    def _languages(self, *, is_source: bool) -> dict:
        """
        Get languages supported by the API.

        :param is_source: Flag to query for supported source-languages.
        :return: Languages supported in translations by type (source or
         target).
        """
        return self._post(
            "languages", data={"type": "source" if is_source else "target"}
        )

    def preprocess(self, elem: TranslateApproval) -> None:
        """
        Protect the text inside element from mangling in translation by adding
        XML-tags.

        :param elem: The element to add XML-protection-tags to.
        :return None. The tag is added to the input object.
        """
        # TODO If the protection tag is found in the content text, somehow
        #  encode such tag first.
        if type(elem) is NoTranslate:
            elem.text = f"<{self.ignore_tag}>{elem.text}</{self.ignore_tag}>"

    def postprocess(self, text: str) -> str:
        """
        Remove unnecessary protection tags from the text and change defined
        aliases back to Markdown syntax.

        :param text: The text returned from DeepL API after translation.
        :return: Text with the needed operations performed to more closely
         match the text before passing it to DeepL API.
        """
        return (
            replace_md_aliases(text)
            .replace(f"<{self.ignore_tag}>", "")
            .replace(f"</{self.ignore_tag}>", "")
        )

    def translate(
        self,
        texts: list[TranslateBlock],
        source_lang: Language | None,
        target_lang: Language,
        tag_handling: str = "xml",
        max_workers: int | None = None,
        max_retries: int | None = None,
    ) -> list[str]:
        """
        Use the DeepL API to translate text between languages.

        :param texts: Some set of texts to be translated.
        :param source_lang: Language of input text. None value makes DeepL
         guess it from the text.
        :param target_lang: Language for target language.
        :param tag_handling: See comment in superclass.
        :return: List of strings in target language with the non-translatable
         parts intact.
        :param max_workers: Maximum number of workers to use for sending translation requests
        :param max_retries: Maximum number of retries on a failed translation request
        """
        source_lang_code = source_lang.lang_code if source_lang else None

        # Get the translatable text of objects and add XML-tag -protection to
        # them if so needed.
        if tag_handling == "xml":
            # TODO This multidimensionalism of lists is hard to read
            for block in texts:
                for elem in block:
                    self.preprocess(elem)
        # TODO This multidimensionalism of lists is hard to read
        # Combine the strings of each block for maximum-effectiveness of the
        # translation-call.
        protected_texts = list(
            map(lambda xs: "".join(map(lambda x: x.text, xs)), texts)
        )

        # Translate texts 50 at a time to match DeepL-spec:
        # "Up to 50 text parameters can be submitted in one request."
        # https://www.deepl.com/docs-api/translating-text/large-volumes/
        translate_calls = list()
        # Initialize the session for parallel translate-calls.
        # To avoid sending too many requests at a time, the calling function should
        # set suitably low values for the optional parameters `max_workers` and `max_retries`.
        session = FuturesSession(
            max_workers=max_workers if max_workers else DEFAULT_MAX_WORKERS,
            adapter_kwargs=dict(
                max_retries=max_retries
                if max_retries
                else requests.adapters.DEFAULT_RETRIES
            ),
        )

        i = 0
        while i < len(protected_texts):
            req_chars = 0
            num_params = 0
            start = i

            if not len(protected_texts[i]) < DEEPL_REQUEST_SIZE_LIMIT:
                i += 1
            else:
                # In addition to the text parameters limit, requests also have a size limit in bytes
                while (
                    i < len(protected_texts)
                    and num_params < DEEPL_REQUEST_MAX_TEXT_PARAMS
                    and req_chars + len(protected_texts[i]) < DEEPL_REQUEST_SIZE_LIMIT
                ):
                    req_chars += len(protected_texts[i])
                    num_params += 1
                    i += 1

            call = self._translate(
                session,
                protected_texts[start:i],
                # Send uppercase, because it is used in DeepL documentation.
                source_lang_code.upper() if source_lang_code else None,
                target_lang.lang_code.upper(),
                # "1" (for example) keeps original document's empty newlines.
                split_sentences="1",
                # NOTE preserve_formatting=1 might remove punctuation even
                # though DeepL should not make guesses of the content.
                preserve_formatting="0",
                tag_handling=tag_handling,
                ignore_tags=[self.ignore_tag] if self.ignore_tag else [],
            )
            translate_calls.append(call)

        # Wait for the parallel calls to finish and get their results in
        # order.
        translation_resps = list()
        for call in translate_calls:
            resp = call.result()
            # TODO Handle exceptions raised in the error handling.
            # NOTE: this is disabled for now, since we can't recover successful results after _handle_post_response
            #       throws an exception. Instead, we will deal with the failed requests a bit later
            # resp_json = self._handle_post_response(resp)

            if resp.ok:
                try:
                    resp_json = resp.json()
                except JSONDecodeError as e:
                    raise Exception(f"DeepL API returned malformed JSON: {e}")
            else:
                status_code = resp.status_code
                if status_code not in DEEPL_EXCEPTION_MESSAGE:
                    # TODO Do not show this to user. Confirm, that wuff is sent.
                    raise DeepLException(
                        description=f"'{resp.url}' responded with: {status_code}",
                    )
                # TODO: salvage successful translation results on (some) other exceptions as well.
                # Oversized request, return empty text to signify paragraph should not be modified.
                # TODO: inform user that some paragraphs were not translated?
                #       We already have the 'check translation' marks, so that seems a bit redundant.
                elif status_code == 413:
                    resp_json = {"translations": [{"text": ""}]}
                else:
                    raise DeepLException(
                        description=DEEPL_EXCEPTION_MESSAGE[status_code],
                    )

            translation_resps += resp_json["translations"]

        # Insert the text-parts sent to the API into correct places in
        # original elements.
        translated_texts = list()
        for resp in translation_resps:
            clean_block = (
                self.postprocess(resp["text"])
                if tag_handling == "xml"
                else resp["text"]
            )
            translated_texts.append(clean_block)
        return translated_texts

    def usage(self) -> Usage:
        """
        Fetch current API usage of the registered key from DeepL.

        :return: Usage returned from DeepL.
        """
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

                # This is needed because DeepL's source languages only include
                # English (EN) and not regional variants.
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
            en: Language | None = Language(
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

    @cache.memoize(timeout=LANGUAGES_CACHE_TIMEOUT, args_to_ignore=["self"])
    def languages(self) -> LanguagePairing:
        """
        Asks the DeepL API for the list of supported languages and turns the
        returned language codes to Languages found in the database.

        :return: Dictionary of source langs to lists of target langs, that are
         supported by the API and also found in database.
        """

        def get_lang(deepl_lang: dict) -> Language | None:
            try:
                language = deepl_lang["language"]
                code = langcodes.get(language).to_tag()

                # This is needed because DeepL's source languages only include
                # English (EN) and not regional variants.
                if code.lower() == "en":
                    code = self.source_Language_code
                return Language.query_by_code(code)
            except LookupError:
                return None

        # Query API for supported source and target languages and transform
        # them into the return type.
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
        Check that the source language can be translated into target language
        by the translation API.

        :param source_lang: Language to check the translation capability from.
        :param target_lang: Language to check the translation capability into.
        :return: True, if the pairing is supported.
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

        # The target language is found by the primary key.
        # TODO is this too much? Can't strings be just as good?
        #  Maybe better would be to handle Languages by their database id's?
        return any(x.lang_code == target_lang.lang_code for x in supported_languages)

    def supports_tag_handling(self, tag_type: str) -> bool:
        """
        Check if DeeplTranslationService supports a tag-handling.

        :param tag_type: The tag-type to check handling for.
        :return: True if the tag-type is supported.
        """
        return tag_type in ["xml", "html"]

    # TODO Make the value an enum like with Verification?
    __mapper_args__ = {"polymorphic_identity": "DeepL Free"}


class DeeplProTranslationService(DeeplTranslationService):
    """Translation service using the DeepL API Pro."""

    # TODO Make the value an enum like with Verification?
    __mapper_args__ = {"polymorphic_identity": "DeepL Pro"}


def purge_languages_cache() -> None:
    """
    Purges the cached LanguagePairings from the DeeplTranslationService.
    Mostly needed when new languages are added, but the cache has not refreshed yet.
    :return: None
    """
    cache.delete_memoized(DeeplTranslationService.languages)
