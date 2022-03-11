import requests

from dataclasses import dataclass
from enum import Enum
from typing import Dict
from timApp.util.flask.requesthelper import RouteException


@dataclass
class Usage:
    character_count: int
    character_limit: int


# TODO Does dataclass make this unhashable?
class LangCode(Enum):
    """
    Selection of ISO 639-1 two-letter codes as described at: https://en.wikipedia.org/wiki/List_of_ISO_639-1_codes
    """

    ENGLISH: str = "EN"
    FINNISH: str = "FI"
    SWEDISH: str = "SV"


# TODO make dataclass?
class LanguagePairing(Dict[LangCode, list[LangCode]]):
    """
    Holds the list of supported source language codes mapping to target language codes
    """

    ...


class ITranslator:
    def translate(self, text: list[str], src_lang: str, target_lang: str) -> str:
        raise NotImplementedError

    def usage(self) -> Usage:
        raise NotImplementedError

    def languages(self) -> LanguagePairing:
        raise NotImplementedError


@dataclass
class DeepLTranslator(ITranslator):
    api_key: str
    url: str = "https://api-free.deepl.com/v2"

    def __post_init__(self) -> None:
        self.headers = {"Authorization": f"DeepL-Auth-Key {self.api_key}"}

    # TODO Change the dict to DeepLTranslateParams or smth
    def _post(self, url_slug: str, data: dict | None = None) -> dict:
        """
        Perform a authorized post-request to the DeepL-API
        :param url_slug: The last part of URL-path for the API function _without_ the starting '/' slash
        :param data: Data to be transmitted along the request
        :return: The response's JSON TODO Is dict allowed?
        """
        resp = requests.post(self.url + "/" + url_slug, data=data, headers=self.headers)

        if resp.ok:
            try:
                return resp.json()
            except requests.exceptions.JSONDecodeError as e:
                raise Exception(f"DeepL API returned malformed JSON: {e}")
        else:
            # TODO Handle the various HTTP error codes that API can return
            # (Using Python 3.10's match-statement would be cool here...)
            raise Exception(f"DeepL API / {url_slug} responded with {resp.status_code}")

    def _translate(
        self,
        text: list[str],
        source_lang: str,
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
        data = {
            "text": text,
            "source_lang": source_lang,
            "target_lang": target_lang,
            "split_sentences": split_sentences,
            "preserve_formatting": preserve_formatting,
            "tag_handling": tag_handling,
            "non_splitting_tags": ",".join(non_splitting_tags),
            "splitting_tags": ",".join(splitting_tags),
            "ignore_tags": ",".join(ignore_tags),
        }

        return self._post("translate", data)

    def translate(self, text: list[str], source_lang: str, target_lang: str) -> str:
        """
        Uses the DeepL API for translating text between languages
        :param text: Text to be translated
        :param source_lang: DeepL-compliant language code of input text
        :param target_lang: DeepL-compliant language code for target language
        :return: The input text translated into the target language
        """
        resp_json = self._translate(text, source_lang, target_lang)
        # TODO Use a special structure to insert the text-parts sent to the API into correct places in original text
        return "".join([tr["text"] for tr in resp_json["translations"]])

    def usage(self) -> Usage:
        resp_json = self._post("usage")
        return Usage(
            character_count=int(resp_json["character_count"]),
            character_limit=int(resp_json["character_limit"]),
        )

    def languages(self) -> LanguagePairing:
        """
        Asks the DeepL API for the list of supported languages (Note: the supported language _pairings_ are not explicitly specified) and picks the returned language codes that when turned to lowercase match the LangCode-enum's values.
        :return: Dictionary of source langs to lists of target langs, that are supported by the API and also defined in LangCode
        """
        resp_json = self._post("languages")
        lang_codes: list[LangCode] = list(
            filter(
                None,
                map(
                    # The DeepL language code might contain '-' for example in 'EN-GB'
                    lambda x: get_lang_code(x["language"].split("-")[0].upper()),
                    resp_json,
                ),
            )
        )
        # NOTE it is assumed, that DeepL supports all its languages translated both ways,
        # thus all selected languages are mapped to all selected languages
        d = {code: lang_codes for code in lang_codes}
        return LanguagePairing(d)

    def supports(self, source_lang: str, target_lang: str) -> bool:
        """
        Check that the source language can be translated into target language by the translation API
        :param source_lang: Language of original text TODO In what format?
        :param target_lang: Language to translate into TODO In what format?
        :return: True, if the pairing is supported
        """
        source_lang_code = get_lang_code(source_lang)
        if source_lang_code is None:
            raise RouteException(
                description=f"The language '{source_lang}' is not supported"
            )

        target_lang_code = get_lang_code(target_lang)
        if target_lang_code is None:
            raise RouteException(
                description=f"The language '{target_lang}' is not supported"
            )

        supported_languages: list[LangCode] = self.languages()[source_lang_code]

        return target_lang_code in supported_languages


def get_lang_code(s: str) -> LangCode | None:
    try:
        return LangCode(s)
    except KeyError:
        return None
    except ValueError:
        return None
