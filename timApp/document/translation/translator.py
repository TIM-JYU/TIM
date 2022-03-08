from dataclasses import dataclass
import requests


@dataclass
class Usage:
    character_count: int
    character_limit: int


@dataclass
class LanguagePairing:
    en: list[str]
    fi: list[str]

    def get(self, language: str) -> list[str]:
        """
        Get the list of supported languages matching the language code
        :param language: Source language code
        :return: List of supported target languages
        """
        # TODO Use the ISO-codes
        # TODO think about just having this whole class be a dict...
        if language.lower() == "en":
            return self.en
        if language.lower() == "fi":
            return self.fi
        return []


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

    def _post(self, url_slug: str, data=None) -> dict:
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
        resp_json = self._post("languages")
        # NOTE it is assumed, that DeepL supports all its languages translated both ways
        lang_codes = list(map(lambda x: x["language"], resp_json))
        return LanguagePairing(en=lang_codes, fi=lang_codes)

    def supports(self, source_lang: str, target_lang: str) -> bool:
        """
        Check that the source language can be translated into target language by the translation API
        :param source_lang: Language of original text
        :param target_lang: Language to translate into
        :return: True, if the pairing is supported
        """
        # TODO Use some standard codes for the languages
        return target_lang.upper() in self.languages().get(source_lang.upper())
