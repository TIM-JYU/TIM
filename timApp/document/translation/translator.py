from dataclasses import dataclass
from subprocess import run as run_subprocess
from json import loads as json_loads


@dataclass
class Usage:
    character_count: int
    character_limit: int


class ITranslator:
    def translate(self, text: str, src_lang: str, target_lang: str) -> str:
        raise NotImplementedError

    def usage(self) -> Usage:
        raise NotImplementedError


@dataclass
class KonttiTranslator(ITranslator):

    # TODO Should maybe just ignore lang params like this:
    # def translate(self, text: str, *) -> str:
    def translate(
        self, text: str, src_lang: str | None = None, target_lang: str | None = None
    ) -> str:
        """
        Translate text - whatever it is - into Kontti-language
        https://fi.wikipedia.org/wiki/Kontinkieli
        :param text: Text that is ready to be translated
        :param src_lang: The language to translate from (ignored)
        :param target_lang: The language to translate into (ignored)
        :return: text translated into Kontti-language
        """
        s = ""
        words = text.split(" ")
        for word in words:
            if len(word) < 3 or not word.isalpha():
                s += word
            else:
                s += "Ko" if word[0].isupper() else "ko"
                s += word[2:] + word[0].lower() + word[1] + "ntti "
        return s

    def usage(self) -> Usage:
        return Usage(
            # No need to keep count
            character_count=0,
            # Unlimited amount of translation quota
            character_limit=int("inf"),
        )


@dataclass
class DeepLTranslator(ITranslator):
    api_key: str

    def translate(self, text: str, src_lang: str, target_lang: str) -> str:
        """
        Uses the DeepL API for translating text between languages
        :param text: Text to be translated
        :param src_lang: DeepL-compliant language code of input text
        :param target_lang: DeepL-compliant language code for target language
        :return: The input text translated into the target language
        """
        # TODO Handle the various HTTP error codes that API can return
        # TODO Do not make the requests using curl
        response = run_subprocess(
            [
                "curl",
                "-H",
                f"Authorization: DeepL-Auth-Key {self.api_key}",
                "https://api-free.deepl.com/v2/translate",
                "-d",
                f"text={text}",
                "-d",
                f"source_lang={src_lang}",
                "-d",
                f"target_lang={target_lang}",
            ],
            capture_output=True,
        )
        if response.returncode == 0 and response.stdout:
            resp_json = json_loads(response.stdout.decode("utf-8"))
            # TODO Check all the indices (or assemble a larger request based on whole Document?)
            return resp_json["translations"][0]["text"]
        else:
            raise Exception(
                "Failed to call DeepL API / Translate with curl: "
                + response.stderr.decode("utf-8")
            )

    def usage(self) -> Usage:
        response = run_subprocess(
            [
                "curl",
                "-H",
                f"Authorization: DeepL-Auth-Key {self.api_key}",
                "https://api-free.deepl.com/v2/usage",
            ],
            capture_output=True,
        )
        if response.returncode == 0 and response.stdout:
            resp_json = json_loads(response.stdout.decode("utf-8"))
            return Usage(
                character_count=int(resp_json["character_count"]),
                character_limit=int(resp_json["character_limit"]),
            )
        else:
            raise Exception(
                "Failed to call DeepL API / Usage with curl: "
                + response.stderr.decode("utf-8")
            )
