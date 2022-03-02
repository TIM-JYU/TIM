from dataclasses import dataclass


@dataclass
class Usage:
    character_count: int
    character_limit: int


@dataclass
class KonttiTranslator:
    """Translate text - whatever it is - into Kontti-language
    https://fi.wikipedia.org/wiki/Kontinkieli
    """

    def translate(self, text: str) -> str:
        """
        Translates text but does not perfmorm any pre- or postprocessing
        :param text: Text that is ready to be translated
        :param src_lang: The language to translate from
        :param target_lang: The language to translate into
        :return: text translated into the target language
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
