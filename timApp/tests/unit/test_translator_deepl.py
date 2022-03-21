import unittest

from timApp.document.translation import routes
from timApp.document.translation import translator


class TestTranslatorDeepl(unittest.TestCase):
    def test_incorrect_key(self):
        deepltrans = routes.init_deepl_translator("FI", "EN")
        translator.DeepLTranslator.api_key = "incorrect key"
        translatedtext = ["first str", "second str", "third str"]
        with self.assertRaises(Exception) as context:
            routes.translate(deepltrans, "translate text", "FI", "EN")

            self.assertTrue(
                "DeepL API / languages responded with 403" in context.exception
            )
        # add assertion here


if __name__ == "__main__":
    unittest.main()
