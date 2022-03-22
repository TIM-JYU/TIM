import unittest

from timApp.document.translation import routes
from timApp.document.translation import translator


class TestTranslatorDeepl(unittest.TestCase):
    def test_incorrect_key(self):
        with self.assertRaises(Exception) as e:
            routes.init_deepl_translator("FI", "EN")

        self.assertEqual(
            e.exception.args[0],
            "DeepL API / languages responded with 403",
        )


# code in preparation for when the initialization changes
#        deepltrans = routes.init_deepl_translator("FI", "EN")
#        translator.DeepLTranslator.api_key = "incorrect key"
#        translatedtext = ["first str", "second str", "third str"]
#
#        with self.assertRaises(Exception) as e:
#            routes.translate(deepltrans, "translate text", "FI", "EN")
#
#        self.assertEqual(
#            e.exception.args[0],
#            "DeepL API / languages responded with 403",
#        )


# add assertion here


if __name__ == "__main__":
    unittest.main()
