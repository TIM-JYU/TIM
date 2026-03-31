import unittest

from util.utils import resolve_doc_path


class ResolveDocPathTest(unittest.TestCase):
    REF = "https://localhost/view/users/vesa/koe/lainaus/lainaa"

    # --- Absolute paths -----------------------------------------------------

    def test_absolute_path_unchanged(self):
        self.assertEqual(
            resolve_doc_path(self.REF, "kurssit/tie/ohj1/moniste/ohj-1"),
            "kurssit/tie/ohj1/moniste/ohj-1",
        )

    # --- Basic relative paths ----------------------------------------------

    def test_relative_same_directory_file(self):
        self.assertEqual(
            resolve_doc_path(self.REF, "_/toinen"),
            "users/vesa/koe/lainaus/toinen",
        )

    def test_relative_parent_directory_file(self):
        self.assertEqual(
            resolve_doc_path(self.REF, "_/../toinen"),
            "users/vesa/koe/toinen",
        )

    def test_relative_multiple_parents(self):
        self.assertEqual(
            resolve_doc_path(self.REF, "_/../../x"),
            "users/vesa/x",
        )

    # --- Keeping filename ---------------------------------------------------

    def test_relative_dot_keeps_filename(self):
        self.assertEqual(
            resolve_doc_path(self.REF, "_/."),
            "users/vesa/koe/lainaus/lainaa",
        )

    def test_relative_dotdot(self):
        self.assertEqual(
            resolve_doc_path(self.REF, "_/.."),
            "users/vesa/koe/lainaus",
        )

    def test_relative_trailing_slash_keeps_filename(self):
        self.assertEqual(
            resolve_doc_path(self.REF, "_/./"),
            "users/vesa/koe/lainaus/lainaa",
        )

    # --- No referrer --------------------------------------------------------

    def test_relative_without_referrer_returns_none(self):
        self.assertIsNone(resolve_doc_path(None, "_/test"))

    # --- Edge cases ---------------------------------------------------------

    def test_root_boundary_not_crashing(self):
        result = resolve_doc_path(self.REF, "_/../../../../x")
        self.assertIsInstance(result, str)

    def test_empty_relative_path(self):
        self.assertEqual(
            resolve_doc_path(self.REF, "_/"),
            "users/vesa/koe/lainaus/lainaa",
        )

    def test_only_underscore_not_relative(self):
        self.assertEqual(
            resolve_doc_path(self.REF, "_"), "users/vesa/koe/lainaus/lainaa"
        )

    def test_invalid_referrer_path(self):
        bad_ref = "https://localhost/view"
        self.assertIsNone(resolve_doc_path(bad_ref, "_/test"))

    # --- Complex combinations ----------------------------------------------

    def test_complex_mixed_path(self):
        self.assertEqual(
            resolve_doc_path(self.REF, "_/../x/./y/../z"),
            "users/vesa/koe/x/z",
        )


if __name__ == "__main__":
    unittest.main()
