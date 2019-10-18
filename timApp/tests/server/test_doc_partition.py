from timApp.tests.server.timroutetest import TimRouteTest
from timApp.document.specialnames import TEMPLATE_FOLDER_NAME, PREAMBLE_FOLDER_NAME

class DocPartitionTest(TimRouteTest):

    def test_set_and_unset_view_range_cookie(self):
        self.json_post(url=f'/viewrange/set/piecesize',
                       json_data={'pieceSize': 10},
                       expect_cookie=('r','10'),
                       expect_status=200)
        self.json_post(url=f'/viewrange/set/piecesize',
                       json_data={'pieceSize': -10},
                       expect_content="Invalid piece size",
                       expect_cookie=('r', None),
                       expect_status=400)
        self.get(url=f'/viewrange/unset/piecesize',
                 expect_cookie=('r', None),
                 expect_status=200)


    def test_calculating_part_indices(self):
        self.login_test1()
        self.json_post(url=f'/viewrange/set/piecesize',
                       json_data={'pieceSize': 5},
                       expect_cookie=('r','5'),
                       expect_status=200)
        forwards = 1
        backwards = 0
        d1 = self.create_doc()
        d2 = self.create_doc(initial_par=["1","2","3","4","5","6","7","8","9","10"])
        # Empty document.
        self.get(f'/viewrange/get/{d1.id}/0/1', expect_content={'b': 0, 'e': 0})
        # Begin index at the doc beginning.
        self.get(f'/viewrange/get/{d2.id}/0/{forwards}', expect_content={'b': 0, 'e': 5})
        # Begin index is at the doc end; rounded to avoid a too short part.
        self.get(f'/viewrange/get/{d2.id}/10/{forwards}', expect_content={'b': 8, 'e': 10})
        # Begin index is 5 and moving backwards.
        self.get(f'/viewrange/get/{d2.id}/5/{backwards}', expect_content={'b': 0, 'e': 5})
        # Begin index is atthe doc beginning and moving backwards; rounded to avoid a too short part.
        self.get(f'/viewrange/get/{d2.id}/0/{backwards}', expect_content={'b': 0, 'e': 2})
        self.json_post(url=f'/viewrange/set/piecesize',
                       json_data={'pieceSize': 8},
                       expect_cookie=('r','8'),
                       expect_status=200)
        # Test rounding when remaining pars are shorter than half the piece size.
        self.get(f'/viewrange/get/{d2.id}/0/{forwards}', expect_content={'b': 0, 'e': 10})


    def test_partitioning_document(self):
        self.login_test1()
        d = self.create_doc(initial_par=["1", "2", "3", "4", "5", "6", "7", "8", "9", "10"])

        # No partitioning.
        tree = self.get(d.url, as_tree=True)
        self.assert_content(tree, ["1", "2", "3", "4", "5", "6", "7", "8", "9", "10"])

        self.json_post(url=f'/viewrange/set/piecesize',
                       json_data={'pieceSize': 5},
                       expect_cookie=('r','5'),
                       expect_status=200)

        # Partitioning on, no URL parameters.
        tree = self.get(d.url, as_tree=True)
        self.assert_content(tree, ["1","2","3","4","5"])

        # Check ranges for navigation links:
        self.assert_js_variable(tree, "nav_ranges", [
            {"b": 0, "e": 5, "name": "First"},
            {"b": 0, "e": 2, "name": "Previous"},
            {"b": 5, "e": 10, "name": "Next"},
            {"b": 5, "e": 10, "name": "Last"}])

        # Partitioning with URL parameters, mid-document range.
        tree = self.get(d.url, query_string={'b': 2, 'e': 6}, as_tree=True)
        self.assert_content(tree, ["3", "4", "5", "6"])

        # Check ranges for navigation links:
        self.assert_js_variable(tree, "nav_ranges", [
            {"b": 0, "e": 5, "name": "First"},
            {"b": 0, "e": 4, "name": "Previous"},
            {"b": 6, "e": 10, "name": "Next"},
            {"b": 5, "e": 10, "name": "Last"}])

        # Partitioning with URL parameters, whole document range.
        tree = self.get(d.url, query_string={'b': 0, 'e': 10}, as_tree=True)
        self.assert_content(tree, ["1", "2", "3", "4", "5", "6", "7", "8", "9", "10"])

        # Check ranges for navigation links:
        self.assert_js_variable(tree, "nav_ranges", [
            {"b": 0, "e": 5, "name": "First"},
            {"b": 0, "e": 2, "name": "Previous"},
            {"b": 8, "e": 10, "name": "Next"},
            {"b": 5, "e": 10, "name": "Last"}])


    def test_partitioning_document_with_overflowing_range(self):
        self.login_test1()
        d = self.create_doc(initial_par=["1","Kissa","3","4","5","6","Koira","8","9","10"])
        self.json_post(url=f'/viewrange/set/piecesize',
                       json_data={'pieceSize': 5},
                       expect_cookie=('r','5'),
                       expect_status=200)

        # Overflowing range end.
        tree = self.get(d.url, query_string={'b': 6, 'e': 100}, as_tree=True)
        self.assert_content(tree, ["Koira","8","9","10"])

        # Negative range begin.
        tree = self.get(d.url, query_string={'b': -100, 'e': 4}, as_tree=True)
        self.assert_content(tree, ["1", "Kissa", "3", "4"])


    def test_partitioning_empty_document(self):
        self.login_test1()
        self.json_post(url=f'/viewrange/set/piecesize',
                       json_data={'pieceSize': 5},
                       expect_cookie=('r', '5'),
                       expect_status=200)
        d = self.create_doc()

        tree = self.get(d.url, query_string={'b': 3, 'e': 14}, as_tree=True)
        self.assert_content(tree, [])


    def test_partitioning_with_preambles(self):
        self.login_test1()
        self.json_post(url=f'/viewrange/set/piecesize',
                       json_data={'pieceSize': 5},
                       expect_cookie=('r','5'),
                       expect_status=200)
        d = self.create_doc(
            initial_par=["1","Kissa","3","4","5","6","Koira","8","9","10"])
        self.create_preamble_for(d, initial_par=["Preamble par 1", "Preamble par 2"])

        # Partitioning on, no URL parameters.
        tree = self.get(d.url, as_tree=True)
        self.assert_content(tree, ["Preamble par 1", "Preamble par 2", "1","Kissa","3","4","5"])

        # Partitioning with URL parameters, starting from doc beginning.
        tree = self.get(d.url, query_string={'b': 0, 'e': 6, 'preamble': 'true'}, as_tree=True)
        self.assert_content(tree, ["Preamble par 1", "Preamble par 2", "1","Kissa","3","4","5", "6"])

        # Partitioning with URL parameters, starting from mid-document.
        tree = self.get(d.url, query_string={'b': 6, 'e': 9, 'preamble': 'true'}, as_tree=True)
        self.assert_content(tree, ["Koira","8","9"])


    def test_partitioning_with_special_class_preambles(self):
        self.login_test2()
        self.json_post(url=f'/viewrange/set/piecesize',
                       json_data={'pieceSize': 5},
                       expect_cookie=('r','5'),
                       expect_status=200)
        d = self.create_doc(path=self.get_personal_item_path(f'2/test'),
                            initial_par=["1","Kissa","3","4","5","6","Koira","8","9","10"])
        self.create_preamble_for(d, initial_par=["Preamble par 1", "Preamble par 2", """
#- {.includeInParts}
Preamble par 3"""])
        # Partitioning on, no URL parameters; all pars should be included.
        tree = self.get(d.url, as_tree=True)
        self.assert_content(tree, ["Preamble par 1", "Preamble par 2", "Preamble par 3", "1", "Kissa", "3", "4", "5"])

        # Partitioning with URL parameters, starting from mid-document; special par should be included.
        tree = self.get(d.url, query_string={'b': 6, 'e': 9, 'preamble': 'true'}, as_tree=True)
        self.assert_content(tree, ["Preamble par 3","Koira","8","9"])

        # Preamble loading disabled.
        tree = self.get(d.url, query_string={'b': 5, 'e': 8, 'preamble': 'false'}, as_tree=True)
        self.assert_content(tree, ["6","Koira","8"])


    def test_partitioning_range(self):
        # TODO: Range check not implemented yet.
        pass
