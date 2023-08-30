from timApp.document.caching import clear_doc_cache
from timApp.item.partitioning import INCLUDE_IN_PARTS_CLASS_NAME
from timApp.tests.server.timroutetest import TimRouteTest


class DocPartitionTest(TimRouteTest):
    def test_set_and_unset_view_range_cookie(self):
        self.json_post(
            url=f"/viewrange/set/piecesize",
            json_data={"pieceSize": 10},
            expect_cookie=("r", "10"),
            expect_status=200,
        )
        self.json_post(
            url=f"/viewrange/set/piecesize",
            json_data={"pieceSize": -10},
            expect_content="Invalid piece size",
            expect_cookie=("r", None),
            expect_status=400,
        )
        self.get(
            url=f"/viewrange/unset/piecesize",
            expect_cookie=("r", None),
            expect_status=200,
        )

    def test_no_preferred_size(self):
        self.login_test1()
        self.get(url=f"/viewrange/unset/piecesize")
        d = self.create_doc()
        self.get(d.url, query_string={"b": 0, "e": 0})

    def test_calculating_part_indices(self):
        self.login_test1()
        self.json_post(
            url=f"/viewrange/set/piecesize",
            json_data={"pieceSize": 5},
            expect_cookie=("r", "5"),
            expect_status=200,
        )
        forwards = 1
        backwards = 0
        d1 = self.create_doc()
        d2 = self.create_doc(
            initial_par=["1", "2", "3", "4", "5", "6", "7", "8", "9", "10"]
        )
        # Empty document.
        self.get(
            f"/viewrange/get/{d1.id}/0/1",
            expect_content={"b": 0, "e": 0, "is_full": True},
        )
        # Begin index at the doc beginning.
        self.get(
            f"/viewrange/get/{d2.id}/0/{forwards}",
            expect_content={"b": 0, "e": 5, "is_full": False},
        )
        # Begin index is at the doc end; rounded to avoid a too short part.
        self.get(
            f"/viewrange/get/{d2.id}/10/{forwards}",
            expect_content={"b": 8, "e": 10, "is_full": False},
        )
        # Begin index is 5 and moving backwards.
        self.get(
            f"/viewrange/get/{d2.id}/5/{backwards}",
            expect_content={"b": 0, "e": 5, "is_full": False},
        )
        # Begin index is atthe doc beginning and moving backwards; rounded to avoid a too short part.
        self.get(
            f"/viewrange/get/{d2.id}/0/{backwards}",
            expect_content={"b": 0, "e": 2, "is_full": False},
        )
        self.json_post(
            url=f"/viewrange/set/piecesize",
            json_data={"pieceSize": 8},
            expect_cookie=("r", "8"),
            expect_status=200,
        )
        # Test rounding when remaining pars are shorter than half the piece size.
        self.get(
            f"/viewrange/get/{d2.id}/0/{forwards}",
            expect_content={"b": 0, "e": 10, "is_full": True},
        )

    def test_partitioning_document(self):
        self.login_test1()
        d = self.create_doc(
            initial_par=["1", "2", "3", "4", "5", "6", "7", "8", "9", "10"]
        )

        # No partitioning.
        tree = self.get(d.url, as_tree=True)
        self.assert_content(tree, ["1", "2", "3", "4", "5", "6", "7", "8", "9", "10"])

        self.json_post(
            url=f"/viewrange/set/piecesize",
            json_data={"pieceSize": 5},
            expect_cookie=("r", "5"),
            expect_status=200,
        )

        # Partitioning on, no URL parameters.
        tree = self.get(d.url, as_tree=True)
        self.assert_content(tree, ["1", "2", "3", "4", "5"])

        # Check ranges for navigation links:
        self.assert_js_variable(
            tree,
            "nav_ranges",
            [
                {"b": 0, "e": 5, "is_full": False, "name": "First"},
                {"b": 0, "e": 2, "is_full": False, "name": "Previous"},
                {"b": 5, "e": 10, "is_full": False, "name": "Next"},
                {"b": 5, "e": 10, "is_full": False, "name": "Last"},
            ],
        )

        # Partitioning with URL parameters, mid-document range.
        tree = self.get(d.url, query_string={"b": 2, "e": 6}, as_tree=True)
        self.assert_content(tree, ["3", "4", "5", "6"])

        # Check ranges for navigation links:
        self.assert_js_variable(
            tree,
            "nav_ranges",
            [
                {"b": 0, "e": 5, "is_full": False, "name": "First"},
                {"b": 0, "e": 4, "is_full": False, "name": "Previous"},
                {"b": 6, "e": 10, "is_full": False, "name": "Next"},
                {"b": 5, "e": 10, "is_full": False, "name": "Last"},
            ],
        )

        # Partitioning with URL parameters, whole document range.
        tree = self.get(d.url, query_string={"b": 0, "e": 10}, as_tree=True)
        self.assert_content(tree, ["1", "2", "3", "4", "5", "6", "7", "8", "9", "10"])

        # We're showing the full document, so there should be no part navigation.
        self.assert_js_variable(tree, "nav_ranges", [])

    def test_partitioning_document_with_overflowing_range(self):
        self.login_test1()
        d = self.create_doc(
            initial_par=["1", "Kissa", "3", "4", "5", "6", "Koira", "8", "9", "10"]
        )
        self.json_post(
            url=f"/viewrange/set/piecesize",
            json_data={"pieceSize": 5},
            expect_cookie=("r", "5"),
            expect_status=200,
        )

        # Overflowing range end.
        tree = self.get(d.url, query_string={"b": 6, "e": 100}, as_tree=True)
        self.assert_content(tree, ["Koira", "8", "9", "10"])

        # Negative range begin.
        tree = self.get(d.url, query_string={"b": -100, "e": 4}, as_tree=True)
        self.assert_content(tree, ["1", "Kissa", "3", "4"])

    def test_partitioning_empty_document(self):
        self.login_test1()
        self.json_post(
            url=f"/viewrange/set/piecesize",
            json_data={"pieceSize": 5},
            expect_cookie=("r", "5"),
            expect_status=200,
        )
        d = self.create_doc()

        tree = self.get(d.url, query_string={"b": 3, "e": 14}, as_tree=True)
        self.assert_content(tree, [])

    def test_partitioning_with_preambles(self):
        self.login_test1()
        self.json_post(
            url=f"/viewrange/set/piecesize",
            json_data={"pieceSize": 5},
            expect_cookie=("r", "5"),
            expect_status=200,
        )
        d = self.create_doc(
            initial_par=["1", "Kissa", "3", "4", "5", "6", "Koira", "8", "9", "10"]
        )
        self.create_preamble_for(d, initial_par=["Preamble par 1", "Preamble par 2"])

        # Partitioning on, no URL parameters.
        tree = self.get(d.url, as_tree=True)
        self.assert_content(
            tree, ["Preamble par 1", "Preamble par 2", "1", "Kissa", "3", "4", "5"]
        )

        # Partitioning with URL parameters, starting from doc beginning.
        tree = self.get(
            d.url, query_string={"b": 0, "e": 6, "preamble": "true"}, as_tree=True
        )
        self.assert_content(
            tree, ["Preamble par 1", "Preamble par 2", "1", "Kissa", "3", "4", "5", "6"]
        )

        # Partitioning with URL parameters, starting from mid-document.
        tree = self.get(
            d.url, query_string={"b": 6, "e": 9, "preamble": "true"}, as_tree=True
        )
        self.assert_content(tree, ["Koira", "8", "9"])

    def test_partitioning_with_special_class_preambles(self):
        self.login_test2()
        self.json_post(
            url=f"/viewrange/set/piecesize",
            json_data={"pieceSize": 5},
            expect_cookie=("r", "5"),
            expect_status=200,
        )
        d = self.create_doc(
            path=self.get_personal_item_path(f"2/test"),
            initial_par=["1", "Kissa", "3", "4", "5", "6", "Koira", "8", "9", "10"],
        )
        self.create_preamble_for(
            d,
            initial_par=[
                "Preamble par 1",
                "Preamble par 2",
                f"""
#- {{.{INCLUDE_IN_PARTS_CLASS_NAME}}}
Preamble par 3""",
            ],
        )
        # Partitioning on, no URL parameters; all pars should be included.
        tree = self.get(d.url, as_tree=True)
        self.assert_content(
            tree,
            [
                "Preamble par 1",
                "Preamble par 2",
                "Preamble par 3",
                "1",
                "Kissa",
                "3",
                "4",
                "5",
            ],
        )

        # Partitioning with URL parameters, starting from mid-document; special par should be included.
        tree = self.get(
            d.url, query_string={"b": 6, "e": 9, "preamble": "true"}, as_tree=True
        )
        self.assert_content(tree, ["Preamble par 3", "Koira", "8", "9"])

        # Preamble loading disabled.
        tree = self.get(
            d.url, query_string={"b": 5, "e": 8, "preamble": "false"}, as_tree=True
        )
        self.assert_content(tree, ["6", "Koira", "8"])

    def test_partitioning_document_areas(self):
        self.login_test1()
        self.json_post(
            url=f"/viewrange/set/piecesize",
            json_data={"pieceSize": 5},
            expect_cookie=("r", "5"),
            expect_status=200,
        )

        # Normal area within the document. The view range is expected to adjust avoid cutting the area.
        # Note: URL parameter ranges will take precedence and cut areas, so these tests are done without them.
        d = self.create_doc(
            initial_par="""
#-
1

#- {area="test"}

#-
Kissa

#-
3

#-
4

#-
5

#-
6

#-
Koira

#- {area_end="test"}

#-
8

#-
9

#-
10
"""
        )
        tree = self.get(d.url, as_tree=True)
        self.assert_content(tree, ["1", "", "Kissa", "3", "4", "5", "6", "Koira", ""])

        # Area inside area.
        d2 = self.create_doc(
            initial_par="""
#-
1

#- {area="test"}

#-
Kissa

#-
3

#- {area="test2"}

#-
4

#- {area_end="test2"}

#-
5

#-
6

#-
Koira

#- {area_end="test"}

#-
8

#-
9

#-
10
"""
        )
        self.assert_content(
            self.get(d2.url, as_tree=True),
            ["1", "", "Kissa", "3", "", "4", "", "5", "6", "Koira", ""],
        )
        # Whole document wide area.
        d3 = self.create_doc(
            initial_par="""
#- {area="test"}

#-
1

#-
2

#-
3

#-
4

#-
5

#-
6

#- {area_end="test"}
"""
        )
        self.assert_content(
            self.get(d3.url, as_tree=True), ["", "1", "2", "3", "4", "5", "6", ""]
        )

        # Two separate areas, the latter is partially within document part range.
        d3 = self.create_doc(
            initial_par="""
#-
1 

#- {area="test1"}

#-
2

#- {area_end="test1"}

#-
3

#- {area="test2"}

#-
4

#- {area_end="test2"}

#-
5

"""
        )
        self.assert_content(
            self.get(d3.url, as_tree=True), ["1", "", "2", "", "3", "", "4", ""]
        )

        # Empty area within document.
        d3 = self.create_doc(
            initial_par="""
#-
1 

#-
2

#-
3

#- {area="test"}

#- {area_end="test"}

#-
5

#-
6

#-
7
"""
        )
        self.assert_content(
            self.get(d3.url, as_tree=True),
            [
                "1",
                "2",
                "3",
                "",
                "",
            ],
        )

        # Broken area within document; handled like a normal par.
        d3 = self.create_doc(
            initial_par="""
#-
1 

#-
2

#-
3

#- {area="test"}

#-
5

#-
6

#-
7

#-
8
"""
        )
        self.assert_content(self.get(d3.url, as_tree=True), ["1", "2", "3", "", "5"])

    # TODO: Test areas + preambles & areas.

    def test_b_block_id(self):
        self.login_test1()
        d = self.create_doc(
            initial_par="""
#-
1

#-
2

#-
3

#-
4
        """
        )
        par_ids = [p.get_id() for p in d.document.get_paragraphs()]
        tree = self.get(
            d.url, query_string={"b": par_ids[1], "e": par_ids[3]}, as_tree=True
        )
        self.assert_content(tree, ["2", "3"])
        tree = self.get(
            d.url, query_string={"b": par_ids[1], "e": par_ids[3]}, as_tree=True
        )
        self.assert_content(tree, ["2", "3"])
        tree = self.get(
            d.url, query_string={"b": par_ids[0], "e": par_ids[3]}, as_tree=True
        )
        self.assert_content(tree, ["1", "2", "3"])
        tree = self.get(d.url, query_string={"e": par_ids[3]}, as_tree=True)
        self.assert_content(tree, ["1", "2", "3"])
        tree = self.get(d.url, query_string={"b": par_ids[1]}, as_tree=True)
        self.assert_content(tree, ["2", "3", "4"])
        tree = self.get(d.url, query_string={"b": par_ids[1], "size": 1}, as_tree=True)
        self.assert_content(tree, ["2"])
        tree = self.get(d.url, query_string={"b": par_ids[1], "size": 0}, as_tree=True)
        self.assert_content(tree, [])
        tree = self.get(d.url, query_string={"b": par_ids[1], "size": 10}, as_tree=True)
        self.assert_content(tree, ["2", "3", "4"])

    def test_settings_with_area(self):
        self.login_test1()
        d = self.create_doc(
            initial_par="""
#-
1

#- {area="24"}

#-
2

#- {visible="%% (False | isview) %%" nocache="true"}
3

#-
4

#- {area_end="24"}

#-
5
        """
        )
        tree = self.get(d.url, query_string={"area": "24"}, as_tree=True)
        # partitioning works normally right after document generation
        self.assert_content(tree, ["", "2", "4", ""])
        d.document.set_settings({"cache": "true"})
        clear_doc_cache(d, None)
        tree = self.get(d.url, query_string={"area": "24"}, as_tree=True)
        # partitioning works normally right after document settings are edited
        self.assert_content(tree, ["", "", "2", "4", ""])
        # partitioning works normally when loading document from cache
        tree = self.get(d.url, query_string={"area": "24"}, as_tree=True)
        self.assert_content(tree, ["", "", "2", "4", ""])
