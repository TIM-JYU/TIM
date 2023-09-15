"""Server tests for document caching."""
import re
from unittest.mock import patch, Mock

from timApp.auth.accesstype import AccessType
from timApp.document.caching import clear_doc_cache
from timApp.document.docentry import DocEntry
from timApp.item import routes
from timApp.item.routes import render_doc_view
from timApp.tests.server.timroutetest import TimRouteTest, get_note_id_from_json
from timApp.user.usergroup import UserGroup
from timApp.user.userutils import grant_access


class CachingTest(TimRouteTest):
    def test_cache(self):
        self.login_test1()
        d = self.create_doc(initial_par="#- {plugin=textfield #t}")
        self.test_user_2.grant_access(d, AccessType.view)
        self.test_user_3.grant_access(d, AccessType.view)
        self.commit_db()
        clear_doc_cache(d, None)
        self.login_test3()
        self.check_not_cached(d)
        self.check_not_cached(d)  # cache is disabled by default
        d.document.set_settings({"cache": True})
        self.check_not_cached(d)  # was not in cache, added to cache

        def normalize(s: str) -> str:
            # Remove modified date as it can be slightly different depending on CI speed
            return re.sub(r'"modified": "[^"]+"', r'"modified": "X"', s)

        last = normalize(self.last_get)
        self.check_is_cached(d)  # was in cache, fetched from cache

        # Cached result should be the same as not cached.
        self.assertEqual(last, normalize(self.last_get))

        self.check_is_cached(d)  # still in cache
        self.check_is_cached(
            d, {"unlock": True}
        )  # value of unlock shouldn't affect caching
        self.check_not_cached(d, {"nocache": True})  # nocache should clear the cache
        self.check_not_cached(d, {"nocache": True})  # make sure nocache is not cached
        self.check_is_cached(d)
        self.post_answer("textfield", f"{d.id}.t", user_input={"c": "x"})
        self.check_not_cached_and_then_cached(d)
        par = d.document.get_paragraphs()[0]
        self.mark_as_read(d, par.get_id())
        self.check_not_cached_and_then_cached(d)

        self.login_test2()
        self.check_not_cached_and_then_cached(d)

        self.login_test3()
        c = self.post_comment(par, public=False, text="test")
        nid = get_note_id_from_json(c)
        self.check_not_cached_and_then_cached(d)

        self.login_test2()
        self.check_is_cached(d)

        self.login_test3()
        self.edit_comment(nid, par, True, "test2")
        self.check_not_cached_and_then_cached(d)

        self.login_test2()
        self.check_not_cached_and_then_cached(d)

        self.login_test3()
        self.edit_comment(nid, par, False, "test2")
        self.check_not_cached_and_then_cached(d)

        self.login_test2()
        self.check_not_cached_and_then_cached(d)

    def check_not_cached_and_then_cached(self, d, query=None):
        self.check_not_cached(d, query)
        self.check_is_cached(d, query)

    def check_is_cached(self, d, query=None):
        self.get_with_patch(d, query).assert_not_called()

    def check_not_cached(self, d, query=None):
        self.get_with_patch(d, query).assert_called_once()

    def get_with_patch(self, d, query=None):
        with patch.object(
            routes, render_doc_view.__name__, wraps=render_doc_view
        ) as m:  # type: Mock
            self.last_get = self.get(d.url, query_string=query)
        return m

    def test_cache_pregenerate(self):
        self.login_test1()
        d = self.create_doc(initial_par="test")
        self.test_user_2.grant_access(d, AccessType.view)
        self.commit_db()
        clear_doc_cache(d, None)
        self.get(
            f"/generateCache/{d.path}",
            expect_status=400,
            expect_content="Document does not have caching enabled.",
        )
        d.document.set_settings({"cache": True})
        self.get(
            f"/generateCache/{d.path}",
            expect_content="""
1/2 testuser1: ok
2/2 testuser2: ok
        """.strip()
            + "\n",
        )
        self.get(
            f"/generateCache/{d.path}",
            expect_content="""
1/2 testuser1: already cached
2/2 testuser2: already cached
                """.strip()
            + "\n",
        )
        self.check_is_cached(d)
        self.get(
            f"/generateCache/{d.path}",
            # TODO enable this test. We need two users with only view right.
            #  testuser1 and 2 have different rights so there will be differences in the HTML.
            # query_string={'print_diffs': True},
            expect_content="""
1/2 testuser1: already cached
2/2 testuser2: already cached

                        """.strip()
            + "\n",
        )

        ug = UserGroup.create("testgroup1")
        self.test_user_3.add_to_group(ug, added_by=None)
        grant_access(ug, d, AccessType.view)
        self.commit_db()
        d = DocEntry.find_by_id(d.id)
        self.get(
            f"/generateCache/{d.path}",
            expect_status=403,
            expect_content="No access for group testgroup1",
        )

        # When running all server tests in IDE (PyCharm), the tests start failing in test_clipboard with
        # sqlalchemy.orm.exc.FlushError in initialize_database. Refreshing the test client prevents it.
        # The line self.test_user_3.add_to_group seems to trigger the error.
        self.refresh_client()

    def test_cache_generate_exam_mode(self):
        self.login_test1()
        d = self.create_doc(settings={"exam_mode": "view", "cache": True})

        # The nocache is just for making sure there's no previous doc cache when rerunning this same test from IDE.
        # The doc id is always the same because the DB starts fresh.
        self.get(d.url, query_string={"nocache": True})

        self.test_user_2.grant_access(d, AccessType.view)
        self.commit_db()
        self.get(
            f"/generateCache/{d.path}",
        )
        self.login_test2()
        r = self.get(d.url, as_tree=True)
        self.assert_js_variable(r, "exam_mode", True)
        rights = self.get_js_variable(r, "curr_item")["rights"]
        self.assertEqual(
            {
                "browse_own_answers": True,
                "can_comment": True,
                "can_mark_as_read": True,
                "copy": False,
                "editable": False,
                "manage": False,
                "owner": False,
                "see_answers": False,
                "teacher": False,
            },
            rights,
        )
        rights = self.get_js_variable(r, "translations")[0]["rights"]
        self.assertEqual(
            {
                "browse_own_answers": True,
                "can_comment": True,
                "can_mark_as_read": True,
                "copy": False,
                "editable": False,
                "manage": False,
                "owner": False,
                "see_answers": False,
                "teacher": False,
            },
            rights,
        )
