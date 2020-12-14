"""Server tests for document caching."""
from unittest.mock import patch, Mock

from timApp.auth.accesstype import AccessType
from timApp.item.routes import render_doc_view
from timApp.tests.server.timroutetest import TimRouteTest, get_note_id_from_json
from timApp.item import routes
from timApp.timdb.sqa import db


class CachingTest(TimRouteTest):
    def test_cache(self):
        self.login_test1()
        d = self.create_doc(initial_par='#- {plugin=textfield #t}')
        self.test_user_2.grant_access(d, AccessType.view)
        db.session.commit()
        self.check_not_cached(d)
        self.check_not_cached(d)  # cache is disabled by default
        d.document.set_settings({'cache': True})
        self.check_not_cached(d)  # was not in cache, added to cache
        self.check_is_cached(d)  # was in cache, fetched from cache
        self.check_is_cached(d)  # still in cache
        self.check_is_cached(d, {'unlock': True})  # value of unlock shouldn't affect caching
        self.check_not_cached(d, {'nocache': True})  # nocache should clear the cache
        self.check_not_cached(d, {'nocache': True})  # make sure nocache is not cached
        self.check_is_cached(d)
        self.post_answer('textfield', f'{d.id}.t', user_input={'c': 'x'})
        self.check_not_cached_and_then_cached(d)
        par = d.document.get_paragraphs()[0]
        self.mark_as_read(d, par.get_id())
        self.check_not_cached_and_then_cached(d)

        self.login_test2()
        self.check_not_cached_and_then_cached(d)

        self.login_test1()
        c = self.post_comment(par, public=False, text='test')
        nid = get_note_id_from_json(c)
        self.check_not_cached_and_then_cached(d)

        self.login_test2()
        self.check_is_cached(d)

        self.login_test1()
        self.edit_comment(nid, True, 'test2')
        self.check_not_cached_and_then_cached(d)

        self.login_test2()
        self.check_not_cached_and_then_cached(d)

        self.login_test1()
        self.edit_comment(nid, False, 'test2')
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
        with patch.object(routes, render_doc_view.__name__, wraps=render_doc_view) as m:  # type: Mock
            self.get(d.url, query_string=query)
        return m
