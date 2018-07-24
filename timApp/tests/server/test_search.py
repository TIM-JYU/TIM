from timApp.item.tag import TagType
from timApp.tests.server.timroutetest import TimRouteTest


class SearchTest(TimRouteTest):

    def test_search(self):
        u = self.test_user_1
        self.login_test1()
        text_to_search = 'cat'
        text_in_document = 'House cats like to hunt too.'
        d = self.create_doc(initial_par=text_in_document)
        url = f'search?caseSensitive=false&folder=&ignorePluginsSettings=false&maxDocPars=999' \
              f'&query={text_to_search}&regex=false&searchDocNames=false&searchWords=true'
        self.get(url, expect_status=200, expect_content={'complete': True,
                                                         'errors': [],
                                                         'results': [{'doc': {'id': d.id,
                                                                              'isFolder': False,
                                                                              'location': d.location,
                                                                              'modified': 'just now',
                                                                              'name': d.short_name,
                                                                              'owner': {
                                                                                  'id': self.get_test_user_1_group_id(),
                                                                                  'name': u.name},
                                                                              'path': d.path,
                                                                              'public': True,
                                                                              'rights': {'browse_own_answers': True,
                                                                                         'can_comment': True,
                                                                                         'can_mark_as_read': True,
                                                                                         'editable': True,
                                                                                         'manage': True,
                                                                                         'owner': True,
                                                                                         'see_answers': True,
                                                                                         'teacher': True},
                                                                              'title': d.title,
                                                                              'unpublished': True},
                                                                      'num_par_results': 1,
                                                                      'num_title_results': 0,
                                                                      'par_results': [{'num_results': 1,
                                                                                       'par_id': '02rrzIY0IaHV',
                                                                                       'preview': 'House cats like to hunt too....',
                                                                                       'results': [{'match_end': 9,
                                                                                                    'match_start': 6,
                                                                                                    'match_word': 'cat'}]}],
                                                                      'title_results': []}],
                                                         'titleResultCount': 0,
                                                         'wordResultCount': 1})

    def test_too_short_search(self):
        self.login_test1()
        self.create_doc(initial_par="There's a match inside, but it can't be searched.")
        text_to_search = 'a'
        url = f'search?caseSensitive=false&folder=&ignorePluginsSettings=false&maxDocPars=999' \
              f'&query={text_to_search}&regex=false&searchDocNames=false&searchWords=true'
        self.get(url, expect_status=400,
                 expect_content={'error': 'Search text must be at least 3 character(s) long with whitespace stripped.'})

    def test_not_found_search(self):
        self.login_test1()
        text_to_search = 'Another text to search'
        url = f'search?caseSensitive=false&folder=&ignorePluginsSettings=false&maxDocPars=999' \
              f'&query={text_to_search}&regex=false&searchDocNames=false&searchWords=true'
        self.get(url, expect_status=200, expect_content={'complete': True,
                                                         'errors': [],
                                                         'results': [],
                                                         'titleResultCount': 0,
                                                         'wordResultCount': 0})

    def test_case_sensitive_search(self):
        self.login_test1()
        text_to_search = 'Text to search'
        case_sensitive = True
        self.create_doc(initial_par=text_to_search)
        text_to_search_upper = 'TEXT TO SEARCH'
        url = f'search?caseSensitive={case_sensitive}&folder=&ignorePluginsSettings=false&maxDocPars=999' \
              f'&query={text_to_search_upper}&regex=false&searchDocNames=false&searchWords=true'
        self.get(url, expect_status=200, expect_content={'complete': True,
                                                         'errors': [],
                                                         'results': [],
                                                         'titleResultCount': 0,
                                                         'wordResultCount': 0})

    def test_whole_word_search(self):
        u = self.test_user_1
        self.login_test1()
        text = 'Cats are intensely attracted to catnip. For a cat it can be irresistible.'
        text_to_search = 'cat'
        whole_words = True
        d = self.create_doc(initial_par=text)
        url = f'search?caseSensitive=false&folder=&ignorePluginsSettings=false&maxDocPars=999&' \
              f'query={text_to_search}&searchExactWords={whole_words}&regex=false&searchDocNames=false&' \
              f'searchWords=true'
        self.get(url, expect_status=200, expect_content={'complete': True,
                                                         'errors': [],
                                                         'results': [{'doc': {'id': d.id,
                                                                              'isFolder': False,
                                                                              'location': d.location,
                                                                              'modified': 'just now',
                                                                              'name': d.short_name,
                                                                              'owner': {
                                                                                  'id': self.get_test_user_1_group_id(),
                                                                                  'name': u.name},
                                                                              'path': d.path,
                                                                              'public': True,
                                                                              'rights': {'browse_own_answers': True,
                                                                                         'can_comment': True,
                                                                                         'can_mark_as_read': True,
                                                                                         'editable': True,
                                                                                         'manage': True,
                                                                                         'owner': True,
                                                                                         'see_answers': True,
                                                                                         'teacher': True},
                                                                              'title': d.title,
                                                                              'unpublished': True},
                                                                      'num_par_results': 1,
                                                                      'num_title_results': 0,
                                                                      'par_results': [{'num_results': 1,
                                                                                       'par_id': 'DiJPVjF2AdjH',
                                                                                       'preview': '...are intensely attracted to '
                                                                                                  'catnip. For a cat it can be '
                                                                                                  'irresistible....',
                                                                                       'results': [{'match_end': 50,
                                                                                                    'match_start': 45,
                                                                                                    'match_word': ' cat '}]}],
                                                                      'title_results': []}],
                                                         'titleResultCount': 0,
                                                         'wordResultCount': 1})

    def test_title_search(self):
        u = self.test_user_1
        self.login_test1()
        d = self.create_doc()
        search_titles = True
        text_to_search = d.title
        url = f'search?caseSensitive=false&folder=&ignorePluginsSettings=false&maxDocPars=1000' \
              f'&query={text_to_search}&regex=false&searchDocNames={search_titles}&searchWords=false'
        self.get(url, expect_status=200, expect_content={'complete': True,
                                                         'errors': [],
                                                         'results': [{'doc': {'id': d.id,
                                                                              'isFolder': False,
                                                                              'location': d.location,
                                                                              'modified': 'just now',
                                                                              'name': d.short_name,
                                                                              'owner': {
                                                                                  'id': self.get_test_user_1_group_id(),
                                                                                  'name': u.name},
                                                                              'path': d.path,
                                                                              'public': True,
                                                                              'rights': {'browse_own_answers': True,
                                                                                         'can_comment': True,
                                                                                         'can_mark_as_read': True,
                                                                                         'editable': True,
                                                                                         'manage': True,
                                                                                         'owner': True,
                                                                                         'see_answers': True,
                                                                                         'teacher': True},
                                                                              'title': d.title,
                                                                              'unpublished': True},
                                                                      'num_par_results': 0,
                                                                      'num_title_results': 1,
                                                                      'par_results': [],
                                                                      'title_results': [{'num_results': 1,
                                                                                         'results': [
                                                                                             {'match_end': len(d.title),
                                                                                              'match_start': 0,
                                                                                              'match_word': d.title}]}]}],
                                                         'titleResultCount': 1,
                                                         'wordResultCount': 0})

    def test_tag_search(self):
        u = self.test_user_1
        self.login_test1()
        d = self.create_doc()
        tags = ["dog", "dog2", "cat"]
        self.json_post(f'/tags/add/{d.path}', {'tags': [{'name': tags[0], 'expires': None, 'type': TagType.Regular},
                                                        {'name': tags[1], 'expires': None, 'type': TagType.Regular},
                                                        {'name': tags[2], 'expires': None, 'type': TagType.Regular}]})

        tag_to_search = 'dog'
        url = f'search/tags?caseSensitive=true&folder=&query={tag_to_search}&regex=false'
        self.get(url, expect_status=200, expect_content={'complete': True,
                                                         'errors': [],
                                                         'results': [{'doc': {'id': d.id,
                                                                              'isFolder': False,
                                                                              'location': d.location,
                                                                              'modified': 'just now',
                                                                              'name': d.short_name,
                                                                              'owner': {
                                                                                  'id': self.get_test_user_1_group_id(),
                                                                                  'name': u.name},
                                                                              'path': d.path,
                                                                              'public': True,
                                                                              'rights': {'browse_own_answers': True,
                                                                                         'can_comment': True,
                                                                                         'can_mark_as_read': True,
                                                                                         'editable': True,
                                                                                         'manage': True,
                                                                                         'owner': True,
                                                                                         'see_answers': True,
                                                                                         'teacher': True},
                                                                              'tags': [{'block_id': d.id,
                                                                                        'expires': None,
                                                                                        'name': tags[0],
                                                                                        'type': TagType.Regular.value},
                                                                                       {'block_id': d.id,
                                                                                        'expires': None,
                                                                                        'name': tags[1],
                                                                                        'type': TagType.Regular.value},
                                                                                       {'block_id': d.id,
                                                                                        'expires': None,
                                                                                        'name': tags[2],
                                                                                        'type': TagType.Regular.value}],
                                                                              'title': 'document 4',
                                                                              'unpublished': True},
                                                                      'matching_tags': [{'block_id': d.id,
                                                                                         'expires': None,
                                                                                         'name': tags[0],
                                                                                         'type': TagType.Regular.value},
                                                                                        {'block_id': d.id,
                                                                                         'expires': None,
                                                                                         'name': tags[1],
                                                                                         'type': TagType.Regular.value}],
                                                                      'num_results': 2}],
                                                         'tagResultCount': 2})
