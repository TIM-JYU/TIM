from timApp.tests.server.timroutetest import TimRouteTest


class SearchTest(TimRouteTest):

    def test_search(self):
        u = self.test_user_1
        self.login_test1()
        text_to_search = 'cat'
        text_in_document = 'House cats like to hunt too.'
        d = self.create_doc(initial_par=text_in_document)
        url = f'search?caseSensitive=false&folder=&ignorePluginsSettings=false&onlyfirst=999' \
              f'&query={text_to_search}&regex=false&searchDocNames=false&searchWords=true'
        self.get(url, expect_status=200, expect_content=[{'doc': {'id': d.id,
                                                                  'isFolder': False,
                                                                  'location': d.location,
                                                                  'modified': 'just now',
                                                                  'name': d.short_name,
                                                                  'owner': {'id': self.get_test_user_1_group_id(),
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
                                                          'in_title': False,
                                                          'match_end_index': 9,
                                                          'match_start_index': 6,
                                                          'match_word': text_to_search,
                                                          'num_pars': 1,
                                                          'num_pars_found': 1,
                                                          'num_results': 1,
                                                          'par': {'attrs': {},
                                                                  'html': f'<p>{text_in_document}</p>',
                                                                  'id': '02rrzIY0IaHV',
                                                                  'md': text_in_document,
                                                                  't': 'MHgzMTA3NTA0Zg=='}}])

    def test_too_short_search(self):
        self.login_test1()
        self.create_doc(initial_par="There's a match inside, but it can't be searched.")
        text_to_search = 'a'
        url = f'search?caseSensitive=false&folder=&ignorePluginsSettings=false&onlyfirst=999' \
              f'&query={text_to_search}&regex=false&searchDocNames=false&searchWords=true'
        self.get(url, expect_status=400,
                 expect_content={'error': 'Search text must be at least 3 characters long with whitespace stripped.'})

    def test_not_found_search(self):
        self.login_test1()
        text_to_search = 'Another text to search'
        url = f'search?caseSensitive=false&folder=&ignorePluginsSettings=false&onlyfirst=999' \
              f'&query={text_to_search}&regex=false&searchDocNames=false&searchWords=true'
        self.get(url, expect_status=200, expect_content=[])

    def test_case_sensitive_search(self):
        self.login_test1()
        text_to_search = 'Text to search'
        case_sensitive = True
        self.create_doc(initial_par=text_to_search)
        text_to_search_upper = 'TEXT TO SEARCH'
        url = f'search?caseSensitive={case_sensitive}&folder=&ignorePluginsSettings=false&onlyfirst=999' \
              f'&query={text_to_search_upper}&regex=false&searchDocNames=false&searchWords=true'
        self.get(url, expect_status=200, expect_content=[])
