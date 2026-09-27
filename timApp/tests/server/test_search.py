from timApp.auth.accesstype import AccessType
from timApp.item.tag import TagType
from timApp.tests.server.timroutetest import TimRouteTest
from timApp.timdb.sqa import db


class SearchTest(TimRouteTest):
    date_re = r"\d{4}-\d{2}-\d{2}T\d{2}:\d{2}:\d{2}\+\d{2}:\d{2}"

    def test_search(self):
        u = self.test_user_1
        self.make_admin(u)
        u_name = u.name
        self.login_test1()
        text_to_search = "cat"
        text_in_document = "House cats like to hunt too."
        d = self.create_doc(initial_par=text_in_document)
        self.get(f"search/createContentFile")
        url = f"search?ignoreRelevance=true&caseSensitive=false&folder=&ignorePlugins=false&query={text_to_search}&regex=false&searchContent=true&searchTitles=true&searchPaths=true&searchTags=true"

        response_json = str(self.get(url, expect_status=200))
        self.assertRegex(
            response_json,
            rf"""
{{
    'content_results': \[
        {{
            'doc': {{
                'name': '{d.short_name}', 
                'path': '{d.path}', 
                'title': '{d.title}', 
                'location': '{d.location}', 
                'id': {d.id}, 
                'modified': 'just now', 
                'modifiedTimeFull': '{self.date_re}', 
                'owners': \[
                    {{'id': {self.get_test_user_1_group_id()}, 'name': '{u.name}'}}
                \], 
                'rights': {{
                    'editable': True, 
                    'can_mark_as_read': True, 
                    'can_comment': True, 
                    'copy': True, 
                    'browse_own_answers': True, 
                    'teacher': True, 
                    'see_answers': True, 
                    'manage': True, 
                    'owner': True
                }}, 
                'unpublished': True, 
                'public': True, 
                'relevance': None, 
                'isFolder': False
            }}, 
            'incomplete': False, 
            'title_results': \[\], 
            'num_title_results': 0, 
            'tag_results': \[\], 
            'num_tag_results': 0, 
            'path_results': \[\], 
            'num_path_results': 0, 
            'par_results': \[
                {{
                    'par_id': '{d.document.get_paragraphs()[0].get_id()}', 
                    'preview': 'House cats like to hunt too.', 
                    'results': \[\], 
                    'num_results': 1
                }}
            \], 
            'num_par_results': 1
        }}
    \], 
    'errors': \[\], 
    'incomplete_search_reason': '', 
    'paths_result_count': 0, 
    'paths_results': \[\], 
    'tags_result_count': 0, 
    'tags_results': \[\], 
    'title_result_count': 0, 
    'title_results': \[\], 
    'word_result_count': 1
}}
""".replace(
                "\n", ""
            ).replace(
                "    ", ""
            ),
        )

    def test_too_short_search(self):
        u = self.test_user_1
        self.make_admin(u)
        self.login_test1()
        self.create_doc(initial_par="There's a match inside, but it can't be searched.")
        text_to_search = "a"
        self.get(f"search/createContentFile")
        url = f"search?folder=&query={text_to_search}&searchContent=true&searchTitles=true&searchPaths=true&searchTags=true"
        self.get(
            url,
            expect_status=400,
            expect_content={
                "error": "Search text must be at least 3 character(s) long with whitespace stripped."
            },
        )

    def test_not_found_search(self):
        u = self.test_user_1
        self.make_admin(u)
        self.login_test1()
        self.create_doc(
            initial_par="I contain plenty of text but not the one you are searching!"
        )
        self.get(f"search/createContentFile")
        text_to_search = "Cannot be found anywhere"
        url = f"search?folder=&query={text_to_search}&searchContent=true&searchTitles=true&searchPaths=true&searchTags=true"
        self.get(
            url,
            expect_status=200,
            expect_content={
                "content_results": [],
                "incomplete_search_reason": "",
                "errors": [],
                "title_results": [],
                "tags_results": [],
                "paths_results": [],
                "title_result_count": 0,
                "word_result_count": 0,
                "tags_result_count": 0,
                "paths_result_count": 0,
            },
        )

    def test_case_sensitive_search(self):
        u = self.test_user_1
        self.make_admin(u)
        self.login_test1()
        text_to_search = "Text to search"
        case_sensitive = True
        self.create_doc(initial_par=text_to_search)
        self.get(f"search/createContentFile")
        text_to_search_upper = "TEXT TO SEARCH"
        url = f"search?caseSensitive={case_sensitive}&folder=&query={text_to_search_upper}&searchContent=true&searchTitles=true&searchPaths=true&searchTags=true"
        self.get(
            url,
            expect_status=200,
            expect_content={
                "content_results": [],
                "incomplete_search_reason": "",
                "errors": [],
                "title_results": [],
                "tags_results": [],
                "paths_results": [],
                "title_result_count": 0,
                "word_result_count": 0,
                "tags_result_count": 0,
                "paths_result_count": 0,
            },
        )

    def test_search_without_view_rights(self):
        text_to_search = "secret"
        url = f"search?folder=&query={text_to_search}&searchContent=true&searchTitles=true&searchPaths=true&searchTags=true"

        self.make_admin(self.test_user_1)
        self.login_test1()
        d = self.create_doc(initial_par="Super secret things.")
        self.get(f"search/createContentFile")

        self.login_test2()
        self.test_user_2.remove_access(d.id, "view")
        self.get(
            url,
            expect_status=200,
            expect_content={
                "content_results": [],
                "incomplete_search_reason": "",
                "errors": [],
                "title_results": [],
                "title_result_count": 0,
                "word_result_count": 0,
                "paths_result_count": 0,
                "paths_results": [],
                "tags_result_count": 0,
                "tags_results": [],
            },
        )

    def test_search_visible(self):
        """
        Tests for searching conditionally visible par.
        :return:
        """
        text_to_search = "hidden"
        url = f"search?folder=&query={text_to_search}&searchContent=true"
        u1 = self.test_user_1
        u1_name = u1.name
        self.login_test1()
        d = self.create_doc(
            initial_par="""
        #- {visible="%%username in ['akuankka']%%"} hidden text
        """
        )
        self.get(f"search/createContentFile")
        # User is the doc owner, search results from paragraphs with visibility-condition are always shown.
        response_json = str(self.get(url, expect_status=200))
        self.assertRegex(
            response_json,
            rf"""
{{
    'content_results': \[
        {{
            'doc': {{
                'name': '{d.short_name}', 
                'path': '{d.path}', 
                'title': '{d.title}', 
                'location': '{d.location}', 
                'id': {d.id}, 
                'modified': 'just now', 
                'modifiedTimeFull': '{self.date_re}', 
                'owners': \[
                    {{'id': {self.get_test_user_1_group_id()}, 'name': '{u1_name}'}}
                \], 
                'rights': {{
                    'editable': True, 
                    'can_mark_as_read': True, 
                    'can_comment': True, 
                    'copy': True, 
                    'browse_own_answers': True, 
                    'teacher': True, 
                    'see_answers': True, 
                    'manage': True, 
                    'owner': True
                }}, 
                'unpublished': True, 
                'public': True, 
                'relevance': None, 
                'isFolder': False
            }}, 
            'incomplete': False, 
            'title_results': \[\], 
            'num_title_results': 0, 
            'tag_results': \[\], 
            'num_tag_results': 0, 
            'path_results': \[\], 
            'num_path_results': 0, 
            'par_results': \[
                {{
                    'par_id': '{d.document.get_paragraphs()[0].get_id()}', 
                    'preview': '\.\.\.visible="%%username in \[\\'akuankka\\'\]%%"}} hidden text', 
                    'results': \[\], 
                    'num_results': 1
                }}
            \], 
            'num_par_results': 1
        }}
    \], 
    'errors': \[\], 
    'incomplete_search_reason': '', 
    'paths_result_count': 0, 
    'paths_results': \[\], 
    'tags_result_count': 0, 
    'tags_results': \[\], 
    'title_result_count': 0, 
    'title_results': \[\], 
    'word_result_count': 1
}}
""".replace(
                "\n", ""
            ).replace(
                "    ", ""
            ),
        )
        self.login_test2()
        # User is not the doc owner, paragraphs with visibility-condition are always skipped.
        self.get(
            url,
            expect_status=200,
            expect_content={
                "content_results": [],
                "errors": [],
                "incomplete_search_reason": "",
                "title_result_count": 0,
                "title_results": [],
                "word_result_count": 0,
                "paths_result_count": 0,
                "paths_results": [],
                "tags_result_count": 0,
                "tags_results": [],
            },
        )

    def test_search_plugin(self):
        text_to_search = "answer"
        plugin_md = """``` {plugin="test"}
        question: What cats like the most?
        answer: Catnip.
        ```"""
        self.make_admin(self.test_user_1)
        self.login_test1()
        d = self.create_doc(initial_par=plugin_md)
        self.get(f"search/createContentFile")
        self.test_user_1.grant_access(d, AccessType.edit)
        db.session.commit()
        response_json = str(
            self.get(
                f"search?ignoreRelevance=true&folder=&query={text_to_search}&searchContent=true",
                expect_status=200,
            )
        )
        self.assertRegex(
            response_json.replace("    ", ""),
            rf"""
{{
    'content_results': \[
        {{
            'doc': {{
                'name': '{d.short_name}', 
                'path': '{d.path}', 
                'title': '{d.title}', 
                'location': '{d.location}', 
                'id': {d.id}, 
                'modified': 'just now', 
                'modifiedTimeFull': '{self.date_re}', 
                'owners': \[
                    {{
                        'id': {self.get_test_user_1_group_id()}, 
                        'name': '{self.test_user_1.name}'
                    }}
                \], 
                'rights': {{
                    'editable': True, 
                    'can_mark_as_read': True, 
                    'can_comment': True, 
                    'copy': True, 
                    'browse_own_answers': True, 
                    'teacher': True, 
                    'see_answers': True, 
                    'manage': True, 
                    'owner': True
                }}, 
                'unpublished': False, 
                'public': True, 
                'relevance': None, 
                'isFolder': False
            }}, 
            'incomplete': False, 
            'title_results': \[\], 
            'num_title_results': 0, 
            'tag_results': \[\], 
            'num_tag_results': 0, 
            'path_results': \[\], 
            'num_path_results': 0, 
            'par_results': \[
                {{
                    'par_id': '{d.document.get_paragraphs()[0].get_id()}', 
                    'preview': '\.\.\.stion: What cats like the most\?         answer: Catnip\.         ``` ```', 
                    'results': \[\], 
                    'num_results': 1
                }}
            \], 
            'num_par_results': 1
        }}
    \], 
    'errors': \[\], 
    'incomplete_search_reason': '', 
    'paths_result_count': 0, 
    'paths_results': \[\], 
    'tags_result_count': 0, 
    'tags_results': \[\], 
    'title_result_count': 0, 
    'title_results': \[\], 
    'word_result_count': 1
}}
""".replace(
                "\n", ""
            ).replace(
                "    ", ""
            ),
        )

        self.get(
            f"search?folder=&query={text_to_search}&ignorePlugins=True&searchContent=true",
            expect_status=200,
            expect_content={
                "content_results": [],
                "errors": [],
                "incomplete_search_reason": "",
                "title_result_count": 0,
                "title_results": [],
                "word_result_count": 0,
                "paths_result_count": 0,
                "paths_results": [],
                "tags_result_count": 0,
                "tags_results": [],
            },
        )

        self.login_test2()
        self.test_user_2.grant_access(d, AccessType.view)
        db.session.commit()
        self.get(
            f"search?folder=&query={text_to_search}&searchContent=true",
            expect_status=200,
            expect_content={
                "content_results": [],
                "errors": [],
                "incomplete_search_reason": "",
                "title_result_count": 0,
                "title_results": [],
                "word_result_count": 0,
                "paths_result_count": 0,
                "paths_results": [],
                "tags_result_count": 0,
                "tags_results": [],
            },
        )

    def test_title_search(self):
        self.make_admin(self.test_user_1)
        self.login_test1()
        search_word = "Some title"
        d = self.create_doc(
            title=search_word,
            initial_par="I cannot be found without some par content here.",
        )
        self.get(f"search/createContentFile")
        url = f"search?ignoreRelevance=true&folder=&query={search_word}&searchContent=false&searchTitles=true"

        response_json = str(self.get(url, expect_status=200))
        self.assertRegex(
            response_json,
            rf"""
{{
    'content_results': \[\], 
    'errors': \[\], 
    'incomplete_search_reason': '', 
    'paths_result_count': 0, 
    'paths_results': \[\], 
    'tags_result_count': 0, 
    'tags_results': \[\], 
    'title_result_count': 1, 
    'title_results': \[
        {{
            'doc': {{
                'name': '{d.short_name}', 
                'path': '{d.path}', 
                'title': '{d.title}', 
                'location': '{d.location}', 
                'id': {d.id}, 
                'modified': 'just now', 
                'modifiedTimeFull': '{self.date_re}', 
                'owners': \[
                    {{
                        'id': {self.get_test_user_1_group_id()}, 
                        'name': '{self.test_user_1.name}'
                    }}
                \], 
                'rights': {{
                    'editable': True, 
                    'can_mark_as_read': True, 
                    'can_comment': True, 
                    'copy': True, 
                    'browse_own_answers': True, 
                    'teacher': True, 
                    'see_answers': True, 
                    'manage': True, 
                    'owner': True
                }}, 
                'unpublished': True, 
                'public': True, 
                'relevance': None, 
                'isFolder': False
            }}, 
            'incomplete': False, 
            'title_results': \[{{'results': \[\], 'num_results': 1}}\], 
            'num_title_results': 1, 
            'tag_results': \[\], 
            'num_tag_results': 0, 
            'path_results': \[\], 
            'num_path_results': 0, 
            'par_results': \[\], 
            'num_par_results': 0
        }}
    \], 
    'word_result_count': 0
}}
""".replace(
                "\n", ""
            ).replace(
                "    ", ""
            ),
        )

    def test_tag_search(self):
        self.make_admin(self.test_user_1)
        u = self.test_user_1
        self.login_test1()
        d = self.create_doc(initial_par="Tag test")
        tags = ["dog", "dog2", "cat"]
        self.json_post(
            f"/tags/add/{d.path}",
            {
                "tags": [
                    {"name": tags[0], "expires": None, "type": TagType.Regular},
                    {"name": tags[1], "expires": None, "type": TagType.Regular},
                    {"name": tags[2], "expires": None, "type": TagType.Regular},
                ]
            },
        )
        self.get(f"search/createContentFile")
        tag_to_search = "dog"
        url = f"search?caseSensitive=true&folder=&query={tag_to_search}&regex=false&searchContent=false&searchTitles=false&searchTags=true&searchPaths=false"

        response_json = str(self.get(url, expect_status=200))
        self.assertRegex(
            response_json,
            rf"""
{{
    'content_results': \[\], 
    'errors': \[\], 
    'incomplete_search_reason': '', 
    'paths_result_count': 0, 
    'paths_results': \[\], 
    'tags_result_count': 2, 
    'tags_results': \[
        {{
            'doc': {{
                'name': '{d.short_name}', 
                'path': '{d.path}', 
                'title': '{d.title}', 
                'location': '{d.location}', 
                'id': {d.id}, 
                'modified': 'just now', 
                'modifiedTimeFull': '{self.date_re}', 
                'owners': \[
                    {{'id': {self.get_test_user_1_group_id()}, 'name': '{u.name}'}}
                \], 
                'rights': {{
                    'editable': True, 
                    'can_mark_as_read': True, 
                    'can_comment': True, 
                    'copy': True, 
                    'browse_own_answers': True, 
                    'teacher': True, 
                    'see_answers': True, 
                    'manage': True, 
                    'owner': True
                }}, 
                'unpublished': True, 
                'public': True, 
                'relevance': None, 
                'isFolder': False
            }}, 
            'incomplete': False, 
            'title_results': \[\], 
            'num_title_results': 0, 
            'tag_results': \[{{'results': \[\], 'num_results': 2}}\], 
            'num_tag_results': 2, 
            'path_results': \[\], 
            'num_path_results': 0, 
            'par_results': \[\], 
            'num_par_results': 0
        }}
    \], 
    'title_result_count': 0, 
    'title_results': \[\], 
    'word_result_count': 0
}}
""".replace(
                "\n", ""
            ).replace(
                "    ", ""
            ),
        )

    def test_path_search(self):
        self.make_admin(self.test_user_1)
        u = self.test_user_1
        self.login_test1()
        search_word = "dog"
        uf = f"{self.current_user.get_personal_folder().path}"
        d = self.create_doc(initial_par="Test", path=f"{uf}/a/b/dogs-like-bones/c/d")
        self.get(f"search/createContentFile")
        url = f"search?folder=&query={search_word}&regex=false&searchContent=false&searchTitles=false&searchTags=false&searchPaths=true"

        response_json = str(self.get(url, expect_status=200))
        self.assertRegex(
            response_json,
            rf"""
{{
    'content_results': \[\], 
    'errors': \[\], 
    'incomplete_search_reason': '', 
    'paths_result_count': 1, 
    'paths_results': \[
        {{
            'doc': {{
                'name': '{d.short_name}', 
                'path': '{d.path}', 
                'title': '{d.title}', 
                'location': '{d.location}', 
                'id': {d.id}, 
                'modified': 'just now', 
                'modifiedTimeFull': '{self.date_re}', 
                'owners': \[
                    {{'id': {self.get_test_user_1_group_id()}, 'name': '{u.name}'}}
                \], 
                'rights': {{
                    'editable': True, 
                    'can_mark_as_read': True, 
                    'can_comment': True, 
                    'copy': True, 
                    'browse_own_answers': True, 
                    'teacher': True, 
                    'see_answers': True, 
                    'manage': True, 
                    'owner': True
                }}, 
                'unpublished': True, 
                'public': True, 
                'relevance': None, 
                'isFolder': False
            }}, 
            'incomplete': False, 
            'title_results': \[\], 
            'num_title_results': 0, 
            'tag_results': \[\], 
            'num_tag_results': 0, 
            'path_results': \[{{'results': \[\], 'num_results': 1}}\], 
            'num_path_results': 1, 
            'par_results': \[\], 
            'num_par_results': 0
        }}
    \], 
    'tags_result_count': 0, 
    'tags_results': \[\], 
    'title_result_count': 0, 
    'title_results': \[\], 
    'word_result_count': 0
}}
""".replace(
                "\n", ""
            ).replace(
                "    ", ""
            ),
        )
        url = f"search?folder=&query={search_word}&searchWholeWords=True&searchContent=false&searchTitles=false&searchTags=false&searchPaths=true"
        self.get(
            url,
            expect_status=200,
            expect_content={
                "content_results": [],
                "errors": [],
                "incomplete_search_reason": "",
                "paths_result_count": 0,
                "paths_results": [],
                "tags_result_count": 0,
                "tags_results": [],
                "title_result_count": 0,
                "title_results": [],
                "word_result_count": 0,
            },
        )
        search_word_2 = "dogs"
        url = f"search?folder=&query={search_word_2}&searchWholeWords=True&searchContent=false&searchTitles=false&searchTags=false&searchPaths=true"

        response_json2 = str(self.get(url, expect_status=200))
        self.assertRegex(
            response_json2,
            rf"""
{{
    'content_results': \[\], 
    'errors': \[\], 
    'incomplete_search_reason': '', 
    'paths_result_count': 1, 
    'paths_results': \[
        {{
            'doc': {{
                'name': '{d.short_name}', 
                'path': '{d.path}', 
                'title': '{d.title}', 
                'location': '{d.location}', 
                'id': {d.id}, 
                'modified': 'just now', 
                'modifiedTimeFull': '{self.date_re}', 
                'owners': \[
                    {{'id': {self.get_test_user_1_group_id()}, 'name': '{u.name}'}}
                \], 
                'rights': {{
                    'editable': True, 
                    'can_mark_as_read': True, 
                    'can_comment': True, 
                    'copy': True, 
                    'browse_own_answers': True, 
                    'teacher': True, 
                    'see_answers': True, 
                    'manage': True, 
                    'owner': True
                }}, 
                'unpublished': True, 
                'public': True, 
                'relevance': None, 
                'isFolder': False
            }}, 
            'incomplete': False, 
            'title_results': \[\], 
            'num_title_results': 0, 
            'tag_results': \[\], 
            'num_tag_results': 0, 
            'path_results': \[{{'results': \[\], 'num_results': 1}}\], 
            'num_path_results': 1, 
            'par_results': \[\], 
            'num_par_results': 0
        }}
    \], 
    'tags_result_count': 0, 
    'tags_results': \[\], 
    'title_result_count': 0, 
    'title_results': \[\], 
    'word_result_count': 0
}}
""".replace(
                "\n", ""
            ).replace(
                "    ", ""
            ),
        )
