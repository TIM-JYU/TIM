"""Unit tests for TIM routes."""
import re
import unittest

from lxml.cssselect import CSSSelector

from timApp.documentmodel.document import Document
from timApp.markdownconverter import md_to_html
from timApp.tests.db.timdbtest import TEST_USER_1_NAME
from timApp.tests.server.timroutetest import TimRouteTest
from timApp.timdb.userutils import get_anon_group_id, grant_view_access

link_selector = CSSSelector('a')


class TimTest(TimRouteTest):

    def test_activities(self):
        timdb = self.get_db()

        login_resp = self.login_test1(force=True)
        self.assertDictEqual({'current_user': {'email': 'test1@example.com',
                                               'id': 4,
                                               'name': self.current_user_name(),
                                               'real_name': TEST_USER_1_NAME},
                              'other_users': []}, login_resp)

        # Make sure user's personal folder exists
        personal_folder = self.current_user.get_personal_folder().path
        self.get(f'/view/{personal_folder}')

        doc_names = [personal_folder + '/testing',
                     personal_folder + '/testing2',
                     personal_folder + '/testing3',
                     personal_folder + '/testing4',
                     personal_folder + '/testing5']
        doc_name = doc_names[0]
        doc_id_list = [4, 6, 7, 8, 9]
        doc_id = doc_id_list[0]
        doc_ids = set()
        for idx, n in enumerate(doc_names):
            self.json_post('/createItem', {
                'item_path': n,
                'item_type': 'document',
                'item_title': 'document ' + n
            }, expect_contains={'id': doc_id_list[idx], 'path': doc_names[idx]})
            doc_ids.add(doc_id_list[idx])
        self.json_put(f'/permissions/add/{doc_id}/{"Anonymous users"}/{"view"}',
                      {'type': 'always'}, expect_content=self.ok_resp)
        self.json_put(
            f'/permissions/add/{doc_id_list[1]}/{"Logged-in users"}/{"view"}', {'type': 'always'}, expect_content=self.ok_resp)
        self.json_put(
            f'/permissions/add/{doc_id_list[2]}/{"testuser2"}/{"view"}', {'type': 'always'}, expect_content=self.ok_resp)
        self.json_put(
            f'/permissions/add/{doc_id_list[3]}/{"testuser2"}/{"edit"}', {'type': 'always'}, expect_content=self.ok_resp)
        doc = Document(doc_id)
        doc.add_paragraph('Hello')
        pars = doc.get_paragraphs()
        self.assertEqual(1, len(pars))
        first_id = pars[0].get_id()
        comment_of_test1 = 'This is a comment.'
        comment_of_test1_html = md_to_html(comment_of_test1)
        json = self.json_post('/postNote', {'text': comment_of_test1,
                                            'access': 'everyone',
                                            'docId': doc_id,
                                            'par': first_id})
        note_id = int(re.search(r'note-id="(\d+)"', json['texts']).groups()[0])

        self.assertTrue(comment_of_test1_html in json['texts'])
        self.get(f'/view/{doc_name}', expect_contains=comment_of_test1_html)
        edited_comment = 'was edited!'
        edited_comment_html = md_to_html(edited_comment)
        json = self.json_post('/editNote', {'id': note_id,
                                            'text': edited_comment,
                                            'access': 'everyone',
                                            'docId': doc_id,
                                            'par': first_id})
        self.assertTrue(edited_comment_html in json['texts'])
        self.assertFalse(comment_of_test1_html in json['texts'])

        self.get('/teacher/' + doc_name)
        self.get('/answers/' + doc_name)
        edit_text = 'testing editing now...\nnew line\n'
        par_html = md_to_html(edit_text)
        self.post_par(doc, edit_text, first_id, expect_contains=par_html, json_key='texts')
        self.get(f'/getBlock/{doc_id}/{first_id}', expect_content={'text': edit_text})
        self.post_par(doc, edit_text, first_id, expect_contains=par_html, json_key='texts')
        par2_text = 'new par'
        par2_html = md_to_html(par2_text)
        self.post_par(doc, edit_text + '#-\n' + par2_text, first_id,
                      expect_contains=[par_html, par2_html], json_key='texts')
        pars = Document(doc_id).get_paragraphs()
        self.assertEqual(2, len(pars))
        second_par_id = pars[1].get_id()
        par2_new_text = '    ' + par2_text
        par2_new_html = md_to_html(par2_new_text)
        self.post_par(doc, par2_new_text, second_par_id, expect_contains=par2_new_html, json_key='texts')
        self.post('/logout', follow_redirects=True)
        self.get('/settings', expect_status=403)
        for d in doc_ids - {doc_id}:
            self.get(f'/view/{d}', expect_status=403)
        self.get(f'/view/{doc_id}')
        self.get(f'/view/{doc_id}', query_string={'login': True}, expect_status=403)

        # Login as another user
        self.login_test2()
        self.get(f'/view/{doc_name}', expect_contains=['Test user 2', edited_comment_html])
        not_viewable_docs = {doc_id_list[4]}
        viewable_docs = doc_ids - not_viewable_docs
        for view_id in viewable_docs:
            self.get(f'/view/{view_id}')
            self.get(f'/teacher/{view_id}', expect_status=302)

        for view_id in not_viewable_docs:
            self.get(f'/view/{view_id}', expect_status=403)
            self.get(f'/teacher/{view_id}', expect_status=403)
        self.get('/view/not_exist', expect_status=404)

        comment_of_test2 = 'g8t54h954hy95hg54h'
        self.json_post('/postNote', {'text': comment_of_test2,
                                     'access': 'everyone',
                                     'docId': doc_id,
                                     'par': first_id}, expect_contains=comment_of_test2, json_key='texts')

        ug = self.current_group().id
        notes = timdb.notes.get_notes(ug, Document(doc_id), include_public=False)
        self.assertEqual(1, len(notes))
        test2_note_id = notes[0]['id']

        self.login_test1()
        self.get(f'/note/{test2_note_id}', expect_contains=comment_of_test2, json_key='text')
        teacher_right_docs = {doc_id_list[3]}
        for i in teacher_right_docs:
            self.json_put(f'/permissions/add/{i}/{"testuser2"}/{"teacher"}',
                          {'type': 'always'}, expect_content=self.ok_resp)

        self.json_post('/deleteNote', {'id': test2_note_id,
                                       'docId': doc_id,
                                       'par': first_id})
        ug = self.current_group().id
        notes = timdb.notes.get_notes(ug, Document(doc_id), include_public=True)
        self.assertEqual(1, len(notes))

        self.get(f'/getBlock/{doc_id}/{first_id}',
                 expect_content={'text': edit_text})

        self.get(f'/getBlock/{doc_id}/{first_id}',
                 query_string={'docId': doc_id,
                               'area_start': first_id,
                               'area_end': second_par_id},
                 expect_content={'text': Document(doc_id).export_section(first_id, second_par_id)})

        self.login_test2()
        for view_id in viewable_docs - teacher_right_docs:
            self.get(f'/view/{view_id}')
            self.get('/teacher/' + str(view_id), expect_status=302)
            self.json_put(f'/permissions/add/{view_id}/{"testuser2"}/{"teacher"}', {'type': 'always'}, expect_status=403)
        for view_id in teacher_right_docs:
            self.get(f'/view/{view_id}')
            self.get(f'/teacher/{view_id}')
            self.json_put(f'/permissions/add/{view_id}/{"testuser2"}/{"teacher"}', {'type': 'always'}, expect_status=403)

    def test_same_heading_as_par(self):
        self.login_test1()
        doc = self.create_doc(initial_par=['# Hello', 'Hello']).document
        self.get(f'/view/{doc.doc_id}')

    def test_broken_comment(self):
        self.login_test1()
        doc = self.create_doc(settings={'macros': {}, 'macro_delimiter': '%%'},
                              initial_par="""```{atom=true}\nTest {!!! }\n```""").document
        tree = self.get(f'/view/{doc.doc_id}', as_tree=True)
        syntax_errors = tree.findall(r'.//div[@class="par"]/div[@class="parContent"]/div[@class="error"]')
        self.assertEqual(1, len(syntax_errors))
        self.assertIn('Syntax error in template:', syntax_errors[0].text)

    def test_windows_eol(self):
        """Windows-style EOLs should work with Dumbo.
        """
        self.login_test1()
        md_table = """---\r\n|a|\r\n|-|"""
        doc = self.create_doc(initial_par=md_table).document
        tree = self.get(f'/view/{doc.doc_id}', as_tree=True)
        table_xpath = r'.//div[@class="par"]/div[@class="parContent"]/table'
        tables = tree.findall(table_xpath)
        self.assertEqual(1, len(tables))

        self.json_post(f'/preview/{doc.doc_id}', {'text': md_table}, json_key='texts', expect_xpath=table_xpath)

    def test_clear_cache(self):
        self.login_test1()
        doc = self.create_doc(initial_par="Test").document
        self.get(f'/view/{doc.doc_id}')
        self.get(f'/view/{doc.doc_id}', query_string={'nocache': 'true'})
        doc.get_index()

    def test_document_intermediate_folders(self):
        self.login_test1()
        self.create_doc('users/test-user-1/a/b/c')

    def test_hide_links(self):
        self.login_test1()
        doc = self.create_doc()
        grant_view_access(get_anon_group_id(), doc.id)
        links = link_selector(self.get(f'/view/{doc.id}', as_tree=True))
        self.assertGreater(len(links), 0)

        self.logout()
        links = link_selector(self.get(f'/view/{doc.id}', as_tree=True))
        self.assertGreater(len(links), 0)
        doc.document.add_setting('hide_links', 'view')
        links = link_selector(self.get(f'/view/{doc.id}', as_tree=True))
        self.assertEqual(len(links), 0)
        doc.document.add_paragraph(text='# 1\n\n#2')

        # Index is visible always
        links = link_selector(self.get(f'/view/{doc.id}', as_tree=True))
        self.assertEqual(len(links), 3)

        self.login_test1()
        links = link_selector(self.get(f'/view/{doc.id}', as_tree=True))
        self.assertGreater(len(links), 0)

    def test_teacher(self):
        self.login_test1()
        d = self.create_doc()
        self.get(f'/teacher/{d.path}', query_string={'group': 'Korppi users'})


if __name__ == '__main__':
    unittest.main()
