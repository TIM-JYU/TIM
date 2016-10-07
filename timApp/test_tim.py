"""Unit tests for TIM routes."""
import re
import unittest

from flask import session

from documentmodel.document import Document
from markdownconverter import md_to_html
from timroutetest import TimRouteTest


class TimTest(TimRouteTest):
    doc_num = 1

    def test_activities(self):
        timdb = self.get_db()
        a = self.app

        login_resp = self.login_test1(force=True)
        self.assertDictEqual({'current_user': {'email': 'test1@example.com',
                                               'id': 4,
                                               'name': self.current_user_name(),
                                               'real_name': 'Test user 1'},
                              'other_users': []}, login_resp)

        # Make sure user's personal folder exists
        self.get('/view/users/' + self.current_user_name(), expect_status=200)

        doc_names = ['users/testuser1/testing',
                     'users/testuser1/testing2',
                     'users/testuser1/testing3',
                     'users/testuser1/testing4',
                     'users/testuser1/testing5']
        doc_name = doc_names[0]
        doc_id_list = [4, 5, 6, 7, 8]
        doc_id = doc_id_list[0]
        doc_ids = set()
        for idx, n in enumerate(doc_names):
            self.assertDictResponse({'id': doc_id_list[idx], 'name': doc_names[idx]},
                                    self.json_post('/createDocument', {
                                        'doc_name': n
                                    }))
            doc_ids.add(doc_id_list[idx])
        self.assertDictResponse(self.ok_resp,
                                self.json_put('/permissions/add/{}/{}/{}'.format(doc_id, 'Anonymous users', 'view')))
        self.assertDictResponse(self.ok_resp,
                                self.json_put('/permissions/add/{}/{}/{}'.format(doc_id_list[1], 'Logged-in users', 'view')))
        self.assertDictResponse(self.ok_resp, self.json_put('/permissions/add/{}/{}/{}'.format(doc_id_list[2], 'testuser2', 'view')))
        self.assertDictResponse(self.ok_resp, self.json_put('/permissions/add/{}/{}/{}'.format(doc_id_list[3], 'testuser2', 'edit')))
        doc = Document(doc_id)
        doc.add_paragraph('Hello')
        pars = doc.get_paragraphs()
        self.assertEqual(1, len(pars))
        first_id = pars[0].get_id()
        comment_of_test1 = 'This is a comment.'
        comment_of_test1_html = md_to_html(comment_of_test1)
        resp = self.json_post('/postNote', {'text': comment_of_test1,
                                            'access': 'everyone',
                                            'docId': doc_id,
                                            'par': first_id})
        json = self.assertResponseStatus(resp, expect_status=200, return_json=True)
        note_id = int(re.search(r'note-id="(\d+)"', json['texts']).groups()[0])

        self.assertTrue(comment_of_test1_html in json['texts'])
        self.assertInResponse(comment_of_test1_html, a.get('/view/' + doc_name))
        edited_comment = 'was edited!'
        edited_comment_html = md_to_html(edited_comment)
        resp = self.json_post('/editNote', {'id': note_id,
                                            'text': edited_comment,
                                            'access': 'everyone',
                                            'docId': doc_id,
                                            'par': first_id})
        json = self.assertResponseStatus(resp, expect_status=200, return_json=True)
        self.assertTrue(edited_comment_html in json['texts'])
        self.assertFalse(comment_of_test1_html in json['texts'])

        self.assertResponseStatus(a.get('/teacher/' + doc_name))
        self.assertResponseStatus(a.get('/answers/' + doc_name))
        edit_text = 'testing editing now...\nnew line\n'
        par_html = md_to_html(edit_text)
        self.assertInResponse(par_html, self.post_par(doc, edit_text, first_id))
        self.assertDictResponse({'text': edit_text}, a.get('/getBlock/{}/{}'.format(doc_id, first_id)))
        self.assertInResponse(par_html, self.post_par(doc, edit_text, first_id))
        par2_text = 'new par'
        par2_html = md_to_html(par2_text)
        self.assertManyInResponse([par_html, par2_html],
                                  self.post_par(doc, edit_text + '#-\n' + par2_text, first_id))
        pars = Document(doc_id).get_paragraphs()
        self.assertEqual(2, len(pars))
        second_par_id = pars[1].get_id()
        par2_new_text = '    ' + par2_text
        par2_new_html = md_to_html(par2_new_text)
        self.assertInResponse(par2_new_html, self.post_par(doc, par2_new_text, second_par_id))
        self.assertResponseStatus(a.post('/logout', follow_redirects=True))
        self.assertResponseStatus(a.get('/settings/'), 403)
        for d in doc_ids - {doc_id}:
            self.assertResponseStatus(a.get('/view/' + str(d)), 403)
        self.assertResponseStatus(a.get('/view/' + str(doc_id)))
        self.assertResponseStatus(a.get('/view/' + str(doc_id), query_string={'login': True}), 403)

        # Login as another user
        self.login_test2()
        view_resp = a.get('/view/' + doc_name)
        self.assertInResponse('Test user 2', view_resp)
        self.assertInResponse(edited_comment_html, view_resp)
        not_viewable_docs = {doc_id_list[4]}
        viewable_docs = doc_ids - not_viewable_docs
        for view_id in viewable_docs:
            self.assertResponseStatus(a.get('/view/' + str(view_id)))
            self.assertResponseStatus(a.get('/teacher/' + str(view_id)), 302)

        for view_id in not_viewable_docs:
            self.assertResponseStatus(a.get('/view/' + str(view_id)), 403)
            self.assertResponseStatus(a.get('/teacher/' + str(view_id)), 403)
        self.assertResponseStatus(a.get('/view/not_exist'), 404)

        comment_of_test2 = 'g8t54h954hy95hg54h'
        self.assertInResponse(comment_of_test2,
                              self.json_post('/postNote', {'text': comment_of_test2,
                                                           'access': 'everyone',
                                                           'docId': doc_id,
                                                           'par': first_id}))

        ug = timdb.users.get_personal_usergroup_by_id(session['user_id'])
        notes = timdb.notes.get_notes(ug, Document(doc_id), include_public=False)
        self.assertEqual(1, len(notes))
        test2_note_id = notes[0]['id']

        self.login_test1()
        self.assertInResponse(comment_of_test2,
                              a.get('/note/{}'.format(test2_note_id)))
        teacher_right_docs = {doc_id_list[3]}
        for i in teacher_right_docs:
            self.assertDictResponse(self.ok_resp,
                                    self.json_put('/permissions/add/{}/{}/{}'.format(i, 'testuser2', 'teacher')))

        self.assertResponseStatus(self.json_post('/deleteNote', {'id': test2_note_id,
                                                                 'docId': doc_id,
                                                                 'par': first_id}))
        ug = timdb.users.get_personal_usergroup_by_id(session['user_id'])
        notes = timdb.notes.get_notes(ug, Document(doc_id), include_public=True)
        self.assertEqual(1, len(notes))

        self.assertDictResponse({'text': edit_text}, self.json_req('/getBlock/{}/{}'.format(doc_id, first_id),
                                                                   {'docId': doc_id, 'par': first_id}))

        self.assertDictResponse({'text': Document(doc_id).export_section(first_id, second_par_id)},
                                a.get('/getBlock/{}/{}'.format(doc_id, first_id),
                                      query_string={'docId': doc_id,
                                                    'area_start': first_id,
                                                    'area_end': second_par_id}))

        self.login_test2()
        for view_id in viewable_docs - teacher_right_docs:
            self.assertResponseStatus(a.get('/view/' + str(view_id)))
            self.assertResponseStatus(a.get('/teacher/' + str(view_id)), 302)
            self.assertResponseStatus(self.json_put('/permissions/add/{}/{}/{}'.format(view_id, 'testuser2', 'teacher')),
                                      403)
        for view_id in teacher_right_docs:
            self.assertResponseStatus(a.get('/view/' + str(view_id)))
            self.assertResponseStatus(a.get('/teacher/' + str(view_id)))
            self.assertResponseStatus(self.json_put('/permissions/add/{}/{}/{}'.format(view_id, 'testuser2', 'teacher')),
                                      403)
        timdb.close()

    def test_macro_doc(self):
        self.login_test1()
        doc = self.create_doc(settings={'macro_delimiter': '%%', 'macros': {'rivi': 'kerros'}}).document
        table_text = """
{% set sarakeleveys = 50 %}
{% set sarakkeet = ['eka', 'toka', 'kolmas', 'neljäs'] %}
{% set rivit = ['eka', 'toka', 'kolmas', 'neljäs', 'viides'] %}

{% for x in sarakkeet %} %%'-'*sarakeleveys%% {% endfor %}

{% for r in rivit %}
{% for s in sarakkeet %}%%('{} {}, {} sarake').format(r,rivi,s).rjust(sarakeleveys)%% {% endfor %}

{% endfor %}
{% for x in sarakkeet %} %%'-'*sarakeleveys%% {% endfor %}
        """
        table_html = md_to_html(table_text, sanitize=True, macros={'rivi': 'kerros'}, macro_delimiter='%%')

        self.assertInResponse(table_html, self.new_par(doc, table_text), json_key='texts')
        self.assertInResponse(table_html, self.get('/view/{}'.format(doc.doc_id), as_response=True))

    def test_macro_only_delimiter(self):
        self.login_test1()
        doc = self.create_doc(settings={'macro_delimiter': '%%'}).document
        self.assertInResponse('123457', self.new_par(doc, '{% set a = 123456+1 %}%%a%%'), json_key='texts')

    def test_same_heading_as_par(self):
        self.login_test1()
        doc = self.create_doc(initial_par="""# Hello\n#-\nHello""").document
        self.get('/view/{}'.format(doc.doc_id), expect_status=200)

    def test_broken_comment(self):
        self.login_test1()
        doc = self.create_doc(settings={'macros': {}, 'macro_delimiter': '%%'},
                              initial_par="""```{atom=true}\nTest {!!! }\n```""").document
        tree = self.get('/view/{}'.format(doc.doc_id), as_tree=True)
        syntax_errors = tree.findall(r'.//div[@class="par"]/div[@class="parContent"]/div[@class="error"]')
        self.assertEqual(1, len(syntax_errors))
        self.assertIn('Syntax error in template:', syntax_errors[0].text)

    def test_windows_eol(self):
        """
        Windows-style EOLs should work with Dumbo. If this test fails, try to recompile Dumbo.
        """
        self.login_test1()
        md_table = """---\r\n|a|\r\n|-|"""
        doc = self.create_doc(initial_par=md_table).document
        tree = self.get('/view/{}'.format(doc.doc_id), as_tree=True)
        table_xpath = r'.//div[@class="par"]/div[@class="parContent"]/table'
        tables = tree.findall(table_xpath)
        self.assertEqual(1, len(tables))

        self.assertInResponse(table_xpath,
                              self.json_post('/preview/{}'.format(doc.doc_id), {'text': md_table}),
                              json_key='texts',
                              as_tree=True)

    def test_clear_cache(self):
        self.login_test1()
        doc = self.create_doc(initial_par="Test").document
        self.get('/view/{}'.format(doc.doc_id))
        self.get('/view/{}'.format(doc.doc_id), query_string={'nocache': 'true'})
        doc.get_index()

    def test_document_intermediate_folders(self):
        self.login_test1()
        self.create_doc('users/testuser1/a/b/c')


if __name__ == '__main__':
    unittest.main()
