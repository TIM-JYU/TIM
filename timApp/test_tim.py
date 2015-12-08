"""Unit tests for TIM routes."""

import json
import unittest

from flask import session
from lxml import html

import tim
from documentmodel.document import Document
from markdownconverter import md_to_html
from timdbtest import TimDbTest


def load_json(resp):
    return json.loads(resp.get_data(as_text=True))


class TimTest(TimDbTest):
    @classmethod
    def setUpClass(cls):
        TimDbTest.setUpClass()
        tim.app.config['DATABASE'] = cls.db_path
        tim.app.config['TESTING'] = True
        cls.app = tim.app.test_client()
        cls.app.jpost = lambda url, data=None: cls.json_post(cls.app, url, data)
        cls.app.jput = lambda url, data=None: cls.json_put(cls.app, url, data)
        cls.app.jreq = lambda url, data=None, method='GET': cls.json_req(cls.app, url, data, method)

    def assertResponseStatus(self, resp, expect_status=200):
        self.assertEqual(expect_status, resp.status_code, msg=resp.get_data(as_text=True))

    def assertInResponse(self, expected, resp, expect_status=200, json_key=None):
        resp_text = resp.get_data(as_text=True)
        self.assertResponseStatus(resp, expect_status)
        if json_key is not None:
            resp_text = json.loads(resp_text)[json_key]
        self.assertIn(expected, resp_text,
                      msg="""\n--------THIS TEXT:--------\n{}\n--------WAS NOT FOUND IN:---------\n{}""".format(
                          expected, resp_text))

    def assertManyInResponse(self, expecteds, resp, expect_status=200):
        for e in expecteds:
            self.assertInResponse(e, resp, expect_status)

    def assertResponse(self, expected, resp, expect_status=200):
        self.assertEqual(expect_status, resp.status_code)
        self.assertEqual(expected, resp.get_data(as_text=True))

    def assertDictResponse(self, expected, resp, expect_status=200):
        self.assertEqual(expect_status, resp.status_code)
        self.assertDictEqual(expected, load_json(resp))

    @staticmethod
    def json_put(app, url, json_data=None):
        return TimTest.json_req(app, url, json_data, 'PUT')

    @staticmethod
    def json_post(app, url, json_data=None):
        return TimTest.json_req(app, url, json_data, 'POST')

    @staticmethod
    def json_req(app, url, json_data=None, method='GET'):
        return app.open(url,
                        data=json.dumps(json_data),
                        content_type='application/json',
                        method=method)

    def test_activities(self):
        timdb = self.get_db()
        a = TimTest.app

        login_resp = self.login_test1()
        self.assertInResponse('Logged in as: Test user 1 (testuser1)', login_resp)
        doc_names = ['users/testuser1/testing',
                     'users/testuser1/testing2',
                     'users/testuser1/testing3',
                     'users/testuser1/testing4',
                     'users/testuser1/testing5']
        doc_name = doc_names[0]
        doc_id = 3
        doc_ids = set()
        for idx, n in enumerate(doc_names):
            self.assertDictResponse({'id': doc_id + idx, 'name': doc_names[idx]},
                                    a.jpost('/createDocument', {
                                        'doc_name': n
                                    }))
            doc_ids.add(doc_id + idx)
        self.assertResponse('Success', a.jput('/addPermission/{}/{}/{}'.format(3, 'Anonymous users', 'view')))
        self.assertResponse('Success', a.jput('/addPermission/{}/{}/{}'.format(4, 'Logged-in users', 'view')))
        self.assertResponse('Success', a.jput('/addPermission/{}/{}/{}'.format(5, 'testuser2', 'view')))
        self.assertResponse('Success', a.jput('/addPermission/{}/{}/{}'.format(6, 'testuser2', 'edit')))
        Document(doc_id).add_paragraph('Hello')
        pars = Document(doc_id).get_paragraphs()
        self.assertEqual(1, len(pars))
        first_id = pars[0].get_id()
        comment_of_test1 = 'This is a comment.'
        html_comment_of_test1 = md_to_html(comment_of_test1)
        self.assertInResponse(html_comment_of_test1,
                              a.jpost('/postNote', {'text': comment_of_test1,
                                                    'access': 'everyone',
                                                    'docId': doc_id,
                                                    'par': first_id}))
        self.assertInResponse(html_comment_of_test1, a.get('/view/' + doc_name))
        self.assertResponseStatus(a.get('/teacher/' + doc_name))
        edit_text = 'testing editing now...\nnew line\n'
        par_html = md_to_html(edit_text)
        self.assertInResponse(par_html, self.post_par(a, doc_id, edit_text, first_id))
        self.assertDictResponse({'text': edit_text}, a.get('/getBlock/{}/{}'.format(doc_id, first_id)))
        self.assertInResponse(par_html, self.post_par(a, doc_id, edit_text, first_id))
        par2_text = 'new par'
        par2_html = md_to_html(par2_text)
        self.assertManyInResponse([par_html, par2_html],
                                  self.post_par(a, doc_id, edit_text + '#-\n' + par2_text, first_id))
        pars = Document(doc_id).get_paragraphs()
        self.assertEqual(2, len(pars))
        second_par_id = pars[1].get_id()
        par2_new_text = '    ' + par2_text
        par2_new_html = md_to_html(par2_new_text)
        self.assertInResponse(par2_new_html, self.post_par(a, doc_id, par2_new_text, second_par_id))
        self.assertResponseStatus(a.post('/logout', follow_redirects=True))
        self.assertResponseStatus(a.get('/settings/'), 403)
        for d in doc_ids - {3}:
            self.assertResponseStatus(a.get('/view/' + str(d)), 403)
        self.assertResponseStatus(a.get('/view/' + str(3)))

        # Login as another user
        self.login_test2()
        view_resp = a.get('/view/' + doc_name)
        self.assertInResponse('Logged in as: Test user 2 (testuser2)', view_resp)
        self.assertInResponse(comment_of_test1, view_resp)
        not_viewable_docs = {7}
        viewable_docs = doc_ids - not_viewable_docs
        for view_id in viewable_docs:
            self.assertResponseStatus(a.get('/view/' + str(view_id)))
            self.assertResponseStatus(a.get('/teacher/' + doc_name), 403)

        for view_id in not_viewable_docs:
            self.assertResponseStatus(a.get('/view/' + str(view_id)), 403)
            self.assertResponseStatus(a.get('/teacher/' + doc_name), 403)
        self.assertResponseStatus(a.get('/view/1'), 404)

        with a as a:
            comment_of_test2 = 'g8t54h954hy95hg54h'
            self.assertInResponse(comment_of_test2,
                                  a.jpost('/postNote', {'text': comment_of_test2,
                                                        'access': 'everyone',
                                                        'docId': doc_id,
                                                        'par': first_id}))

            ug = timdb.users.getPersonalUserGroup(session['user_id'])
            notes = timdb.notes.getNotes(ug, Document(doc_id), include_public=False)
            self.assertEqual(1, len(notes))
            test2_note_id = notes[0]['id']

        self.login_test1()
        self.assertInResponse(comment_of_test2,
                              a.get('/note/{}'.format(test2_note_id)))
        teacher_right_docs = {6}
        for i in teacher_right_docs:
            self.assertResponse('Success', a.jput('/addPermission/{}/{}/{}'.format(i, 'testuser2', 'teacher')))

        with a as a:
            self.assertResponseStatus(a.jpost('/deleteNote', {'id': test2_note_id,
                                                              'docId': doc_id,
                                                              'par': first_id}))
            ug = timdb.users.getPersonalUserGroup(session['user_id'])
            notes = timdb.notes.getNotes(ug, Document(doc_id), include_public=True)
            self.assertEqual(1, len(notes))

        self.assertDictResponse({'text': edit_text}, a.jreq('/getBlock/{}/{}'.format(doc_id, first_id),
                                                            {'docId': doc_id, 'par': first_id}))

        self.assertDictResponse({'text': Document(doc_id).export_section(first_id, second_par_id)},
                                a.get('/getBlock/{}/{}'.format(doc_id, first_id),
                                      query_string={'docId': doc_id,
                                                    'area_start': first_id,
                                                    'area_end': second_par_id}))

        self.login_test2()
        for view_id in viewable_docs - teacher_right_docs:
            self.assertResponseStatus(a.get('/view/' + str(view_id)))
            self.assertResponseStatus(a.get('/teacher/' + str(view_id)), 403)
            self.assertResponseStatus(a.jput('/addPermission/{}/{}/{}'.format(view_id, 'testuser2', 'teacher')), 403)
        for view_id in teacher_right_docs:
            self.assertResponseStatus(a.get('/view/' + str(view_id)))
            self.assertResponseStatus(a.get('/teacher/' + str(view_id)))
            self.assertResponseStatus(a.jput('/addPermission/{}/{}/{}'.format(view_id, 'testuser2', 'teacher')), 403)

    def post_par(self, a, doc_id, text, par_id):
        return a.jpost('/postParagraph/', {
            "text": text,
            "docId": doc_id,
            "par": par_id,
            "par_next": None
        })

    def new_par(self, doc_id, text, next_id=None):
        return TimTest.app.jpost('/newParagraph/', {
            "text": text,
            "docId": doc_id,
            "par_next": next_id
        })

    def login_test1(self):
        return TimTest.app.post('/altlogin',
                                data={'email': 'test1@example.com', 'password': 'test1pass'},
                                follow_redirects=True)

    def login_test2(self):
        return TimTest.app.post('/altlogin',
                                data={'email': 'test2@example.com', 'password': 'test2pass'},
                                follow_redirects=True)

    def test_macro_doc(self):
        a = TimTest.app
        self.login_test1()
        docname = 'users/testuser1/macrotest'
        doc = self.create_doc(docname)
        self.new_par(doc.doc_id, """
``` {settings=""}
macro_delimiter: "%%"
macros:
 rivi: kerros
```
""")
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

        self.assertInResponse(table_html, self.new_par(doc.doc_id, table_text), json_key='texts')
        self.assertInResponse(table_html, a.get('/view/' + docname))

    def test_index_one_heading_per_par(self):
        self.login_test1()
        doc = self.create_doc('users/testuser1/indextest', """
``` {settings=""}
macro_delimiter: "%%"
macros:
 rivi: kerros
```
#-
# Heading level 1
Lorem ipsum.

---

#-
## Heading level 2
#-
### Heading level 3
#-
# Second heading level 1
        """)
        self.assertEqual([({'id': 'heading-level-1', 'level': 1, 'text': 'Heading level 1'},
                           [{'id': 'heading-level-2', 'level': 2, 'text': 'Heading level 2'},
                            {'id': 'heading-level-3', 'level': 3, 'text': 'Heading level 3'}]),
                          ({'id': 'second-heading-level-1', 'level': 1, 'text': 'Second heading level 1'},
                           [])], doc.get_index())
        doc = self.create_doc('users/testuser1/indextest5', """
``` {settings=""}
auto_number_headings: true
```
#-
# Heading level 1
Lorem ipsum.

---

```
# Not a header
```

#-
# Unnumbered {.nonumber}
#-
## Heading level 2
#-
## Second heading level 2
#-
### Heading level 3
#-
# Second heading level 1
        """)
        self.assertEqual([({'id': 'heading-level-1', 'level': 1, 'text': '1. Heading level 1'}, []),
                          ({'id': 'unnumbered', 'level': 1, 'text': 'Unnumbered'},
                           [{'id': 'heading-level-2', 'level': 2, 'text': '1.1. Heading level 2'},
                            {'id': 'second-heading-level-2', 'level': 2, 'text': '1.2. Second heading level 2'},
                            {'id': 'heading-level-3', 'level': 3, 'text': '1.2.1. Heading level 3'}]),
                          ({'id': 'second-heading-level-1', 'level': 1, 'text': '2. Second heading level 1'},
                           [])], doc.get_index())

    def test_index_many_headings_per_par(self):
        self.login_test1()
        doc = self.create_doc('users/testuser1/indextest2', """
# Heading level 1
Lorem ipsum.

---

## Heading level 2

## Unnumbered {.nonumber}

### Heading level 3
#-
# Second heading level 1
        """)
        self.assertEqual([({'id': 'heading-level-1', 'level': 1, 'text': 'Heading level 1'},
                           [{'id': 'heading-level-2', 'level': 2, 'text': 'Heading level 2'},
                            {'id': 'unnumbered', 'level': 2, 'text': 'Unnumbered'},
                            {'id': 'heading-level-3', 'level': 3, 'text': 'Heading level 3'}]),
                          ({'id': 'second-heading-level-1', 'level': 1, 'text': 'Second heading level 1'},
                           [])], doc.get_index())

    def test_index_duplicate_headings(self):
        self.login_test1()
        doc = self.create_doc('users/testuser1/indextest3', """
# Same

# Same
        """)
        self.assertEqual([({'id': 'same', 'level': 1, 'text': 'Same'}, []),
                          ({'id': 'same-1', 'level': 1, 'text': 'Same'}, [])], doc.get_index())

        doc = self.create_doc('users/testuser1/indextest4', """
# Same
#-
# Same
        """)
        self.assertEqual([({'id': 'same', 'level': 1, 'text': 'Same'}, []),
                          ({'id': 'same-1', 'level': 1, 'text': 'Same'}, [])], doc.get_index())

    def test_plugin(self):
        self.login_test1()
        docname = 'users/testuser1/plugintest'
        with open('example_docs/mmcq_example.md', encoding='utf-8') as f:
            doc = self.create_doc(docname, f.read())
        resp = TimTest.app.get('/view/' + docname)
        self.assertResponseStatus(resp)
        ht = resp.get_data(as_text=True)
        tree = html.fromstring(ht)
        plugs = tree.findall(r'.//div[@class="par mmcq"]/div[@class="parContent"]/div[@id="{}.mmcqexample"]'.format(doc.doc_id))
        self.assertEqual(1, len(plugs))

    def create_doc(self, docname, initial_par=None, assert_status=200):
        resp = TimTest.app.jpost('/createDocument', {
            'doc_name': docname
        })
        self.assertResponseStatus(resp, assert_status)
        resp_data = load_json(resp)
        doc = Document(resp_data['id'])
        if initial_par is not None:
            self.new_par(doc.doc_id, initial_par)
        return doc


if __name__ == '__main__':
    unittest.main()
