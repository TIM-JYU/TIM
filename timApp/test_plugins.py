from datetime import timezone, datetime, timedelta
import json
import dateutil.parser

import io

import re
from flask import session
from lxml import html

from timroutetest import TimRouteTest

TEST_USER_1_ID = 4
TEST_USER_2_ID = 5

class PluginTest(TimRouteTest):
    def post_answer(self, plugin_type, task_id, user_input,
                    save_teacher=False, teacher=False, user_id=None, answer_id=None):
        return self.json_put('/{}/{}/answer/'.format(plugin_type, task_id),
                             {"input": user_input,
                              "abData": {"saveTeacher": save_teacher,
                                         "teacher": teacher,
                                         "userId": user_id,
                                         "answer_id": answer_id,
                                         "saveAnswer": True}})

    def test_plugin(self):
        self.login_test1()
        doc = self.create_doc(from_file='example_docs/mmcq_example.md')
        resp = self.get('/view/{}'.format(doc.doc_id), expect_status=200)
        tree = html.fromstring(resp)
        mmcq_xpath = r'.//div[@class="par mmcq"]/div[@class="parContent"]/div[@id="{}.mmcqexample.{}"]'.format(
            doc.doc_id, doc.get_paragraphs()[0].get_id())
        plugs = tree.findall(mmcq_xpath)
        self.assertEqual(1, len(plugs))
        task_name = 'mmcqexample'
        plugin_type = 'mmcq'
        task_id = '{}.{}'.format(doc.doc_id, task_name)
        par_id = doc.get_paragraph_by_task(task_name).get_id()
        task_id_ext = task_id + '.' + par_id
        task_id_ext_wrong = task_id + '.' + par_id + 'x'

        resp = self.post_answer(plugin_type, task_id, [True, False, False])
        self.check_ok_answer(resp)

        resp = self.post_answer(plugin_type, task_id, [True, False, False])
        self.check_failed_answer(resp)
        resp = self.post_answer(plugin_type, task_id_ext, [True, False, False])
        self.check_failed_answer(resp)
        resp = self.post_answer(plugin_type, task_id_ext_wrong, [True, False, False])
        self.assertDictResponse({'error': 'Document {}: Paragraph not found: {}'
                                .format(doc.doc_id, par_id + 'x')}, resp, expect_status=400)

        wrongname = 'mmcqexamplez'
        resp = self.post_answer(plugin_type, str(doc.doc_id) + '.' + wrongname, [True, False, False])
        self.assertInResponse('Task not found in the document: {}'.format(wrongname), resp, 400, json_key='error')

        doc.set_settings({'global_plugin_attrs': {'all': {'answerLimit': 2}}})
        resp = self.post_answer(plugin_type, task_id, [True, True, False])
        self.check_ok_answer(resp)

        resp = self.post_answer(plugin_type, task_id, [True, False, False])
        self.check_failed_answer(resp, is_new=True)

        doc.set_settings({'global_plugin_attrs': {'mmcq': {'answerLimit': None}}})
        resp = self.post_answer(plugin_type, task_id, [True, True, True])
        self.check_ok_answer(resp)
        resp = self.post_answer(plugin_type, task_id, [True, True, True])
        self.check_ok_answer(resp, is_new=False)

        doc.set_settings({'global_plugin_attrs': {'mmcq': {'answerLimit': None, 'pointsRule': {'multiplier': 0}}}})
        resp = self.post_answer(plugin_type, task_id, [False, False, True])
        self.check_ok_answer(resp)

        doc.set_settings({'global_plugin_attrs': {'mmcq': {'answerLimit': None, 'pointsRule': {'multiplier': 3}}}})
        resp = self.post_answer(plugin_type, task_id, [True, False, True])
        self.check_ok_answer(resp)

        resp = self.json_req('/answers/{}/{}'.format(task_id, session['user_id']))
        answer_list = self.assertResponseStatus(resp, expect_status=200, return_json=True)
        self.maxDiff = None

        self.assertListEqual(
            [{'collaborators': [{'real_name': 'Test user 1', 'user_id': TEST_USER_1_ID}], 'content': '[true, false, true]',
               'points': 9.0, 'task_id': task_id, 'valid': True, 'last_points_modifier': None},
             {'collaborators': [{'real_name': 'Test user 1', 'user_id': TEST_USER_1_ID}], 'content': '[false, false, true]',
               'points': None, 'task_id': task_id, 'valid': True, 'last_points_modifier': None},
             {'collaborators': [{'real_name': 'Test user 1', 'user_id': TEST_USER_1_ID}], 'content': '[true, true, true]',
               'points': 2.0, 'task_id': task_id, 'valid': True, 'last_points_modifier': None},
             {'collaborators': [{'real_name': 'Test user 1', 'user_id': TEST_USER_1_ID}], 'content': '[true, false, false]',
               'points': 2.0, 'task_id': task_id, 'valid': False, 'last_points_modifier': None},
             {'collaborators': [{'real_name': 'Test user 1', 'user_id': TEST_USER_1_ID}], 'content': '[true, true, false]',
               'points': 1.0, 'task_id': task_id, 'valid': True, 'last_points_modifier': None},
             {'collaborators': [{'real_name': 'Test user 1', 'user_id': TEST_USER_1_ID}], 'content': '[true, false, false]',
               'points': 2.0, 'task_id': task_id, 'valid': True, 'last_points_modifier': None}],
            [{k: v for k, v in ans.items() if k not in ('answered_on', 'id')} for ans in answer_list])
        for ans in answer_list:
            d = dateutil.parser.parse(ans['answered_on'])
            self.assertLess(d - datetime.now(tz=timezone.utc), timedelta(seconds=5))

        resp = self.post_answer(plugin_type, task_id, [True, True, False],
                                save_teacher=False, teacher=True, answer_id=answer_list[0]['id'],
                                user_id=session['user_id'] - 1)
        self.assertDictResponse({'error': 'userId is not associated with answer_id'}, resp, expect_status=400)

        resp = self.post_answer(plugin_type, task_id, [False, False, False],
                                save_teacher=False, teacher=True, answer_id=answer_list[0]['id'],
                                user_id=session['user_id'])
        self.check_ok_answer(resp, is_new=False)

        par_id = doc.get_paragraph_by_task(task_name).get_id()
        j = self.get('/getState', as_json=True,
                     query_string={'user_id': session['user_id'],
                                   'answer_id': answer_list[0]['id'],
                                   'par_id': par_id,
                                   'doc_id': doc.doc_id})
        self.assertDictEqual({'html': "<div id='" + task_id_ext + "' data-plugin='/mmcq'><mmcq "
                                                              "data-content='{&quot;state&quot;:[true,false,true],&quot;question&quot;:{&quot;onTry&quot;:null,&quot;stem&quot;:&quot;&lt;p&gt;Answer "
                                                              'yes or no to the following '
                                                              'questions.&lt;/p&gt;&quot;,&quot;choices&quot;:[{&quot;text&quot;:&quot;&lt;p&gt;&lt;span '
                                                              'class=\\&quot;math '
                                                              'inline\\&quot;&gt;\\\\(2^2=4\\\\)&lt;/span&gt;&lt;/p&gt;&quot;,&quot;correct&quot;:true,&quot;reason&quot;:&quot;&lt;p&gt;This '
                                                              'is true.&lt;/p&gt;&quot;},{&quot;text&quot;:&quot;&lt;p&gt;All '
                                                              'cats are '
                                                              'black.&lt;/p&gt;&quot;,&quot;correct&quot;:false,&quot;reason&quot;:&quot;&lt;p&gt;No '
                                                              'way.&lt;/p&gt;&quot;},{&quot;text&quot;:&quot;&lt;p&gt;Guess.&lt;/p&gt;&quot;,&quot;correct&quot;:true,&quot;reason&quot;:&quot;&lt;p&gt;No '
                                                              "reason.&lt;/p&gt;&quot;}]}}'></mmcq></div>",
                              'reviewHtml': None}, j)

        timdb = self.get_db()
        timdb.users.grant_access(timdb.users.get_anon_group_id(), doc.doc_id, 'view')

        tree = self.get('/view/{}'.format(doc.doc_id), as_tree=True, query_string={'lazy': False})
        plugs = tree.findall(mmcq_xpath)
        self.assertEqual(1, len(plugs))
        self.assertEqual([True, False, True], json.loads(plugs[0].find('mmcq').get('data-content'))['state'])

        # Testing noanswers parameter: There should be no answers in the document
        tree = self.get('/view/{}'.format(doc.doc_id), as_tree=True, query_string={'lazy': False, 'noanswers': True})
        plugs = tree.findall(mmcq_xpath)
        self.assertEqual(1, len(plugs))
        self.assertIsNone(json.loads(plugs[0].find('mmcq').get('data-content')).get('state'))

        summary = tree.findall('.//div[@class="taskSummary"]')
        self.assertEqual(0, len(summary))
        doc.add_setting('show_task_summary', True)
        tree = self.get('/view/{}'.format(doc.doc_id), as_tree=True, query_string={'lazy': False})
        summary = tree.findall('.//div[@class="taskSummary"]')
        self.assertEqual(1, len(summary))

        self.logout()
        resp = self.post_answer(plugin_type, task_id, [True, False, False])
        self.check_ok_answer(resp)

        anon_id = timdb.users.get_anon_user_id()
        anon_answers = timdb.answers.get_answers(anon_id, task_id)

        self.assertListEqual([{'collaborators': [{'real_name': None, 'user_id': anon_id}],
                               'content': '[true, false, false]',
                               'points': 6.0,
                               'task_id': task_id,
                               'valid': 1,
                               'last_points_modifier': None}],
                             [{k: v for k, v in ans.items() if k not in ('answered_on', 'id')} for ans in anon_answers])

        self.get('/getState', query_string={'user_id': anon_id,
                                            'answer_id': answer_list[0]['id'],
                                            'par_id': par_id,
                                            'doc_id': doc.doc_id}, expect_status=403)
        self.get('/getState', query_string={'user_id': anon_id,
                                            'answer_id': anon_answers[0]['id'],
                                            'par_id': par_id,
                                            'doc_id': doc.doc_id}, expect_status=403)
        tree = self.get('/view/{}'.format(doc.doc_id), as_tree=True, query_string={'lazy': False})
        plugs = tree.findall(mmcq_xpath)
        summary = tree.findall('.//div[@class="taskSummary"]')
        self.assertEqual(1, len(plugs))
        self.assertEqual(0, len(summary))
        # Anonymous users can't see their answers
        self.assertIsNone(json.loads(plugs[0].find('mmcq').get('data-content'))['state'])
        timdb.close()

    def test_idless_plugin(self):
        self.login_test1()
        doc = self.create_doc(from_file='example_docs/idless_plugin.md')
        resp = self.get('/view/{}'.format(doc.doc_id), expect_status=200)
        tree = html.fromstring(resp)
        mmcq_xpath = r'.//div[@class="par csPlugin"]/div[@class="parContent"]/div[@id="{}..{}"]'.format(
            doc.doc_id, doc.get_paragraphs()[0].get_id())
        plugs = tree.findall(mmcq_xpath)
        self.assertEqual(1, len(plugs))

    def test_upload(self):
        self.login_test1()
        db = self.get_db()
        doc = self.create_doc(from_file='example_docs/upload_plugin.md')
        task_name = 'testupload'
        task_name2 = 'testupload2'
        task_id = '{}.{}'.format(doc.doc_id, task_name)
        filename = 'test.txt'
        file_content = 'test file'
        mimetype, ur, user_input = self.do_plugin_upload(doc, file_content, filename, task_id, task_name)
        self.do_plugin_upload(doc, file_content, 'test2.txt', task_id, task_name, expect_version=2)
        self.do_plugin_upload(doc, file_content, filename, task_id, task_name2)
        self.do_plugin_upload(doc, file_content, filename, task_id, task_name, expect_version=3)
        self.do_plugin_upload(doc, file_content, filename, task_id, task_name2, expect_version=2)
        resp = self.post_answer('csPlugin', task_id, user_input)
        self.assertDictResponse({'error': 'File was already uploaded: {}'.format(ur['file'])}, resp, expect_status=400)
        invalid_file = '/test/test'
        resp = self.post_answer('csPlugin',
                                task_id,
                                {"uploadedFile": invalid_file,
                                 "uploadedType": mimetype,
                                 "markup": {"type": "upload"}},
                                )
        self.assertDictResponse({'error': 'Non-existent upload: {}'.format(invalid_file)}, resp, expect_status=400)
        self.assertEqual(file_content, self.get(ur['file'], expect_status=200))
        self.assertEqual(file_content,
                            self.get('/uploads/{}/{}/{}/'.format(doc.doc_id, task_name, session['user_name']), expect_status=200))

        self.login_test2()

        # Another user cannot see the file
        self.get(ur['file'], expect_status=403, expect_content=self.permission_error, as_json=True)

        # and cannot post answers
        resp = self.post_answer('csPlugin', task_id, user_input)
        self.assertDictResponse(self.permission_error,
                                resp,
                                expect_status=403)

        # until he is granted a permission
        ug = db.users.get_personal_usergroup_by_id(session['user_id'])
        db.users.grant_view_access(ug, doc.doc_id)

        # but he still cannot see the file
        resp = self.post_answer('csPlugin', task_id, user_input)
        self.assertDictResponse({'error': "You don't have permission to touch this file."},
                                resp,
                                expect_status=403)
        self.get(ur['file'], expect_status=403, expect_content=self.permission_error, as_json=True)

        # until the 'see answers' right is granted for the document
        db.users.grant_access(ug, doc.doc_id, 'see answers')
        self.get(ur['file'], expect_status=200, expect_content=file_content)

    def do_plugin_upload(self, doc, file_content, filename, task_id, task_name, expect_version=1):
        ur = self.post('/pluginUpload/{}/{}/'.format(doc.doc_id, task_name),
                             data={'file': (io.BytesIO(bytes(file_content, encoding='utf-8')), filename)}, as_json=True, expect_status=200)
        mimetype = "text/plain"
        self.assertDictEqual({'file': '/uploads/{}/{}/{}/{}/{}'.format(doc.doc_id,
                                                                       task_name,
                                                                       session['user_name'],
                                                                       expect_version,
                                                                       filename),
                              'type': mimetype,
                              'block': ur['block']}, ur)
        self.assertIsInstance(ur['block'], int)
        user_input = {"uploadedFile": ur['file'], "uploadedType": mimetype, "markup": {"type": "upload"}}
        resp = self.post_answer('csPlugin', task_id, user_input)
        self.check_ok_answer(resp)
        return mimetype, ur, user_input

    def check_failed_answer(self, resp, is_new=False):
        j = self.assertResponseStatus(resp, return_json=True)
        self.assertIn('web', j)
        self.assertIn('You have exceeded the answering limit.', j['error'])
        self.assertEqual(is_new, j['savedNew'])

    def check_ok_answer(self, resp, is_new=True):
        j = self.assertResponseStatus(resp, return_json=True)
        self.assertIn('web', j)
        self.assertNotIn('error', j)
        self.assertEqual(is_new, j['savedNew'])

    def test_group_answering(self):
        self.login_test1()
        self.login_test2(add=True)
        doc = self.create_doc(from_file='example_docs/upload_plugin.md')
        task_name = 'testupload'
        task_id = '{}.{}'.format(doc.doc_id, task_name)
        filename = 'test.txt'
        file_content = 'test file'
        mimetype, ur, user_input = self.do_plugin_upload(doc, file_content, filename, task_id, task_name)
        answer_list = self.json_req('/answers/{}/{}'.format(task_id, session['user_id']), expect_status=200, as_json=True)
        self.assertEqual(1, len(answer_list))
        self.assertListEqual([{'real_name': 'Test user 1', 'user_id': TEST_USER_1_ID},
                              {'real_name': 'Test user 2', 'user_id': TEST_USER_2_ID}], answer_list[0]['collaborators'])
        self.assertEqual(file_content, self.get(ur['file'], expect_status=200))
        self.login_test2()
        answer_list = self.json_req('/answers/{}/{}'.format(task_id, session['user_id']), expect_status=200,
                                    as_json=True)
        self.assertEqual(1, len(answer_list))
        self.assertListEqual([{'real_name': 'Test user 1', 'user_id': TEST_USER_1_ID},
                              {'real_name': 'Test user 2', 'user_id': TEST_USER_2_ID}], answer_list[0]['collaborators'])
        self.assertEqual(file_content, self.get(ur['file'], expect_status=200))

    def test_all_answers(self):
        self.login_test1()
        doc = self.create_doc(from_file='example_docs/multiple_mmcqs.md')
        plugin_type = 'mmcq'
        task_id = '{}.mmcqexample'.format(doc.doc_id)
        task_id2 = '{}.mmcqexample2'.format(doc.doc_id)
        self.post_answer(plugin_type, task_id, [True, False, False])
        self.post_answer(plugin_type, task_id, [True, True, False])
        self.post_answer(plugin_type, task_id2, [True, False])
        timdb = self.get_db()
        timdb.users.grant_view_access(timdb.users.get_personal_usergroup_by_id(TEST_USER_2_ID), doc.doc_id)
        self.login_test2()
        self.post_answer(plugin_type, task_id, [True, True, True])
        self.post_answer(plugin_type, task_id2, [False, False])
        self.post_answer(plugin_type, task_id2, [False, True])
        self.post_answer(plugin_type, task_id2, [True, True])
        self.get('/allDocumentAnswersPlain/{}'.format(doc.doc_id), expect_status=403)
        self.get('/allAnswersPlain/{}'.format(task_id), expect_status=403)
        self.login_test1()
        text = self.get('/allDocumentAnswersPlain/{}'.format(doc.doc_id), expect_status=200)
        date_re = r'\d{4}-\d{2}-\d{2} \d{2}:\d{2}:\d{2}\.\d{6}\+\d{2}:\d{2}'
        self.assertRegex(text, r"""
testuser1: {1}; {0}; 1; 2\.0
\[True, False, False\]

----------------------------------------------------------------------------------
testuser2: {1}; {0}; 1; 2\.0
\[True, True, True\]

----------------------------------------------------------------------------------
testuser1: {2}; {0}; 1; 1\.0
\[True, False\]

----------------------------------------------------------------------------------
testuser2: {2}; {0}; 1; 2\.0
\[False, False\]
""".format(date_re, re.escape(task_id), re.escape(task_id2)).strip())
        text = self.get('/allAnswersPlain/{}'.format(task_id), expect_status=200)
        self.assertRegex(text, r"""
testuser1: {1}; {0}; 1; 2\.0
\[True, False, False\]

----------------------------------------------------------------------------------
testuser2: {1}; {0}; 1; 2\.0
\[True, True, True\]
        """.format(date_re, re.escape(task_id)).strip())
