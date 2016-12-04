import io
import json
import re
from collections import OrderedDict
from datetime import timezone, datetime, timedelta
from itertools import product

import dateutil.parser
from lxml import html

from plugin import Plugin
from routes.sessioninfo import get_current_user_object
from tests.db.timdbtest import TEST_USER_1_ID, TEST_USER_2_ID, TEST_USER_1_NAME
from tests.server.timroutetest import TimRouteTest


class PluginTest(TimRouteTest):
    answer_error = {'error': "You don't have access to this answer."}

    def test_plugin(self):
        self.login_test1()
        doc = self.create_doc(from_file='example_docs/mmcq_example.md').document
        resp = self.get('/view/{}'.format(doc.doc_id))
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
        self.post_answer(plugin_type, task_id_ext_wrong, [True, False, False],
                                expect_status=400,
                                expect_content={'error': 'Document {}: Paragraph not found: {}'
                                .format(doc.doc_id, par_id + 'x')})

        wrongname = 'mmcqexamplez'
        self.post_answer(plugin_type, str(doc.doc_id) + '.' + wrongname, [True, False, False],
                         expect_status=400,
                         expect_content='Task not found in the document: {}'.format(wrongname),
                         json_key='error')

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

        answer_list = self.get('/answers/{}/{}'.format(task_id, self.current_user_id()))
        self.maxDiff = None

        self.assertListEqual(
            [{'collaborators': [{'real_name': TEST_USER_1_NAME, 'email': 'test1@example.com', 'user_id': TEST_USER_1_ID}],
              'content': '[true, false, true]',
              'points': 9.0, 'task_id': task_id, 'valid': True, 'last_points_modifier': None},
             {'collaborators': [{'real_name': TEST_USER_1_NAME, 'email': 'test1@example.com', 'user_id': TEST_USER_1_ID}],
              'content': '[false, false, true]',
              'points': None, 'task_id': task_id, 'valid': True, 'last_points_modifier': None},
             {'collaborators': [{'real_name': TEST_USER_1_NAME, 'email': 'test1@example.com', 'user_id': TEST_USER_1_ID}],
              'content': '[true, true, true]',
              'points': 2.0, 'task_id': task_id, 'valid': True, 'last_points_modifier': None},
             {'collaborators': [{'real_name': TEST_USER_1_NAME, 'email': 'test1@example.com', 'user_id': TEST_USER_1_ID}],
              'content': '[true, false, false]',
              'points': 2.0, 'task_id': task_id, 'valid': False, 'last_points_modifier': None},
             {'collaborators': [{'real_name': TEST_USER_1_NAME, 'email': 'test1@example.com', 'user_id': TEST_USER_1_ID}],
              'content': '[true, true, false]',
              'points': 1.0, 'task_id': task_id, 'valid': True, 'last_points_modifier': None},
             {'collaborators': [{'real_name': TEST_USER_1_NAME, 'email': 'test1@example.com', 'user_id': TEST_USER_1_ID}],
              'content': '[true, false, false]',
              'points': 2.0, 'task_id': task_id, 'valid': True, 'last_points_modifier': None}],
            [{k: v for k, v in ans.items() if k not in ('answered_on', 'id')} for ans in answer_list])
        for ans in answer_list:
            d = dateutil.parser.parse(ans['answered_on'])
            self.assertLess(d - datetime.now(tz=timezone.utc), timedelta(seconds=5))

        self.post_answer(plugin_type, task_id, [True, True, False],
                         save_teacher=False, teacher=True, answer_id=answer_list[0]['id'],
                         user_id=self.current_user_id() - 1, expect_status=400,
                         expect_content={'error': 'userId is not associated with answer_id'})

        resp = self.post_answer(plugin_type, task_id, [False, False, False],
                                save_teacher=False, teacher=True, answer_id=answer_list[0]['id'],
                                user_id=self.current_user_id())
        self.check_ok_answer(resp, is_new=False)

        par_id = doc.get_paragraph_by_task(task_name).get_id()
        j = self.get('/getState',
                     query_string={'user_id': self.current_user_id(),
                                   'answer_id': answer_list[0]['id'],
                                   'par_id': par_id,
                                   'doc_id': doc.doc_id})
        self.assertDictEqual({'html': "<div id='" + task_id_ext + "' data-plugin='/mmcq'><mmcq "
                                                                  "data-content='{&quot;state&quot;:[true,false,true],&quot;question&quot;:{&quot;onTry&quot;:null,&quot;stem&quot;:&quot;&lt;p&gt;Answer "
                                                                  'yes or no to the following '
                                                                  'questions.&lt;/p&gt;&quot;,&quot;headerText&quot;:null,&quot;choices&quot;:[{&quot;text&quot;:&quot;&lt;p&gt;&lt;span '
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

        self.assertListEqual([{'collaborators': [{'real_name': None, 'email': None, 'user_id': anon_id}],
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

    def test_idless_plugin(self):
        self.login_test1()
        doc = self.create_doc(from_file='example_docs/idless_plugin.md').document
        resp = self.get('/view/{}'.format(doc.doc_id))
        tree = html.fromstring(resp)
        mmcq_xpath = r'.//div[@class="par csPlugin"]/div[@class="parContent"]/div[@id="{}..{}"]'.format(
            doc.doc_id, doc.get_paragraphs()[0].get_id())
        plugs = tree.findall(mmcq_xpath)
        self.assertEqual(1, len(plugs))

    def test_upload(self):
        self.login_test1()
        db = self.get_db()
        doc = self.create_doc(from_file='example_docs/upload_plugin.md').document
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
        self.post_answer('csPlugin', task_id, user_input,
                         expect_status=400,
                         expect_content={'error': 'File was already uploaded: {}'.format(ur['file'])})
        invalid_file = '/test/test'
        resp = self.post_answer('csPlugin',
                                task_id,
                                {"uploadedFile": invalid_file,
                                 "uploadedType": mimetype,
                                 "markup": {"type": "upload"}},
                                expect_status=400,
                                expect_content={'error': 'Non-existent upload: {}'.format(invalid_file)}
                                )
        self.assertEqual(file_content, self.get(ur['file']))
        self.assertEqual(file_content,
                         self.get('/uploads/{}/{}/{}/'.format(doc.doc_id, task_name, self.current_user_name()),
                                  expect_status=200))

        self.login_test2()

        # Another user cannot see the file
        self.get(ur['file'], expect_status=403, expect_content=self.permission_error)

        # and cannot post answers
        resp = self.post_answer('csPlugin', task_id, user_input, expect_status=403, expect_content=self.permission_error)

        # until he is granted a permission
        ug = db.users.get_personal_usergroup_by_id(self.current_user_id())
        db.users.grant_view_access(ug, doc.doc_id)

        # but he still cannot see the file
        resp = self.post_answer('csPlugin', task_id, user_input, expect_status=403,
                                expect_content={'error': "You don't have permission to touch this file."})
        self.get(ur['file'], expect_status=403, expect_content=self.permission_error)

        # until the 'see answers' right is granted for the document
        db.users.grant_access(ug, doc.doc_id, 'see answers')
        self.get(ur['file'], expect_content=file_content)

    def do_plugin_upload(self, doc, file_content, filename, task_id, task_name, expect_version=1):
        ur = self.post('/pluginUpload/{}/{}/'.format(doc.doc_id, task_name),
                       data={'file': (io.BytesIO(bytes(file_content, encoding='utf-8')), filename)},
                       expect_status=200)
        mimetype = "text/plain"
        self.assertDictEqual({'file': '/uploads/{}/{}/{}/{}/{}'.format(doc.doc_id,
                                                                       task_name,
                                                                       self.current_user_name(),
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
        self.assertIn('web', resp)
        self.assertIn('You have exceeded the answering limit.', resp['error'])
        self.assertEqual(is_new, resp['savedNew'])

    def check_ok_answer(self, resp, is_new=True):
        self.assertIn('web', resp)
        self.assertNotIn('error', resp)
        self.assertEqual(is_new, resp['savedNew'])

    def test_group_answering(self):
        self.login_test1()
        self.login_test2(add=True)
        doc = self.create_doc(from_file='example_docs/upload_plugin.md').document
        task_name = 'testupload'
        task_id = '{}.{}'.format(doc.doc_id, task_name)
        filename = 'test.txt'
        file_content = 'test file'
        mimetype, ur, user_input = self.do_plugin_upload(doc, file_content, filename, task_id, task_name)
        answer_list = self.get_task_answers(task_id)
        self.assertEqual(1, len(answer_list))
        self.assertListEqual([{'real_name': TEST_USER_1_NAME, 'email': 'test1@example.com', 'user_id': TEST_USER_1_ID},
                              {'real_name': 'Test user 2', 'email': 'test2@example.com', 'user_id': TEST_USER_2_ID}],
                             answer_list[0]['collaborators'])
        self.assertEqual(file_content, self.get(ur['file']))
        self.login_test2()
        answer_list = self.get_task_answers(task_id)
        self.assertEqual(1, len(answer_list))
        self.assertListEqual([{'real_name': TEST_USER_1_NAME, 'email': 'test1@example.com', 'user_id': TEST_USER_1_ID},
                              {'real_name': 'Test user 2', 'email': 'test2@example.com', 'user_id': TEST_USER_2_ID}],
                             answer_list[0]['collaborators'])
        self.assertEqual(file_content, self.get(ur['file']))

    def test_all_answers(self):
        self.login_test1()
        doc = self.create_doc(from_file='example_docs/multiple_mmcqs.md').document
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
        text = self.get('/allDocumentAnswersPlain/{}'.format(doc.doc_id))
        date_re = r'\d{4}-\d{2}-\d{2} \d{2}:\d{2}:\d{2}\.\d{6}\+\d{2}:\d{2}'
        self.assertRegex(text, r"""
{5}; {3}; {1}; {0}; 1; 2\.0
\[True, False, False\]

----------------------------------------------------------------------------------
{6}; {4}; {1}; {0}; 1; 2\.0
\[True, True, True\]

----------------------------------------------------------------------------------
{5}; {3}; {2}; {0}; 1; 1\.0
\[True, False\]

----------------------------------------------------------------------------------
{6}; {4}; {2}; {0}; 1; 2\.0
\[False, False\]
""".format(date_re, re.escape(task_id), re.escape(task_id2), 'testuser1', 'testuser2', TEST_USER_1_NAME,
           'Test user 2').strip())
        text = self.get('/allAnswersPlain/{}'.format(task_id))
        self.assertRegex(text, r"""
{4}; {2}; {1}; {0}; 1; 2\.0
\[True, False, False\]

----------------------------------------------------------------------------------
{5}; {3}; {1}; {0}; 1; 2\.0
\[True, True, True\]
        """.format(date_re, re.escape(task_id), 'testuser1', 'testuser2', TEST_USER_1_NAME, 'Test user 2').strip())

    def test_save_points(self):
        cannot_give_custom = {'error': 'You cannot give yourself custom points in this task.'}
        self.login_test1()
        doc = self.create_doc(from_file='example_docs/mmcq_example.md').document
        plugin_type = 'mmcq'
        task_id = '{}.mmcqexample'.format(doc.doc_id)
        self.post_answer(plugin_type, task_id, [True, False, False])
        answer_list = self.get_task_answers(task_id)
        answer_id = answer_list[0]['id']
        self.assertEqual(2.0, answer_list[0]['points'])

        # Teacher can give any points regardless of plugin settings
        self.check_save_points(TEST_USER_1_ID, answer_id, 5, 200, self.ok_resp)
        answer_list = self.get_task_answers(task_id)
        self.assertEqual(5.0, answer_list[0]['points'])

        # Teacher can clear points
        self.check_save_points(TEST_USER_1_ID, answer_id, None, 200, self.ok_resp)
        self.check_save_points(TEST_USER_1_ID, answer_id, '', 200, self.ok_resp)
        answer_list = self.get_task_answers(task_id)
        self.assertEqual(None, answer_list[0]['points'])

        point_format_error = {'error': 'Invalid points format.'}
        self.check_save_points(TEST_USER_1_ID, answer_id, '6,6', 400, point_format_error)
        self.check_save_points(TEST_USER_1_ID, answer_id, '6.6', 200, self.ok_resp)
        answer_list = self.get_task_answers(task_id)
        self.assertEqual(6.6, answer_list[0]['points'])
        self.check_save_points(TEST_USER_2_ID, answer_id, None, 200, self.ok_resp)

        self.login_test2()
        self.check_save_points(TEST_USER_1_ID, answer_id, 1, 403, self.permission_error)
        self.check_save_points(TEST_USER_2_ID, answer_id, 1, 403, self.permission_error)
        timdb = self.get_db()
        timdb.users.grant_view_access(timdb.users.get_personal_usergroup_by_id(TEST_USER_2_ID), doc.doc_id)
        self.post_answer(plugin_type, task_id, [True, False, False])
        answer_list = self.get_task_answers(task_id)
        answer_id2 = answer_list[0]['id']
        self.check_save_points(TEST_USER_1_ID, answer_id, 1, 403, self.permission_error)
        self.check_save_points(TEST_USER_2_ID, answer_id, 1, 403, self.answer_error)
        self.check_save_points(TEST_USER_1_ID, answer_id2, 1, 403, self.permission_error)

        self.check_save_points(TEST_USER_2_ID, answer_id2, 1, 400, cannot_give_custom)
        p = Plugin.from_task_id(task_id, user=get_current_user_object())
        p.set_value('pointsRule', {'allowUserMin': 0, 'allowUserMax': 5}).save()
        self.check_save_points(TEST_USER_2_ID, answer_id2, 6, 400, {'error': 'Points must be in range [0,5]'})
        self.check_save_points(TEST_USER_2_ID, answer_id2, 1, 200, self.ok_resp)
        self.check_save_points(TEST_USER_2_ID, answer_id2, None, 400, point_format_error)
        self.check_save_points(TEST_USER_2_ID, answer_id2, '', 400, point_format_error)

        timdb.users.grant_access(timdb.users.get_personal_usergroup_by_id(TEST_USER_2_ID), doc.doc_id, 'see answers')
        self.check_save_points(TEST_USER_1_ID, answer_id, 1, 403, self.permission_error)
        timdb.users.grant_access(timdb.users.get_personal_usergroup_by_id(TEST_USER_2_ID), doc.doc_id, 'teacher')
        self.check_save_points(TEST_USER_1_ID, answer_id, 1, 200, self.ok_resp)

    def test_point_sum_rule(self):
        self.login_test1()
        d = self.create_doc(from_file='example_docs/mmcq_example.md').document
        timdb = self.get_db()
        timdb.users.grant_view_access(timdb.users.get_personal_usergroup_by_id(TEST_USER_2_ID), d.doc_id)
        task_ids = ['{}.{}-{}'.format(d.doc_id, a, b) for a, b in product(('t1', 't2', 't3'), ('a', 'b', 'c'))]
        answers = [
            # t1
            [True, False, True],  # 3 correct
            [True, True, False],  # 1 correct
            [True, False, False],  # 2 correct
            # t2
            [False, True, False],  # 0 correct
            [False, True, False],  # 0 correct
            [False, False, False],  # 1 correct
            # t3
            [False, False, True],  # 2 correct
            [False, True, False],  # 0 correct
            [False, False, False],  # 1 correct
        ]
        pars = d.get_paragraphs()
        new = pars[0]
        for t, a in zip(task_ids, answers):
            new = new.clone()
            new.set_attr('taskId', t.split('.')[1])
            new.save(add=True)
            self.post_answer('mmcq', t, a)
        self.login_test2()
        for t, a in zip(task_ids, answers):
            self.post_answer('mmcq', t, [not b for b in a])
        cases = [
            ('best', 0, 0, 0),
            ('best', 1, 6, 8),
            ('best', 2, 9, 14),
            ('best', 3, 10, 17),
            ('worst', 0, 0, 0),
            ('worst', 1, 1, 3),
            ('worst', 2, 4, 9),
            ('worst', 3, 10, 17),
        ]
        pts = OrderedDict([('1st', 6.0), ('2nd', 1.0), ('3rd', 3.0)])
        pts2 = OrderedDict(((k, 9.0 - v) for k, v in pts.items()))
        for count_type, count, sum1, sum2 in cases:
            points = timdb.answers.get_points_by_rule(
                {'groups': {'1st': 't1.*', '2nd': 't2.*', '3rd': 't3.*'},
                 'count': {count_type: count}},
                task_ids, [TEST_USER_1_ID, TEST_USER_2_ID])
            self.assertEqual(sum1, points[TEST_USER_1_ID]['sum'])
            self.assertEqual(sum2, points[TEST_USER_2_ID]['sum'])
            for k, v in pts.items():
                self.assertEqual(v, points[TEST_USER_1_ID]['groups'][k]['sum'])
                self.assertEqual(9 - v, points[TEST_USER_2_ID]['groups'][k]['sum'])
            points = timdb.answers.get_points_by_rule(
                {'groups': {'1st': 't1.*', '2nd': 't2.*', '3rd': 't3.*'},
                 'count': {count_type: count}},
                task_ids, [TEST_USER_1_ID, TEST_USER_2_ID], flatten=True)
            self.assertListEqual([{'email': 'test1@example.com',
                                   'groups': pts,
                                   'id': 4,
                                   'name': 'testuser1',
                                   'real_name': TEST_USER_1_NAME,
                                   'task_count': 3,
                                   'total_points': sum1,
                                   'velp_points': None,
                                   'velped_task_count': 0},
                                  {'email': 'test2@example.com',
                                   'groups': pts2,
                                   'id': 5,
                                   'name': 'testuser2',
                                   'real_name': 'Test user 2',
                                   'task_count': 3,
                                   'total_points': sum2,
                                   'velp_points': None,
                                   'velped_task_count': 0}], points)
        d.set_settings({'show_task_summary': True,
                        'point_sum_rule': {'groups': {'1st': 't1.*', '2nd': 't2.*', '3rd': 't3.*'},
                                           'count': {'best': 2}}})
        d_html = self.get('/view/{}'.format(d.doc_id), as_tree=True)
        task_summary_text = d_html.cssselect('.taskSummary')[0].text_content()
        self.assertIn('Total points: {}'.format(cases[2][3]), task_summary_text)
        self.assertIn(', '.join(('{}: {}'.format(k, v) for k, v in pts2.items())), task_summary_text)

        # Make sure invalid settings don't crash the document
        d.add_setting('point_sum_rule', {'groups': {'1st': '*', '2nd': 't2.*', '3rd': 't3.*'},
                                         'count': {'best': 'asd'}})
        self.get('/view/{}'.format(d.doc_id))
        d.add_setting('point_sum_rule', {'groups': 'test'})
        self.get('/view/{}'.format(d.doc_id))
        d.add_setting('point_sum_rule', {'groupz': 'test'})
        self.get('/view/{}'.format(d.doc_id))
        d.add_setting('point_sum_rule', [])
        self.get('/view/{}'.format(d.doc_id))
        d.add_setting('point_sum_rule', None)
        self.get('/view/{}'.format(d.doc_id))

    def check_save_points(self, user_id, answer_id, points, expect_status, expect_content):
        self.json_put('/savePoints/{}/{}'.format(user_id, answer_id),
                      json_data={'points': points},
                      expect_status=expect_status,
                      expect_content=expect_content)

    def test_find_tasks(self):
        self.login_test1()
        d = self.create_doc(from_file='example_docs/programming_examples.md').document
        tasks = d.get_tasks()
        self.assertEqual(27, len(list(tasks)))
