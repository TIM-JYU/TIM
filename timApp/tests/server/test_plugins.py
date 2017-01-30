import io
import json
import re
from collections import OrderedDict
from datetime import timezone, datetime, timedelta
from itertools import product

import dateutil.parser
from lxml import html

from documentmodel.pointsumrule import PointSumRule, PointType
from plugin import Plugin
from routes.sessioninfo import get_current_user_object
from tests.db.timdbtest import TEST_USER_1_ID, TEST_USER_2_ID, TEST_USER_1_NAME
from tests.server.timroutetest import TimRouteTest
from timdb.tim_models import db
from timdb.velp_models import Annotation


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
                                                                  "data-content='{&quot;state&quot;:[true,false,true],&quot;question&quot;:{&quot;button&quot;:null,&quot;onTry&quot;:null,&quot;header&quot;:null,&quot;stem&quot;:&quot;&lt;p&gt;Answer "
                                                                  'yes or no to the following '
                                                                  'questions.&lt;/p&gt;&quot;,&quot;headerText&quot;:null,&quot;choices&quot;:[{&quot;text&quot;:&quot;&lt;p&gt;&lt;span '
                                                                  'class=\\&quot;math '
                                                                  'inline\\&quot;&gt;\\\\(2^2=4\\\\)&lt;/span&gt;&lt;/p&gt;&quot;,&quot;correct&quot;:true,&quot;reason&quot;:&quot;&lt;p&gt;This '
                                                                  'is true.&lt;/p&gt;&quot;},{&quot;text&quot;:&quot;&lt;p&gt;All '
                                                                  'cats are '
                                                                  'black.&lt;/p&gt;&quot;,&quot;correct&quot;:false,&quot;reason&quot;:&quot;&lt;p&gt;No '
                                                                  'way.&lt;/p&gt;&quot;},{&quot;text&quot;:&quot;&lt;p&gt;Guess.&lt;/p&gt;&quot;,&quot;correct&quot;:true,&quot;reason&quot;:&quot;&lt;p&gt;No '
                                                                  "reason.&lt;/p&gt;&quot;}],&quot;buttonText&quot;:null}}'></mmcq></div>",
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

        self.assertListEqual([{'collaborators': [{'real_name': 'Anonymous user', 'email': None, 'user_id': anon_id}],
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
        resp = self.post_answer('csPlugin', task_id, user_input, expect_status=403,
                                expect_content=self.permission_error)

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
        self.assertEqual(is_new, resp['savedNew'] is not None)

    def check_ok_answer(self, resp, is_new=True):
        self.assertIn('web', resp)
        self.assertNotIn('error', resp)
        self.assertEqual(is_new, resp['savedNew'] is not None)

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

        # make sure invalid date won't throw
        self.get('/allDocumentAnswersPlain/{}'.format(doc.doc_id), query_string={'period': 'other', 'periodTo': 'asd'})

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
        def get_pts(rule):
            pts = OrderedDict([('1st', {'task_sum': 6.0, 'velp_sum': 7.0, 'total_sum': 13.0},),
                               ('2nd', {'task_sum': 1.0, 'velp_sum': 5.0, 'total_sum': 6.0},),
                               ('3rd', {'task_sum': 4.0, 'velp_sum': 4.0, 'total_sum': 8.0},)])
            pts2 = OrderedDict([('1st', {'task_sum': 3.0, 'velp_sum': 7.0, 'total_sum': 10.0},),
                                ('2nd', {'task_sum': 8.0, 'velp_sum': 5.0, 'total_sum': 13.0},),
                                ('3rd', {'task_sum': 5.0, 'velp_sum': 4.0, 'total_sum': 9.0},)])
            for k, _ in pts.items():
                for n, t in zip(('task_sum', 'velp_sum'), (PointType.task, PointType.velp)):
                    if t in rule.groups[k].point_types:
                        pass
                    else:
                        pts[k]['total_sum'] -= pts[k][n]
                        pts2[k]['total_sum'] -= pts2[k][n]
                        pts[k][n] = 0
                        pts2[k][n] = 0
            return pts, pts2

        self.login_test1()
        d = self.create_doc(from_file='example_docs/mmcq_example.md').document
        timdb = self.get_db()
        timdb.users.grant_view_access(timdb.users.get_personal_usergroup_by_id(TEST_USER_2_ID), d.doc_id)
        task_ids = ['{}.{}-{}'.format(d.doc_id, a, b) for a, b in product(('t1', 't2', 't3'), ('a', 'b', 'c'))]
        answers = [
            [True, False, True],    # U1: 3 p + 3 v =  6, U2: 0 p + 3 v = 3
            [True, True, False],    # U1: 1 p + 1 v =  2, U2: 2 p + 1 v = 3
            [True, False, False],   # U1: 2 p + 3 v =  5, U2: 1 p + 3 v = 4
            #                         U1: 6 p + 7 v = 13, U2: 3 p + 7 v = 10

            [False, True, False],   # U1: 0 p + 1 v = 1, U2: 3 p + 1 v = 4
            [False, True, False],   # U1: 0 p + 3 v = 3, U2: 3 p + 3 v = 6
            [False, False, False],  # U1: 1 p + 1 v = 2, U2: 2 p + 1 v = 3
            #                         U1: 1 p + 5 v = 6, U2: 8 p + 5 v = 13

            [False, False, True],   # U1: 2 p + 2 v = 4, U2: 1 p + 2 v = 3
            [True, True, False],    # U1: 1 p + 0 v = 1, U2: 2 p + 0 v = 2
            [False, False, False],  # U1: 1 p + 2 v = 2, U2: 2 p + 2 v = 4
            #                         U1: 4 p + 4 v = 8, U2: 5 p + 4 v = 9
        ]
        pars = d.get_paragraphs()
        new = pars[0]
        answer_ids, answer_ids2 = [], []
        for t, a in zip(task_ids, answers):
            new = new.clone()
            new.set_attr('taskId', t.split('.')[1])
            new.save(add=True)
            answer_ids.append(self.post_answer('mmcq', t, a)['savedNew'])
        self.login_test2()
        for t, a in zip(task_ids, answers):
            answer_ids2.append(self.post_answer('mmcq', t, [not b for b in a])['savedNew'])
        _, velp_ver_id = timdb.velps.create_new_velp(TEST_USER_1_ID, 'Test velp')
        # add a 1-point annotation to every answer except the last three
        for ans in answer_ids[:-3] + answer_ids2[:-3]:
            a = Annotation(velp_version_id=velp_ver_id, points=1, annotator_id=TEST_USER_2_ID, answer_id=ans)
            db.session.add(a)

        # add a 2-point annotation to every other answer
        for ans in answer_ids[::2] + answer_ids2[::2]:
            a = Annotation(velp_version_id=velp_ver_id, points=2, annotator_id=TEST_USER_2_ID, answer_id=ans)
            db.session.add(a)

        db.session.commit()
        groups_default = ({'match': ['t1-a', 't1-b', 't1-c']}, 't2.*', 't3.*')
        groups_type_t = ({'match': 't1.*', 'type': 't'},
                         {'match': 't2.*', 'type': 't'},
                         {'match': 't3.*', 'type': 't'})
        groups_type_v = ({'match': 't1.*', 'type': 'v'},
                         {'match': 't2.*', 'type': 'v'},
                         {'match': 't3.*', 'type': 'v'})
        groups_type_mixed = ({'match': 't1.*', 'type': 't'},
                             {'match': 't2.*', 'type': 'v'},
                             {'match': 't3.*', 'type': 'tv'})

        cases = [
            (groups_type_t, 'best', 0, (0, 0, 0), (0, 0, 0)),
            (groups_type_t, 'best', 1, (6, 0, 6), (8, 0, 8)),
            (groups_type_t, 'best', 2, (10, 0, 10), (13, 0, 13)),
            (groups_type_t, 'best', 3, (11, 0, 11), (16, 0, 16)),
            (groups_type_t, 'worst', 0, (0, 0, 0), (0, 0, 0)),
            (groups_type_t, 'worst', 1, (1, 0, 1), (3, 0, 3)),
            (groups_type_t, 'worst', 2, (5, 0, 5), (8, 0, 8)),
            (groups_type_t, 'worst', 3, (11, 0, 11), (16, 0, 16)),

            (groups_default, 'best', 0, (0, 0, 0), (0, 0, 0)),
            (groups_default, 'best', 1, (6, 7, 13), (8, 5, 13)),
            (groups_default, 'best', 2, (10, 11, 21), (11, 12, 23)),
            (groups_default, 'best', 3, (11, 16, 27), (16, 16, 32)),
            (groups_default, 'worst', 0, (0, 0, 0), (0, 0, 0)),
            (groups_default, 'worst', 1, (1, 5, 6), (5, 4, 9)),
            (groups_default, 'worst', 2, (5, 9, 14), (8, 11, 19)),
            (groups_default, 'worst', 3, (11, 16, 27), (16, 16, 32)),

            (groups_type_v, 'best', 0, (0, 0, 0), (0, 0, 0)),
            (groups_type_v, 'best', 1, (0, 7, 7), (0, 7, 7)),
            (groups_type_v, 'best', 2, (0, 12, 12), (0, 12, 12)),
            (groups_type_v, 'best', 3, (0, 16, 16), (0, 16, 16)),
            (groups_type_v, 'worst', 0, (0, 0, 0), (0, 0, 0)),
            (groups_type_v, 'worst', 1, (0, 4, 4), (0, 4, 4)),
            (groups_type_v, 'worst', 2, (0, 9, 9), (0, 9, 9)),
            (groups_type_v, 'worst', 3, (0, 16, 16), (0, 16, 16)),

            (groups_type_mixed, 'best', 0, (0, 0, 0), (0, 0, 0)),
            (groups_type_mixed, 'best', 1, (4, 4, 8), (5, 4, 9)),
            (groups_type_mixed, 'best', 2, (10, 4, 14), (5, 9, 14)),
            (groups_type_mixed, 'best', 3, (10, 9, 19), (8, 9, 17)),
            (groups_type_mixed, 'worst', 0, (0, 0, 0), (0, 0, 0)),
            (groups_type_mixed, 'worst', 1, (0, 5, 5), (3, 0, 3)),
            (groups_type_mixed, 'worst', 2, (6, 5, 11), (3, 5, 8)),
            (groups_type_mixed, 'worst', 3, (10, 9, 19), (8, 9, 17)),
        ]

        for (g1, g2, g3), count_type, count, (tasksum1, velpsum1, sum1), (tasksum2, velpsum2, sum2) in cases:
            rule_dict = {'groups': {'1st': g1, '2nd': g2, '3rd': g3},
                         'count': {count_type: count}}
            rule = PointSumRule(rule_dict)
            points = timdb.answers.get_points_by_rule(
                rule_dict,
                task_ids, [TEST_USER_1_ID, TEST_USER_2_ID])
            self.assertEqual(tasksum1, points[TEST_USER_1_ID]['task_sum'])
            self.assertEqual(tasksum2, points[TEST_USER_2_ID]['task_sum'])
            self.assertEqual(velpsum1, points[TEST_USER_1_ID]['velp_sum'])
            self.assertEqual(velpsum2, points[TEST_USER_2_ID]['velp_sum'])
            pts, pts2 = get_pts(rule)
            for k, _ in pts.items():
                for n, t in zip(('task_sum', 'velp_sum'), (PointType.task, PointType.velp)):
                    if t in rule.groups[k].point_types:
                        self.assertEqual(pts[k][n], points[TEST_USER_1_ID]['groups'][k][n])
                        self.assertEqual(pts2[k][n], points[TEST_USER_2_ID]['groups'][k][n])
                    else:
                        self.assertEqual(0, points[TEST_USER_1_ID]['groups'][k][n])
                        self.assertEqual(0, points[TEST_USER_2_ID]['groups'][k][n])
                self.assertEqual(pts[k]['total_sum'], points[TEST_USER_1_ID]['groups'][k]['total_sum'])
                self.assertEqual(pts2[k]['total_sum'], points[TEST_USER_2_ID]['groups'][k]['total_sum'])
            points = timdb.answers.get_points_by_rule(
                {'groups': {'1st': g1, '2nd': g2, '3rd': g3},
                 'count': {count_type: count}},
                task_ids, [TEST_USER_1_ID, TEST_USER_2_ID], flatten=True)
            self.assertListEqual([{'email': 'test1@example.com',
                                   'groups': pts,
                                   'id': TEST_USER_1_ID,
                                   'name': 'testuser1',
                                   'real_name': TEST_USER_1_NAME,
                                   'task_count': 3,
                                   'task_points': tasksum1,
                                   'velp_points': velpsum1,
                                   'total_points': sum1,
                                   'velped_task_count': 3},
                                  {'email': 'test2@example.com',
                                   'groups': pts2,
                                   'id': TEST_USER_2_ID,
                                   'name': 'testuser2',
                                   'real_name': 'Test user 2',
                                   'task_count': 3,
                                   'task_points': tasksum2,
                                   'velp_points': velpsum2,
                                   'total_points': sum2,
                                   'velped_task_count': 3}], points)

        rule_dict = {'groups': {'1st': groups_type_t[0], '2nd': groups_type_t[1], '3rd': groups_type_t[2]},
                     'count': {'best': 2}}
        _, pts2 = get_pts(PointSumRule(rule_dict))
        d.set_settings({'show_task_summary': True,
                        'point_sum_rule': rule_dict})
        d_html = self.get('/view/{}'.format(d.doc_id), as_tree=True)
        task_summary_text = d_html.cssselect('.taskSummary')[0].text_content()
        self.assertIn('Total points: {}'.format(cases[2][4][0]), task_summary_text)
        self.assertIn(', '.join(('{}: {}'.format(k, v['total_sum']) for k, v in pts2.items())), task_summary_text)

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
