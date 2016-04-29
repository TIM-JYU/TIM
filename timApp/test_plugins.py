import datetime
import json

from flask import session
from lxml import html

from timroutetest import TimRouteTest


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
        resp = self.app.get('/view/{}'.format(doc.doc_id))
        self.assertResponseStatus(resp)
        tree = html.fromstring(resp.get_data(as_text=True))
        mmcq_xpath = r'.//div[@class="par mmcq"]/div[@class="parContent"]/div[@id="{}.mmcqexample"]'.format(
            doc.doc_id)
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
        answer_list = self.assertResponseStatus(resp, expect_status=200, return_json=True)  # type: list(dict)
        self.assertListEqual(
            [{'collaborators': [{'real_name': 'Test user 1', 'user_id': 4}], 'content': '[true, false, true]',
              'id': 6, 'points': '9', 'task_id': task_id, 'valid': 1},
             {'collaborators': [{'real_name': 'Test user 1', 'user_id': 4}], 'content': '[false, false, true]',
              'id': 5, 'points': None, 'task_id': task_id, 'valid': 1},
             {'collaborators': [{'real_name': 'Test user 1', 'user_id': 4}], 'content': '[true, true, true]',
              'id': 4, 'points': '2', 'task_id': task_id, 'valid': 1},
             {'collaborators': [{'real_name': 'Test user 1', 'user_id': 4}], 'content': '[true, false, false]',
              'id': 3, 'points': '2', 'task_id': task_id, 'valid': 0},
             {'collaborators': [{'real_name': 'Test user 1', 'user_id': 4}], 'content': '[true, true, false]',
              'id': 2, 'points': '1', 'task_id': task_id, 'valid': 1},
             {'collaborators': [{'real_name': 'Test user 1', 'user_id': 4}], 'content': '[true, false, false]',
              'id': 1, 'points': '2', 'task_id': task_id, 'valid': 1}],
            [{k: v for k, v in ans.items() if k != 'answered_on'} for ans in answer_list])
        for ans in answer_list:
            datetime.datetime.strptime(ans['answered_on'], '%Y-%m-%d %H:%M:%S')
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
        self.assertDictEqual({'html': "<div id='" + task_id + "' data-plugin='/mmcq'><mmcq "
                                      "data-content='{&quot;state&quot;:[true,false,true],&quot;question&quot;:{&quot;onTry&quot;:null,&quot;stem&quot;:&quot;&lt;p&gt;Answer "
                                      'yes or no to the following '
                                      'questions.&lt;/p&gt;&quot;,&quot;choices&quot;:[{&quot;text&quot;:&quot;&lt;p&gt;&lt;span '
                                      'class=\\&quot;math '
                                      'inline\\&quot;&gt;\\\\(2^2=4\\\\)&lt;/span&gt;&lt;/p&gt;&quot;,&quot;correct&quot;:true,&quot;reason&quot;:&quot;&lt;p&gt;This '
                                      'is true.&lt;/p&gt;&quot;},{&quot;text&quot;:&quot;&lt;p&gt;All '
                                      'cats are '
                                      'black.&lt;/p&gt;&quot;,&quot;correct&quot;:false,&quot;reason&quot;:&quot;&lt;p&gt;No '
                                      'way.&lt;/p&gt;&quot;},{&quot;text&quot;:&quot;&lt;p&gt;Guess.&lt;/p&gt;&quot;,&quot;correct&quot;:true,&quot;reason&quot;:&quot;&lt;p&gt;No '
                                      "reason.&lt;/p&gt;&quot;}]}}'></mmcq></div>", 'reviewHtml': None}, j)

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
                               'id': 7,
                               'points': '6',
                               'task_id': task_id,
                               'valid': 1}],
                             [{k: v for k, v in ans.items() if k != 'answered_on'} for ans in anon_answers])

        self.assertResponseStatus(self.app.get('/getState',
                                               query_string={'user_id': anon_id,
                                                             'answer_id': answer_list[0]['id'],
                                                             'par_id': par_id,
                                                             'doc_id': doc.doc_id}), expect_status=403)
        self.assertResponseStatus(self.app.get('/getState',
                                               query_string={'user_id': anon_id,
                                                             'answer_id': anon_answers[0]['id'],
                                                             'par_id': par_id,
                                                             'doc_id': doc.doc_id}), expect_status=403)
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
        resp = self.app.get('/view/{}'.format(doc.doc_id))
        self.assertResponseStatus(resp)
        tree = html.fromstring(resp.get_data(as_text=True))
        mmcq_xpath = r'.//div[@class="par csPlugin"]/div[@class="parContent"]/div[@id="{}..{}"]'.format(
            doc.doc_id, doc.get_paragraphs()[0].get_id())
        plugs = tree.findall(mmcq_xpath)
        self.assertEqual(1, len(plugs))

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
