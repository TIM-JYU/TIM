from lxml import html

from timroutetest import TimRouteTest


class PluginTest(TimRouteTest):
    def post_answer(self, plugin_type, doc_id, task_name, user_input, save_teacher=False, teacher=False):
        return self.json_put('/{}/{}.{}/answer/'.format(plugin_type, doc_id, task_name),
                             {"input": user_input,
                              "abData": {"saveTeacher": save_teacher, "teacher": teacher}})

    def test_plugin(self):
        self.login_test1()
        doc = self.create_doc(from_file='example_docs/mmcq_example.md')
        resp = self.app.get('/view/{}'.format(doc.doc_id))
        self.assertResponseStatus(resp)
        ht = resp.get_data(as_text=True)
        tree = html.fromstring(ht)
        plugs = tree.findall(
                r'.//div[@class="par mmcq"]/div[@class="parContent"]/div[@id="{}.mmcqexample"]'.format(doc.doc_id))
        self.assertEqual(1, len(plugs))
        task_name = 'mmcqexample'
        plugin_type = 'mmcq'

        resp = self.post_answer(plugin_type, doc.doc_id, task_name, [True, False, False])
        self.check_ok_answer(resp)

        resp = self.post_answer(plugin_type, doc.doc_id, task_name, [True, False, False])
        self.check_failed_answer(resp)

        wrongname = 'mmcqexamplez'
        resp = self.post_answer(plugin_type, doc.doc_id, wrongname, [True, False, False])
        self.assertInResponse('Task not found in the document: {}'.format(wrongname), resp, 400, json_key='error')

        doc.set_settings({'global_plugin_attrs': {'all': {'answerLimit': 2}}})
        resp = self.post_answer(plugin_type, doc.doc_id, task_name, [True, True, False])
        self.check_ok_answer(resp)

        resp = self.post_answer(plugin_type, doc.doc_id, task_name, [True, True, False])
        self.check_failed_answer(resp)

        doc.set_settings({'global_plugin_attrs': {'mmcq': {'answerLimit': None}}})
        resp = self.post_answer(plugin_type, doc.doc_id, task_name, [True, True, True])
        self.check_ok_answer(resp)
        resp = self.post_answer(plugin_type, doc.doc_id, task_name, [True, True, True])
        self.check_ok_answer(resp, is_new=False)

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
