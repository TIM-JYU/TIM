from lxml import html

from timroutetest import TimRouteTest


class PluginTest(TimRouteTest):
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

        resp = self.json_put('/mmcq/{}.mmcqexample/answer/'.format(doc.doc_id),
                             {"input": [True, False, False],
                              "abData": {"saveTeacher": False, "teacher": False}})
        j = self.assertResponseStatus(resp, return_json=True)
        self.assertIn('web', j)
        self.assertNotIn('error', j)
        self.assertTrue(j['savedNew'])

        resp = self.json_put('/mmcq/{}.mmcqexample/answer/'.format(doc.doc_id),
                             {"input": [True, False, False],
                              "abData": {"saveTeacher": False, "teacher": False}})
        j = self.assertResponseStatus(resp, return_json=True)
        self.assertIn('web', j)
        self.assertIn('You have exceeded the answering limit.', j['error'])
        self.assertFalse(j['savedNew'])

        wrongname = 'mmcqexamplez'
        resp = self.json_put('/mmcq/{}.{}/answer/'.format(doc.doc_id, wrongname),
                             {"input": [True, False, False],
                              "abData": {"saveTeacher": False, "teacher": False}})
        self.assertInResponse('Task not found in the document: {}'.format(wrongname), resp, 400, json_key='error')
