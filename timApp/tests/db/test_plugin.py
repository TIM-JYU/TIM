from timApp.answer.answer import Answer
from timApp.answer.answers import save_answer
from timApp.document.docentry import DocEntry
from timApp.plugin.plugin import Plugin
from timApp.tests.db.timdbtest import TimDbTest
from timApp.util.flask.responsehelper import to_dict
from timApp.util.utils import EXAMPLE_DOCS_PATH


class PluginTest(TimDbTest):

    def test_info(self):
        d = DocEntry.create('test', self.test_user_1.get_personal_group(), from_file=f'{EXAMPLE_DOCS_PATH}/mmcq_example.md')
        p = None
        for p in d.document.get_tasks():
            p = Plugin.from_paragraph(p)
            break
        a_ids = []
        for i in range(1, 5):
            aid = save_answer([self.test_user_1], p.task_id,
                              f'content{i}', points=None, valid=True)
            aid2 = save_answer([self.test_user_2], p.task_id,
                               f'content{i}', points=None, valid=True)
            a_ids.append((aid, aid2))
        for i, (aid, aid2) in enumerate(a_ids, start=1):
            a: Answer = Answer.query.get(aid)
            self.assert_dict_subset(to_dict(a), {
                'content': f'"content{i}"',
                'points': None,
                'task_id': p.task_id.doc_task,
                'valid': True
            })
            self.assertEqual(i, a.get_answer_number())
            a = Answer.query.get(aid2)
            self.assert_dict_subset(to_dict(a), {
                'content': f'"content{i}"',
                'points': None,
                'task_id': p.task_id.doc_task,
                'valid': True
            })
            self.assertEqual(i, a.get_answer_number())
