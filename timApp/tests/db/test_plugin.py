from plugin import Plugin
from tests.db.timdbtest import TimDbTest, TEST_USER_1_ID, TEST_USER_2_ID
from timdb.models.docentry import DocEntry


class PluginTest(TimDbTest):
    def test_info(self):
        db = self.get_db()
        d = DocEntry.create('test', self.get_test_user_1_group_id(), from_file='example_docs/mmcq_example.md')
        p = None
        for p in d.document.get_tasks():
            p = Plugin.from_paragraph(p)
            break
        a_ids = []
        for i in range(1, 5):
            aid = db.answers.save_answer([TEST_USER_1_ID], p.full_task_id, 'content{}'.format(i), points=None, valid=True)
            aid2 = db.answers.save_answer([TEST_USER_2_ID], p.full_task_id, 'content{}'.format(i), points=None, valid=True)
            a_ids.append((aid, aid2))
        for i, (aid, aid2) in enumerate(a_ids, start=1):
            answer_data = db.answers.get_answer(aid)
            self.assert_dict_subset(answer_data, {
                'cnt': i,
                'collaborators': [{'email': 'test1@example.com',
                                   'real_name': 'Test user 1',
                                   'user_id': TEST_USER_1_ID}],
                'content': 'content{}'.format(i),
                'points': None,
                'task_id': p.full_task_id,
                'valid': True
            })
            answer_data = db.answers.get_answer(aid2)
            self.assert_dict_subset(answer_data, {
                'cnt': i,
                'collaborators': [{'email': 'test2@example.com',
                                   'real_name': 'Test user 2',
                                   'user_id': TEST_USER_2_ID}],
                'content': 'content{}'.format(i),
                'points': None,
                'task_id': p.full_task_id,
                'valid': True
            })
