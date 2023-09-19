from timApp.answer.answer import Answer
from timApp.answer.answers import save_answer
from timApp.document.docentry import DocEntry
from timApp.document.viewcontext import default_view_ctx
from timApp.plugin.plugin import Plugin
from timApp.tests.db.timdbtest import TimDbTest
from timApp.timdb.sqa import db
from timApp.util.flask.responsehelper import to_dict
from timApp.util.utils import static_tim_doc


class PluginTest(TimDbTest):
    def test_info(self):
        d = DocEntry.create(
            "test",
            self.test_user_1.get_personal_group(),
            from_file=static_tim_doc("mmcq_example.md"),
        )
        p = None
        for p in d.document.get_tasks():
            p = Plugin.from_paragraph(p, default_view_ctx)
            break
        a_ids = []
        for i in range(1, 5):
            a = save_answer(
                [self.test_user_1], p.task_id, f"content{i}", points=None, valid=True
            )
            a2 = save_answer(
                [self.test_user_2], p.task_id, f"content{i}", points=None, valid=True
            )
            a_ids.append((a.id, a2.id))
        for i, (aid, aid2) in enumerate(a_ids, start=1):
            a: Answer = db.session.get(Answer, aid)
            self.assert_dict_subset(
                to_dict(a),
                {
                    "content": f'"content{i}"',
                    "points": None,
                    "task_id": p.task_id.doc_task,
                    "valid": True,
                },
            )
            self.assertEqual(i, a.get_answer_number())
            a = db.session.get(Answer, aid2)
            self.assert_dict_subset(
                to_dict(a),
                {
                    "content": f'"content{i}"',
                    "points": None,
                    "task_id": p.task_id.doc_task,
                    "valid": True,
                },
            )
            self.assertEqual(i, a.get_answer_number())
