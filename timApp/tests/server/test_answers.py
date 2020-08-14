from datetime import datetime
from typing import List

from timApp.answer.answer import Answer
from timApp.answer.answers import get_users_for_tasks, save_answer
from timApp.plugin.taskid import TaskId
from timApp.tests.server.timroutetest import TimRouteTest
from timApp.timdb.sqa import db
from timApp.user.user import User


class AnswerTest(TimRouteTest):

    def check_totals(self, user: User, task_ids: List[TaskId], task_count, total_points):
        self.assertEqual([{'user': user,
                           'task_count': task_count,
                           'task_points': total_points,
                           'total_points': total_points,
                           'velp_points': 0,
                           'velped_task_count': 0
                           }], get_users_for_tasks(task_ids, [user.id]))

    def test_summary(self):
        user1 = User.get_by_name('testuser1')
        user2 = User.get_by_name('testuser2')
        task_id1 = TaskId.parse('1.test')
        task_id2 = TaskId.parse('1.test2')
        self.check_user(user1, task_id1, task_id2)
        self.check_user(user2, task_id1, task_id2)
        save_answer([user1, user2], TaskId.parse('1.test'), 'content0', 0.5, [], True)
        self.check_totals(user1, [task_id1, task_id2], 2, 1000.5)
        self.check_totals(user2, [task_id1, task_id2], 2, 1000.5)

    def check_user(self, u: User, task_id1: TaskId, task_id2: TaskId):
        uid = u.id
        self.assertListEqual([], get_users_for_tasks([task_id1], [uid]))
        save_answer([u], task_id1, 'content', 1.00001, [], True)
        self.check_totals(u, [task_id1], 1, 1.0)
        save_answer([u], task_id1, 'content1', 10, [], False)
        self.check_totals(u, [task_id1], 1, 1.0)
        save_answer([u], task_id1, 'content', 100, [], True)
        self.check_totals(u, [task_id1], 1, 100.0)
        save_answer([u], task_id2, 'content', 1000, [], True)
        self.check_totals(u, [task_id1], 1, 100.0)
        self.check_totals(u, [task_id1, task_id2], 2, 1100)

    def test_export_import(self):
        self.login_test1()
        d = self.create_doc()
        answers = [
            Answer(
                task_id=f'{d.id}.t',
                points=2,
                content='xx',
                answered_on=datetime(year=2020, month=5, day=19, hour=15, minute=33, second=27),
                valid=True,
            ),
            Answer(
                task_id=f'{d.id}.t',
                points=2,
                content='xx',
                answered_on=datetime(year=2020, month=5, day=18, hour=15, minute=33, second=27),
                valid=True,
            ),
            Answer(
                task_id=f'{d.id}.t',
                points=2,
                content='xx',
                answered_on=datetime(year=2020, month=5, day=20, hour=15, minute=33, second=27),
                valid=True,
            ),
        ]
        for a in answers:
            self.current_user.answers.append(a)
        db.session.commit()

        exported = self.get(f'/exportAnswers/{d.path}')

        d = self.create_doc()
        self.json_post(
            f'/importAnswers', {'answers': exported, 'doc': d.path},
            expect_content='This action requires administrative rights.',
            expect_status=403,
        )
        self.make_admin(self.current_user)
        self.json_post(
            f'/importAnswers', {'answers': exported, 'doc': d.path},
            expect_content={'imported': 3, 'skipped_duplicates': 0, 'missing_users': []},
        )
        self.json_post(
            f'/importAnswers', {'answers': exported, 'doc': d.path},
            expect_content={'imported': 0, 'skipped_duplicates': 3, 'missing_users': []},
        )
        exported[0]['email'] = 'xxx'
        self.json_post(
            f'/importAnswers', {'answers': exported, 'doc': d.path, 'allow_missing_users': True},
            expect_content={'imported': 0, 'skipped_duplicates': 2, 'missing_users': ['xxx']},
        )
        self.json_post(
            f'/importAnswers', {'answers': exported, 'doc': d.path},
            expect_content='Email(s) not found: xxx',
            expect_status=400,
        )

        result: List[Answer] = self.test_user_1.answers.filter_by(task_id=f'{d.id}.t').order_by(Answer.id).all()
        for a, b in zip(result, result[1:]):
            self.assertLess(a.answered_on, b.answered_on)

    def test_too_large_answer(self):
        self.login_test1()
        d = self.create_doc(initial_par='#- {#t plugin=textfield}')
        content_field_size = 200 * 1024
        json_size = content_field_size + 9
        self.post_answer(
            'textfield', f'{d.id}.t',
            user_input={'c': 'x' * content_field_size},
            expect_status=400,
            expect_content=f'Answer is too large (size is {json_size} but maximum is 204800).',
        )
