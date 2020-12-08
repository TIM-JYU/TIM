from datetime import datetime
from typing import List

from timApp.admin.answer_cli import delete_old_answers
from timApp.answer.answer import Answer
from timApp.answer.answers import get_users_for_tasks, save_answer, get_existing_answers_info
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
                           'velp_points': None,
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

    def test_common_answers(self):
        self.login_test1()
        d = self.create_doc()
        self.add_answer(d, 't', 'x1', user=self.test_user_1)
        self.add_answer(d, 't', 'x2', user=self.test_user_1)
        self.add_answer(d, 't', 'y1', user=self.test_user_2)
        a = self.add_answer(d, 't', 'z', user=self.test_user_1)
        a.users_all.append(self.test_user_2)
        answers = get_existing_answers_info([self.test_user_1, self.test_user_2], TaskId(doc_id=d.id, task_name='t'))
        self.assertEqual(1, answers.count)
        answers = get_existing_answers_info([self.test_user_1], TaskId(doc_id=d.id, task_name='t'))
        self.assertEqual(3, answers.count)

    def test_delete_old_answers(self):
        self.login_test1()
        d = self.create_doc()
        self.add_answer(d, 't1', 'x1', user=self.test_user_1)
        self.add_answer(d, 't2', 'x1', user=self.test_user_1)
        self.add_answer(d, 't1', 'y1', user=self.test_user_1)
        self.add_answer(d, 't2', 'y1', user=self.test_user_1)
        self.add_answer(d, 't2', 'y1inv', user=self.test_user_1, valid=False)

        self.add_answer(d, 't1', 'x2', user=self.test_user_2)
        self.add_answer(d, 't2', 'x2', user=self.test_user_2)
        self.add_answer(d, 't1', 'y2', user=self.test_user_2)
        self.add_answer(d, 't2', 'y2', user=self.test_user_2)
        self.add_answer(d, 't2', 'y2inv', user=self.test_user_2, valid=False)
        db.session.commit()
        r = delete_old_answers(d, ['t1', 't2'])
        db.session.commit()
        self.assertEqual(8, r.total)
        self.assertEqual(4, r.deleted)
        self.assertEqual(4, r.adr.answer)
        self.assertEqual(0, r.adr.answersaver)
        r = delete_old_answers(d, ['t1', 't2'])
        self.assertEqual(4, r.total)
        self.assertEqual(0, r.deleted)
        self.assertEqual(0, r.adr.answer)
        self.assertEqual(0, r.adr.answersaver)

        anss: List[Answer] = self.test_user_1.get_answers_for_task(f'{d.id}.t1').all()
        self.assertEqual(1, len(anss))
        self.assertEqual('y1', anss[0].content_as_json.get('c'))
        anss: List[Answer] = self.test_user_1.get_answers_for_task(f'{d.id}.t2').all()
        self.assertEqual(2, len(anss))
        self.assertEqual('y1inv', anss[0].content_as_json.get('c'))
        self.assertEqual('y1', anss[1].content_as_json.get('c'))

        anss: List[Answer] = self.test_user_2.get_answers_for_task(f'{d.id}.t1').all()
        self.assertEqual(1, len(anss))
        self.assertEqual('y2', anss[0].content_as_json.get('c'))
        anss: List[Answer] = self.test_user_2.get_answers_for_task(f'{d.id}.t2').all()
        self.assertEqual(2, len(anss))
        self.assertEqual('y2inv', anss[0].content_as_json.get('c'))
        self.assertEqual('y2', anss[1].content_as_json.get('c'))
