from timApp.tests.db.timdbtest import TimDbTest
from timApp.user.user import User


class AnswerTest(TimDbTest):

    def check_totals(self, db, user: User, task_ids, task_count, total_points):
        self.assertListEqual([{'id': user.id,
                               'name': user.name,
                               'email': user.email,
                               'real_name': user.real_name,
                               'task_count': task_count,
                               'task_points': total_points,
                               'total_points': total_points,
                               'velp_points': None,
                               'velped_task_count': 0
                               }], db.answers.get_users_for_tasks(task_ids, [user.id]))

    def test_summary(self):
        db = self.get_db()
        user1 = User.get_by_name('testuser1')
        user2 = User.get_by_name('testuser2')
        task_id1 = '1.test'
        task_id2 = '1.test2'
        self.check_user(db, user1, task_id1, task_id2)
        self.check_user(db, user2, task_id1, task_id2)
        db.answers.save_answer([user1.id, user2.id], '1.test', 'content0', 0.5, [], True)
        self.check_totals(db, user1, [task_id1, task_id2], 2, 1000.5)
        self.check_totals(db, user2, [task_id1, task_id2], 2, 1000.5)

    def check_user(self, db, u: User, task_id1, task_id2):
        uid = u.id
        self.assertListEqual([], db.answers.get_users_for_tasks([task_id1], [uid]))
        db.answers.save_answer([uid], task_id1, 'content', 1, [], True)
        self.check_totals(db, u, [task_id1], 1, 1.0)
        db.answers.save_answer([uid], task_id1, 'content1', 10, [], False)
        self.check_totals(db, u, [task_id1], 1, 1.0)
        db.answers.save_answer([uid], task_id1, 'content', 100, [], True)
        self.check_totals(db, u, [task_id1], 1, 100.0)
        db.answers.save_answer([u.id], task_id2, 'content', 1000, [], True)
        self.check_totals(db, u, [task_id1], 1, 100.0)
        self.check_totals(db, u, [task_id1, task_id2], 2, 1100)
