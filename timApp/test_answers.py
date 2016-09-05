from timdb.tim_models import User
from timdbtest import TimDbTest


class AnswerTest(TimDbTest):
    def check_totals(self, db, user: User, task_ids, task_count, total_points):
        self.assertListEqual([{'id': user.id,
                               'name': user.name,
                               'email': user.email,
                               'real_name': user.real_name,
                               'task_count': task_count,
                               'total_points': total_points}], db.answers.getUsersForTasks(task_ids, [user.id]))

    def test_summary(self):
        self.maxDiff = None
        db = self.get_db()
        user1 = db.users.get_user_by_name('testuser1')
        user2 = db.users.get_user_by_name('testuser2')
        task_id1 = '1.test'
        task_id2 = '1.test2'
        self.check_user(db, user1, task_id1, task_id2)
        self.check_user(db, user2, task_id1, task_id2)
        db.answers.saveAnswer([user1.id, user2.id], '1.test', 'content0', 0.5, [], True)
        self.check_totals(db, user1, [task_id1, task_id2], 2, 1000.5)
        self.check_totals(db, user2, [task_id1, task_id2], 2, 1000.5)
        db.close()

    def check_user(self, db, u: User, task_id1, task_id2):
        uid = u.id
        self.assertListEqual([], db.answers.getUsersForTasks([task_id1], [uid]))
        db.answers.saveAnswer([uid], task_id1, 'content', 1, [], True)
        self.check_totals(db, u, [task_id1], 1, 1.0)
        db.answers.saveAnswer([uid], task_id1, 'content1', 10, [], False)
        self.check_totals(db, u, [task_id1], 1, 1.0)
        db.answers.saveAnswer([uid], task_id1, 'content', 100, [], True)
        self.check_totals(db, u, [task_id1], 1, 100.0)
        db.answers.saveAnswer([u.id], task_id2, 'content', 1000, [], True)
        self.check_totals(db, u, [task_id1], 1, 100.0)
        self.check_totals(db, u, [task_id1, task_id2], 2, 1100)
