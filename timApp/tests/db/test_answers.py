from typing import List

from timApp.answer.answers import get_users_for_tasks, save_answer
from timApp.plugin.taskid import TaskId
from timApp.tests.db.timdbtest import TimDbTest
from timApp.user.user import User


class AnswerTest(TimDbTest):

    def check_totals(self, user: User, task_ids: List[TaskId], task_count, total_points):
        self.assertEqual([{**user.basic_info_dict,
                           'user': user,
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
