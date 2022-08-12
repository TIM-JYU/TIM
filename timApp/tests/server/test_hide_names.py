from timApp.auth.accesstype import AccessType
from timApp.tests.db.timdbtest import TEST_USER_1_ID
from timApp.tests.server.timroutetest import TimRouteTest
from timApp.timdb.sqa import db
from timApp.user.user import User, UserInfo
from timApp.util.utils import get_current_time


class HideNamesTest(TimRouteTest):
    def test_no_hide_model_answer_name(self):
        self.login_test1()
        m_user, _ = User.create_with_group(UserInfo(username="mallivastaus"))
        db.session.commit()
        d = self.create_doc(initial_par="#- {#t plugin=textfield}")
        now = get_current_time()
        self.add_answer(d, "t", content="test", user=m_user, answered_on=now)
        self.add_answer(d, "t", content="test2", user=self.test_user_1, answered_on=now)
        self.test_user_2.grant_access(d, AccessType.see_answers)
        db.session.commit()
        self.login_test2()
        r = self.get(d.get_url_for_view("answers"), as_tree=True)
        self.assert_js_variable(
            r,
            "users",
            [
                {
                    "task_count": 1,
                    "task_points": None,
                    "total_points": None,
                    "first_answer_on": now.isoformat(),
                    "last_answer_on": now.isoformat(),
                    "answer_duration": "P0D",
                    "user": {
                        "email": "user2@example.com",
                        "id": TEST_USER_1_ID,
                        "name": "user2",
                        "real_name": "User 2",
                    },
                    "velp_points": None,
                    "velped_task_count": 0,
                },
                {
                    "task_count": 1,
                    "task_points": None,
                    "total_points": None,
                    "first_answer_on": now.isoformat(),
                    "last_answer_on": now.isoformat(),
                    "answer_duration": "P0D",
                    "user": {
                        "email": None,
                        "id": m_user.id,
                        "name": "mallivastaus",
                        "real_name": None,
                    },
                    "velp_points": None,
                    "velped_task_count": 0,
                },
            ],
        )
