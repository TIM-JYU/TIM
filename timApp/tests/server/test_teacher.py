"""Server tests for xxx."""
import json
import re

from timApp.answer.answer import Answer
from timApp.answer.answers import save_answer
from timApp.auth.accesstype import AccessType
from timApp.plugin.taskid import TaskId
from timApp.tests.db.timdbtest import TEST_USER_2_ID, TEST_USER_1_ID
from timApp.tests.server.timroutetest import TimRouteTest
from timApp.tim_app import get_home_organization_group
from timApp.timdb.sqa import db
from timApp.user.usergroup import UserGroup
from timApp.util.utils import get_current_time


class TeacherTest(TimRouteTest):
    def test_teacher(self):
        self.login_test1()
        d = self.create_doc()
        r = self.get(
            f"/teacher/{d.path}",
            query_string={"group": get_home_organization_group().name},
            as_tree=True,
        )
        self.assertEqual(
            "You don't have access to group 'jyu.fi users'.",
            r.cssselect(".alert.alert-info")[0].text_content().strip(),
        )

    def test_teacher_nonexistent_group(self):
        self.login_test1()
        d = self.create_doc()
        r = self.get(f"/teacher/{d.path}", query_string={"group": "nonexistent"})
        self.assertIn("Following groups were not found: {&#39;nonexistent&#39;}", r)

    def test_teacher_single_user_in_group(self):
        self.login_test1()
        d = self.create_doc(initial_par="#- {plugin=textfield #t}")
        now = get_current_time()
        self.test_user_1.answers.append(
            Answer(
                content=json.dumps({"c": "x"}),
                valid=True,
                task_id=f"{d.id}.t",
                answered_on=now,
            )
        )
        self.test_user_2.answers.append(
            Answer(
                content=json.dumps({"c": "x"}),
                valid=True,
                task_id=f"{d.id}.t",
                answered_on=now,
            )
        )
        self.test_user_2.grant_access(d, AccessType.teacher)
        db.session.commit()
        self.login_test2()
        r = self.get(
            f"/teacher/{d.path}",
            query_string={"group": "testuser2"},
            as_tree=True,
        )
        self.assertEqual(0, len(r.cssselect(".alert.alert-info")))
        self.assert_js_variable(
            r,
            "users",
            [
                {
                    "answer_duration": None,
                    "doc_id": "",
                    "first_answer_on": None,
                    "last_answer_on": None,
                    "task_id": "",
                    "task_count": 1,
                    "task_points": None,
                    "total_points": None,
                    "user": {
                        "email": "test2@example.com",
                        "id": TEST_USER_2_ID,
                        "name": "testuser2",
                        "real_name": "Test user 2",
                    },
                    "velp_points": None,
                    "velped_task_count": 0,
                }
            ],
        )

    def create_group(self, name: str, users: list) -> UserGroup:
        ug = UserGroup.create(name)
        for u in users:
            ug.users.append(u)
        return ug

    def test_mix_accesses(self):
        self.login_test1()
        ug = self.create_group("tu2_mix", [self.test_user_2])
        ug.admin_doc = self.create_doc().block
        self.create_group("tu3_mix", [self.test_user_3])
        self.create_group("empty_mix", [])
        db.session.commit()
        d = self.create_doc(initial_par="#- {plugin=textfield #t}")
        d.document.set_settings(
            {"groups": ["tu2_mix", "tu3_mix"], "group": "empty_mix"}
        )
        save_answer(
            [self.test_user_1], TaskId.parse(f"{d.id}.t"), json.dumps({"c": "tu1"}), 0
        )
        save_answer(
            [self.test_user_2], TaskId.parse(f"{d.id}.t"), json.dumps({"c": "tu2"}), 0
        )
        r = self.get(
            f"/teacher/{d.path}",
            as_tree=True,
        )
        # tu1 and tu2 answered, groups want tu2, tu3 (not personal groups) and empty
        # => only tu2 is listed, missing group access warnings present
        alerts = r.cssselect(".alert.alert-info")
        self.assertEqual(len(alerts), 2)
        self.assertSetEqual(
            {a.text_content().strip() for a in alerts},
            {
                "You don't have access to group 'tu3_mix'.",
                "You don't have access to group 'empty_mix'.",
            },
        )
        users = self.get_js_variable(r, "users")
        self.assertEqual(len(users), 1)
        self.assertEqual(users[0]["user"]["id"], TEST_USER_2_ID)

    def test_missing_groups(self):
        self.login_test1()
        ug = self.create_group("tu2_mg", [self.test_user_2])
        ug.admin_doc = self.create_doc().block
        db.session.commit()
        d = self.create_doc(initial_par="#- {plugin=textfield #t}")
        d.document.set_settings(
            {"groups": ["tu2_mg", "tu2_mg", "kissa", "tu2_mg"], "group": "koira"}
        )
        r = self.get(
            f"/teacher/{d.path}",
            as_tree=True,
        )
        alert = r.cssselect(".alert.alert-info")[0].text_content().strip()
        self.assertIn("Following groups were not found:", alert)
        self.assertSetEqual(
            {m for m in re.findall("'(.*?)'", alert)}, {"kissa", "koira"}
        )

    def test_overlapping_users_in_groups(self):
        self.login_test1()
        ug = self.create_group("tu2_ol", [self.test_user_2])
        ug.admin_doc = self.create_doc().block
        ug = self.create_group("tu12_ol", [self.test_user_1, self.test_user_2])
        ug.admin_doc = self.create_doc().block
        d = self.create_doc(initial_par="#- {plugin=textfield #t}")
        d.document.set_settings({"groups": ["tu2_ol", "tu12_ol"]})
        r = self.get(
            f"/teacher/{d.path}",
            as_tree=True,
        )
        # tu2 is not listed twice
        users = self.get_js_variable(r, "users")
        self.assertEqual(len(users), 2)
        self.assertSetEqual(
            {u["user"]["id"] for u in users}, {TEST_USER_1_ID, TEST_USER_2_ID}
        )
