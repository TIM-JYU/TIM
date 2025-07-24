from timApp.tests.db.timdbtest import TEST_USER_1_NAME
from timApp.tests.server.timroutetest import TimRouteTest


class GroupLoginTest(TimRouteTest):
    def test_grouplogin(self):
        resp = self.login_test1(force=True)
        uid1 = resp["current_user"]["id"]
        gid = self.get_test_user_1_group_id()
        one_user = {
            "current_user": {
                "email": "test1@example.com",
                "id": uid1,
                "name": "testuser1",
                "consent": None,
                "real_name": TEST_USER_1_NAME,
                "last_name": None,
                "tos_accepted_at": None,
                "group": {"id": gid, "name": "testuser1"},
                "groups": [{"id": gid, "name": "testuser1", "external_id": None}],
            },
            "other_users": [],
        }
        resp["current_user"].pop("folder")
        self.assertEqual(one_user, resp)
        resp = self.login_test2(add=True)
        uid2 = resp["other_users"][0]["id"]
        two_users = {
            "current_user": {
                "email": "test1@example.com",
                "id": uid1,
                "name": "testuser1",
                "consent": None,
                "real_name": TEST_USER_1_NAME,
                "last_name": None,
                "tos_accepted_at": None,
                "group": {"id": gid, "name": "testuser1"},
                "groups": [{"id": gid, "name": "testuser1", "external_id": None}],
            },
            "other_users": [
                {
                    "email": "test2@example.com",
                    "id": uid2,
                    "name": "testuser2",
                    "real_name": "Test user 2",
                }
            ],
        }
        resp["current_user"].pop("folder")
        self.assertEqual(two_users, resp)

        # Trying to log in again should not add a duplicate entry
        resp = self.login_test2(add=True)
        resp["current_user"].pop("folder")
        self.assertDictEqual(two_users, resp)

        resp = self.logout(user_id=uid2)
        resp["current_user"].pop("folder")
        self.assertDictEqual(one_user, resp)
        resp = self.login_test2(add=True)
        resp["current_user"].pop("folder")
        self.assertDictEqual(two_users, resp)
        resp = self.logout(user_id=uid1)
        resp["current_user"].pop("folder")
        self.assertEqual(
            {
                "current_user": {
                    "email": None,
                    "consent": None,
                    "id": 0,
                    "name": "Anonymous",
                    "last_name": None,
                    "tos_accepted_at": None,
                    "real_name": "Anonymous user",
                    "group": {"id": 1, "name": "Anonymous users"},
                    "groups": [
                        {"id": 1, "name": "Anonymous users", "external_id": None}
                    ],
                },
                "other_users": [],
            },
            resp,
        )
