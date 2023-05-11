from timApp.tests.server.timroutetest import TimRouteTest
from timApp.timdb.sqa import db
from timApp.user.user import UserInfo, User
from timApp.user.usergroup import UserGroup


class UserSelectTest(TimRouteTest):
    """
    Tests for UserSelect plugin.
    """

    def test_userselect_queued_group_actions(self) -> None:
        """UserSelect: Test that basic queued group actions work"""

        # Note: we need to run these test in sequence because the processing queue is shared

        User.create_with_group(
            UserInfo(
                username="testadmin1",
                full_name="Test Admin 1",
                email="testadmin1@test.org",
                password="testadmin1",
            ),
            is_admin=True,
        )
        all_g = UserGroup.create("queue-testusers-all1")
        target_g = UserGroup.create("queue-testusers-target1")
        db.session.flush()

        self.test_user_1.add_to_group(all_g, None)
        self.test_user_2.add_to_group(all_g, None)
        self.test_user_3.add_to_group(all_g, None)

        db.session.commit()

        self.login(
            username="testadmin1", email="testadmin1@test.org", passw="testadmin1"
        )
        d = self.create_doc(
            initial_par="""
``` {#user_select1 plugin="userSelect"}
groups:
    - queue-testusers-all1
useActionQueues: true
actions:
    addToGroups: [queue-testusers-target1]
```

``` {#user_select2 plugin="userSelect"}
groups:
    - queue-testusers-all1
useActionQueues: true
actions:
    removeFromGroups: [queue-testusers-target1]
```
"""
        )

        db.session.commit()

        pars = d.document.get_paragraphs()

        add_group_block_id = pars[0].id
        remove_group_block_id = pars[1].id
        doc_id = d.document.doc_id

        with self.temp_config({"USERSELECT_QUEUED_ACTIONS_CELERY": False}):
            self.subtest_queued_group_actions_add(
                doc_id, add_group_block_id, remove_group_block_id, target_g.name
            )
            self.subtest_queued_group_actions_remove_add_concurrent(
                doc_id, add_group_block_id, remove_group_block_id, target_g.name
            )

    def subtest_queued_group_actions_add(
        self,
        doc_id: int,
        add_group_block_id: str,
        remove_group_block_id: str,
        target_g_name: str,
    ) -> None:
        self.json_post(
            "/userSelect/apply",
            {
                "par": {
                    "doc_id": doc_id,
                    "par_id": add_group_block_id,
                },
                "username": self.test_user_1.name,
            },
        )

        self.get("/userSelect/applyPendingActions")

        ug = UserGroup.get_by_name(target_g_name)

        self.assertEqual({self.test_user_1.name}, {u.name for u in ug.users})

        self.json_post(
            "/userSelect/apply",
            {
                "par": {
                    "doc_id": doc_id,
                    "par_id": remove_group_block_id,
                },
                "username": self.test_user_1.name,
            },
        )

        self.get("/userSelect/applyPendingActions")

        ug = UserGroup.get_by_name(target_g_name)
        db.session.refresh(ug)

        self.assertEqual(set(), {u.name for u in ug.users})

    def subtest_queued_group_actions_remove_add_concurrent(
        self,
        doc_id: int,
        add_group_block_id: str,
        remove_group_block_id: str,
        target_g_name: str,
    ) -> None:
        # Applying two operations for the same user only should apply the latest action
        self.json_post(
            "/userSelect/apply",
            {
                "par": {
                    "doc_id": doc_id,
                    "par_id": add_group_block_id,
                },
                "username": self.test_user_2.name,
            },
        )

        self.json_post(
            "/userSelect/apply",
            {
                "par": {
                    "doc_id": doc_id,
                    "par_id": remove_group_block_id,
                },
                "username": self.test_user_2.name,
            },
        )

        self.get("/userSelect/applyPendingActions")

        ug = UserGroup.get_by_name(target_g_name)
        db.session.refresh(ug)

        self.assertEqual(set(), {u.name for u in ug.users})
