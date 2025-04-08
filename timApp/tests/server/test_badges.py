from timApp.auth.accesstype import AccessType
from timApp.document.docentry import DocEntry
from timApp.tests.server.timroutetest import TimRouteTest
from timApp.timdb.sqa import db
from timApp.user.usergroup import UserGroup


class BadgeTest(TimRouteTest):
    def create_group(self, name: str, users: list) -> UserGroup:
        ug = UserGroup.create(name)
        for u in users:
            ug.users.append(u)
        return ug

    def test_badge(self):
        main_group = self.create_group("it_25", [self.test_user_1])
        sub_group = self.create_group("it_25-cats", [])
        doc = DocEntry.create("groups/it_25", main_group)
        db.session.flush()
        self.test_user_1.grant_access(doc.block, AccessType.teacher)
        self.commit_db()
        doc.block  # TODO: Why isn't the test working without this?
        self.login_test1()
        result_ab_empty = self.get(f"/all_badges")
        self.assertEqual([], result_ab_empty)

        result_abic_empty = self.get(
            f"/all_badges_in_context/{self.test_user_1.id}/{doc.block.id}/{main_group.name}"
        )
        self.assertEqual([], result_abic_empty)
        result_cb_1 = self.post(
            "/create_badge",
            data={
                "created_by": self.test_user_1.id,
                "doc_id": doc.block.id,
                "context_group": main_group.name,
                "title": "Coordinator",
                "color": "blue",
                "shape": "hexagon",
                "image": 1,
                "description": "Great coordination",
            },
        )
        self.assertEqual(
            {
                "active": True,
                "color": "blue",
                "context_group": main_group.id,
                "created": result_cb_1["created"],
                "created_by": 2,
                "deleted": None,
                "deleted_by": None,
                "description": "Great coordination",
                "id": 1,
                "image": 1,
                "modified": None,
                "modified_by": None,
                "restored": None,
                "restored_by": None,
                "shape": "hexagon",
                "title": "Coordinator",
            },
            result_cb_1,
        )
        result_cb_2 = self.post(
            "/create_badge",
            data={
                "created_by": self.test_user_1.id,
                "doc_id": doc.block.id,
                "context_group": main_group.name,
                "title": "Communicator",
                "color": "red",
                "shape": "circle",
                "image": 2,
                "description": "Great communication",
            },
        )
        self.assertEqual(
            {
                "active": True,
                "color": "red",
                "context_group": main_group.id,
                "created": result_cb_2["created"],
                "created_by": 2,
                "deleted": None,
                "deleted_by": None,
                "description": "Great communication",
                "id": 2,
                "image": 2,
                "modified": None,
                "modified_by": None,
                "restored": None,
                "restored_by": None,
                "shape": "circle",
                "title": "Communicator",
            },
            result_cb_2,
        )
        result_ab_nonempty = self.get(f"/all_badges")
        self.assertEqual(
            [
                {
                    "active": True,
                    "color": "blue",
                    "context_group": main_group.id,
                    "created": result_cb_1["created"],
                    "created_by": 2,
                    "deleted": None,
                    "deleted_by": None,
                    "description": "Great coordination",
                    "id": 1,
                    "image": 1,
                    "modified": None,
                    "modified_by": None,
                    "restored": None,
                    "restored_by": None,
                    "shape": "hexagon",
                    "title": "Coordinator",
                },
                {
                    "active": True,
                    "color": "red",
                    "context_group": main_group.id,
                    "created": result_cb_2["created"],
                    "created_by": 2,
                    "deleted": None,
                    "deleted_by": None,
                    "description": "Great communication",
                    "id": 2,
                    "image": 2,
                    "modified": None,
                    "modified_by": None,
                    "restored": None,
                    "restored_by": None,
                    "shape": "circle",
                    "title": "Communicator",
                },
            ],
            result_ab_nonempty,
        )

        # TODO: Test routes with a user that isn't included in it_25.
        # TODO: Test routes with a user that doesn't have teacher access.
