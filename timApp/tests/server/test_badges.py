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
        it_25 = self.create_group("it_25", [self.test_user_1, self.test_user_2])
        it_26 = self.create_group("it_26", [self.test_user_1, self.test_user_3])
        it_25_cats = self.create_group(
            "it_25-cats", [self.test_user_1, self.test_user_2]
        )
        doc_it_25 = DocEntry.create("groups/it_25", it_25)
        doc_it_26 = DocEntry.create("groups/it_26", it_26)
        db.session.flush()
        self.test_user_1.grant_access(doc_it_25.block, AccessType.teacher)
        self.test_user_1.grant_access(doc_it_26.block, AccessType.teacher)
        self.commit_db()
        doc_it_25.block  # TODO: Why isn't the test working without this?
        doc_it_26.block  # TODO: Why isn't the test working without this?
        self.login_test1()

        # fetch all badges when no badges created
        result_ab_empty = self.get(f"/all_badges")
        self.assertEqual([], result_ab_empty)

        # fetch all badges in context when no badges created
        result_abic_empty = self.get(
            f"/all_badges_in_context/{self.test_user_1.id}/{doc_it_25.block.id}/{it_25.name}"
        )
        self.assertEqual([], result_abic_empty)

        # create 2 badges
        result_cb_1 = self.post(
            "/create_badge",
            data={
                "created_by": self.test_user_1.id,
                "doc_id": doc_it_25.block.id,
                "context_group": it_25.name,
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
                "context_group": it_25.id,
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
                "doc_id": doc_it_25.block.id,
                "context_group": it_25.name,
                "title": "Communicator",
                "color": "red",
                "shape": "round",
                "image": 2,
                "description": "Great communication",
            },
        )

        # fetch all badges after 2 badges created
        result_ab_nonempty = self.get(f"/all_badges")
        self.assertEqual(
            [
                {
                    "active": True,
                    "color": "blue",
                    "context_group": it_25.id,
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
                    "context_group": it_25.id,
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
                    "shape": "round",
                    "title": "Communicator",
                },
            ],
            result_ab_nonempty,
        )

        # modify a badge
        result_mb = self.post(
            "/modify_badge",
            data={
                "badge_id": 1,
                "modified_by": self.test_user_1.id,
                "doc_id": doc_it_25.block.id,
                "context_group": it_25.id,
                "title": "MVP",
                "color": "gold",
                "shape": "hexagon",
                "image": 3,
                "description": "Most valuable player",
            },
        )
        self.assertEqual(
            {
                "color": "gold",
                "context_group": it_25.id,
                "description": "Most valuable player",
                "image": 3,
                "modified": result_mb["modified"],
                "modified_by": 2,
                "shape": "hexagon",
                "title": "MVP",
            },
            result_mb,
        )

        # fetch all badges after 2 badges created and the other one modified
        result_ab_modified = self.get(f"/all_badges")
        self.assertEqual(
            [
                {
                    "active": True,
                    "color": "gold",
                    "context_group": it_25.id,
                    "created": result_cb_1["created"],
                    "created_by": 2,
                    "deleted": None,
                    "deleted_by": None,
                    "description": "Most valuable player",
                    "id": 1,
                    "image": 3,
                    "modified": result_mb["modified"],
                    "modified_by": 2,
                    "restored": None,
                    "restored_by": None,
                    "shape": "hexagon",
                    "title": "MVP",
                },
                {
                    "active": True,
                    "color": "red",
                    "context_group": it_25.id,
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
                    "shape": "round",
                    "title": "Communicator",
                },
            ],
            result_ab_modified,
        )

        # delete a badge
        result_db = self.post(
            "/deactivate_badge",
            data={
                "badge_id": 2,
                "deleted_by": self.test_user_1.id,
                "doc_id": doc_it_25.block.id,
                "context_group": it_25.name,
            },
        )
        self.assertEqual(
            {
                "active": False,
                "deleted": result_db["deleted"],
                "deleted_by": 2,
            },
            result_db,
        )

        # fetch all badges in context after 2 badges created and the other one deleted
        result_abic_deactivated = self.get(
            f"/all_badges_in_context/{self.test_user_1.id}/{doc_it_25.block.id}/{it_25.name}"
        )
        self.assertEqual(
            [
                {
                    "active": True,
                    "color": "gold",
                    "context_group": it_25.id,
                    "created": result_cb_1["created"],
                    "created_by": 2,
                    "deleted": None,
                    "deleted_by": None,
                    "description": "Most valuable player",
                    "id": 1,
                    "image": 3,
                    "modified": result_mb["modified"],
                    "modified_by": 2,
                    "restored": None,
                    "restored_by": None,
                    "shape": "hexagon",
                    "title": "MVP",
                }
            ],
            result_abic_deactivated,
        )

        # create a badge in a different context group
        result_cb_3 = self.post(
            "/create_badge",
            data={
                "created_by": self.test_user_1.id,
                "doc_id": doc_it_26.block.id,
                "context_group": it_26.name,
                "title": "Quick",
                "color": "orange",
                "shape": "rectangle",
                "image": 4,
                "description": "Very fast",
            },
        )

        # fetch all badges in context after creating a badge in different context group
        result_abic_deactivated = self.get(
            f"/all_badges_in_context/{self.test_user_1.id}/{doc_it_25.block.id}/{it_25.name}"
        )
        self.assertEqual(
            [
                {
                    "active": True,
                    "color": "gold",
                    "context_group": it_25.id,
                    "created": result_cb_1["created"],
                    "created_by": 2,
                    "deleted": None,
                    "deleted_by": None,
                    "description": "Most valuable player",
                    "id": 1,
                    "image": 3,
                    "modified": result_mb["modified"],
                    "modified_by": 2,
                    "restored": None,
                    "restored_by": None,
                    "shape": "hexagon",
                    "title": "MVP",
                }
            ],
            result_abic_deactivated,
        )

        # fetch groups badges when no badges given
        result_grba_empty_1 = self.get(f"/groups_badges/{it_25_cats.id}/{it_25.name}")
        self.assertEqual([], result_grba_empty_1)

        # check with badge_given-route is a badge given to some usergroup when no badges given
        result_bg_empty = self.get("/badge_given/1")
        self.assertEqual([], result_bg_empty)

        # check with badge_holders-route is a badge given to some usergroup when no badges given
        result_bg_empty = self.get("/badge_holders/1")
        self.assertEqual([[], []], result_bg_empty)

        # give 3 badges
        result_giba_1 = self.post(
            "/give_badge",
            data={
                "given_by": self.test_user_1.id,
                "doc_id": doc_it_25.block.id,
                "context_group": it_25.name,
                "group_id": it_25_cats.id,
                "badge_id": 1,
                "message": "Great work!",
            },
        )
        self.assertEqual(
            {
                "id": 1,
                "active": True,
                "given_by": self.test_user_1.id,
                "given": result_giba_1["given"],
                "withdrawn_by": None,
                "withdrawn": None,
                "undo_withdrawn_by": None,
                "undo_withdrawn": None,
                "group_id": it_25_cats.id,
                "badge_id": 1,
                "message": "Great work!",
            },
            result_giba_1,
        )
        result_giba_2 = self.post(
            "/give_badge",
            data={
                "given_by": self.test_user_1.id,
                "doc_id": doc_it_25.block.id,
                "context_group": it_25.name,
                "group_id": it_25_cats.id,
                "badge_id": 1,
                "message": "Great work again!",
            },
        )
        result_giba_3 = self.post(
            "/give_badge",
            data={
                "given_by": self.test_user_1.id,
                "doc_id": doc_it_25.block.id,
                "context_group": it_25.name,
                "group_id": it_25_cats.id,
                "badge_id": 1,
                "message": "Great work again! Yeah!",
            },
        )

        # withdraw a badge
        result_wb = self.post(
            "/withdraw_badge",
            data={
                "badge_given_id": 2,
                "withdrawn_by": self.test_user_1.id,
                "doc_id": doc_it_25.block.id,
                "context_group": it_25.name,
            },
        )
        self.assertEqual(
            {
                "active": False,
                "withdrawn_by": self.test_user_1.id,
                "withdrawn": result_wb["withdrawn"],
            },
            result_wb,
        )

        # fetch groups badges after 3 badges given and one of them withdrawn
        result_grba_nonempty = self.get(f"/groups_badges/{it_25_cats.id}/{it_25.name}")
        self.assertEqual(
            [
                {
                    "id": 1,
                    "message": "Great work!",
                    "badgegiven_id": 1,
                    "active": True,
                    "given_by": self.test_user_1.id,
                    "given_by_name": self.test_user_1.real_name,
                    "given": result_giba_1["given"],
                    "withdrawn_by": None,
                    "withdrawn_by_name": None,
                    "withdrawn": None,
                    "undo_withdrawn_by": None,
                    "undo_withdrawn_by_name": None,
                    "undo_withdrawn": None,
                    "color": "gold",
                    "context_group": it_25.id,
                    "created": result_cb_1["created"],
                    "created_by": self.test_user_1.id,
                    "created_by_name": self.test_user_1.real_name,
                    "deleted": None,
                    "deleted_by": None,
                    "deleted_by_name": None,
                    "description": "Most valuable player",
                    "image": 3,
                    "modified": result_mb["modified"],
                    "modified_by": self.test_user_1.id,
                    "modified_by_name": self.test_user_1.real_name,
                    "restored": None,
                    "restored_by": None,
                    "restored_by_name": None,
                    "shape": "hexagon",
                    "title": "MVP",
                },
                {
                    "id": 1,
                    "message": "Great work again! Yeah!",
                    "badgegiven_id": 3,
                    "active": True,
                    "given_by": self.test_user_1.id,
                    "given_by_name": self.test_user_1.real_name,
                    "given": result_giba_3["given"],
                    "withdrawn_by": None,
                    "withdrawn_by_name": None,
                    "withdrawn": None,
                    "undo_withdrawn_by": None,
                    "undo_withdrawn_by_name": None,
                    "undo_withdrawn": None,
                    "color": "gold",
                    "context_group": it_25.id,
                    "created": result_cb_1["created"],
                    "created_by": self.test_user_1.id,
                    "created_by_name": self.test_user_1.real_name,
                    "deleted": None,
                    "deleted_by": None,
                    "deleted_by_name": None,
                    "description": "Most valuable player",
                    "image": 3,
                    "modified": result_mb["modified"],
                    "modified_by": self.test_user_1.id,
                    "modified_by_name": self.test_user_1.real_name,
                    "restored": None,
                    "restored_by": None,
                    "restored_by_name": None,
                    "shape": "hexagon",
                    "title": "MVP",
                },
            ],
            result_grba_nonempty,
        )

        # check with badge_given-route is a badge given to some usergroup
        # after 3 badges given and one of them withdrawn
        result_bg_nonempty = self.get("/badge_given/1")
        self.assertEqual(
            [
                {
                    "id": 1,
                    "message": "Great work!",
                    "badge_id": 1,
                    "group_id": it_25_cats.id,
                    "active": True,
                    "given_by": self.test_user_1.id,
                    "given": result_giba_1["given"],
                    "withdrawn_by": None,
                    "withdrawn": None,
                    "undo_withdrawn_by": None,
                    "undo_withdrawn": None,
                },
                {
                    "id": 3,
                    "message": "Great work again! Yeah!",
                    "badge_id": 1,
                    "group_id": it_25_cats.id,
                    "active": True,
                    "given_by": self.test_user_1.id,
                    "given": result_giba_3["given"],
                    "withdrawn_by": None,
                    "withdrawn": None,
                    "undo_withdrawn_by": None,
                    "undo_withdrawn": None,
                },
            ],
            result_bg_nonempty,
        )

        # check with badge_holders-route is a badge given to some usergroup
        # after 3 badges given and one of them withdrawn
        result_bg_nonempty = self.get("/badge_holders/1")
        self.assertEqual(
            [
                [],
                [
                    {
                        "id": it_25_cats.id,
                        "name": it_25_cats.name,
                        "personal_user": None,
                    }
                ],
            ],
            result_bg_nonempty,
        )

        # withdraw all badges with id=1 from it_25-cats
        result_wab = self.post(
            "/withdraw_all_badges",
            data={
                "badge_id": 1,
                "usergroup_id": it_25_cats.id,
                "withdrawn_by": self.test_user_1.id,
                "context_group": it_25.name,
            },
        )
        self.assertEqual(
            {
                "active": False,
                "withdrawn_by": self.test_user_1.id,
                "withdrawn": result_wab["withdrawn"],
            },
            result_wab,
        )

        # fetch groups badges after 3 badges given and all of them withdrawn
        result_grba_empty_2 = self.get(f"/groups_badges/{it_25_cats.id}/{it_25.name}")
        self.assertEqual([], result_grba_empty_2)

        # TODO: Test badge_holders after given badge to a user.
        # TODO: Test routes with a user that isn't included in it_25.
        # TODO: Test routes with a user that doesn't have teacher access.
        # TODO: Test routes when badges have been given to a user who is included in many groups.
