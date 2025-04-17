from timApp.auth.accesstype import AccessType
from timApp.document.docentry import DocEntry
from timApp.tests.server.timroutetest import TimRouteTest
from timApp.timdb.sqa import db
from timApp.user.usergroup import UserGroup


class BadgeTestMain(TimRouteTest):
    def create_group(self, name: str, users: list) -> UserGroup:
        ug = UserGroup.create(name)
        for u in users:
            ug.users.append(u)
        return ug

    def test_badge_main(self):
        # initialization
        it_25 = self.create_group("it_25", [self.test_user_1, self.test_user_2])
        it_26 = self.create_group("it_26", [self.test_user_1, self.test_user_3])
        it_25_cats = self.create_group(
            "it_25-cats", [self.test_user_1, self.test_user_2]
        )
        it_25_dogs = self.create_group("it_25-dogs", [self.test_user_1])
        doc_it_25 = DocEntry.create("groups/it_25", it_25)
        doc_it_25_cats = DocEntry.create("groups/it_25-cats", it_25_cats)
        doc_it_26 = DocEntry.create("groups/it_26", it_26)
        db.session.flush()
        self.test_user_1.grant_access(doc_it_25.block, AccessType.teacher)
        self.test_user_1.grant_access(doc_it_25_cats.block, AccessType.teacher)
        self.test_user_1.grant_access(doc_it_26.block, AccessType.teacher)
        self.commit_db()
        doc_it_25.block  # TODO: Why isn't the test working without this?
        doc_it_25_cats.block  # TODO: Why isn't the test working without this?
        doc_it_26.block  # TODO: Why isn't the test working without this?
        self.login_test1()

        # fetch all badges when no badges created
        result_ab_empty = self.get(f"/all_badges")
        self.assertEqual([], result_ab_empty)

        # fetch all badges in context when no badges created
        result_abic_empty = self.get(f"/all_badges_in_context/{it_25.name}")
        self.assertEqual([], result_abic_empty)

        # create 2 badges
        result_cb_1 = self.post(
            "/create_badge",
            data={
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
        result_abic_deactivated = self.get(f"/all_badges_in_context/{it_25.name}")
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
                "context_group": it_26.name,
                "title": "Quick",
                "color": "orange",
                "shape": "rectangle",
                "image": 4,
                "description": "Very fast",
            },
        )

        # fetch all badges in context after creating a badge in different context group
        result_abic_deactivated = self.get(f"/all_badges_in_context/{it_25.name}")
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
        self.post(
            "/give_badge",
            data={
                "context_group": it_25.name,
                "group_id": it_25_cats.id,
                "badge_id": 1,
                "message": "Great work again!",
            },
        )
        result_giba_3 = self.post(
            "/give_badge",
            data={
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

        # fetch subgroups when there are not any
        result_sg_empty = self.get(f"/subgroups/{it_26.name}")
        self.assertEqual([], result_sg_empty)

        # fetch subgroups when there are 2 of them
        result_sg_nonempty = self.get(f"/subgroups/{it_25.name}")
        self.assertEqual(
            [
                {"id": it_25_cats.id, "name": it_25_cats.name},
                {"id": it_25_dogs.id, "name": it_25_dogs.name},
            ],
            result_sg_nonempty,
        )

        # fetch users subgroups when there are not any
        result_usg_empty = self.get(
            f"/users_subgroups/{self.test_user_3.id}/{it_25.name}"
        )
        self.assertEqual([], result_usg_empty)

        # fetch users subgroups when there is one
        result_usg_empty = self.get(
            f"/users_subgroups/{self.test_user_2.id}/{it_25.name}"
        )
        self.assertEqual(
            [
                {"id": it_25_cats.id, "name": it_25_cats.name},
            ],
            result_usg_empty,
        )

        # fetch user and his/her personal usergroup
        result_uapg = self.get(f"/user_and_personal_group/{self.test_user_2.name}")
        self.assertEqual(
            [
                {
                    "id": self.test_user_2.id,
                    "name": self.test_user_2.name,
                    "real_name": self.test_user_2.real_name,
                    "email": self.test_user_2.email,
                },
                {
                    "id": self.test_user_2.get_personal_group().id,
                    "name": self.test_user_2.get_personal_group().name,
                },
            ],
            result_uapg,
        )

        # fetch usergroup's members
        result_ugm = self.get(f"/usergroups_members/{it_25_cats.name}")
        self.assertEqual(
            [
                {
                    "id": self.test_user_1.id,
                    "name": self.test_user_1.name,
                    "real_name": self.test_user_1.real_name,
                    "email": self.test_user_1.email,
                },
                {
                    "id": self.test_user_2.id,
                    "name": self.test_user_2.name,
                    "real_name": self.test_user_2.real_name,
                    "email": self.test_user_2.email,
                },
            ],
            result_ugm,
        )

        self.login_test3()

        # fetch all badges in context when user doesn't have teacher access to the context group
        self.get(
            f"/all_badges_in_context/{it_25.name}",
            expect_content="Sorry, you don't have permission to use this resource.",
            expect_status=403,
        )

        # create a badge when user doesn't have teacher access to the context group
        self.post(
            f"/create_badge",
            data={
                "context_group": it_25.name,
                "title": "The Boss",
                "color": "gold",
                "shape": "hexagon",
                "image": "4",
                "description": "You are the boss!",
            },
            expect_content="Sorry, you don't have permission to use this resource.",
            expect_status=403,
        )

        # modify a badge when user doesn't have teacher access to the context group
        self.post(
            f"/modify_badge",
            data={
                "badge_id": 1,
                "active": True,
                "context_group": it_25.id,
                "title": "MVP",
                "color": "yellow",
                "shape": "hexagon",
                "image": 3,
                "description": "Most valuable player",
            },
            expect_content="Sorry, you don't have permission to use this resource.",
            expect_status=403,
        )

        # delete a badge when user doesn't have teacher access to the context group
        self.post(
            f"/deactivate_badge",
            data={
                "badge_id": 1,
                "context_group": it_25.name,
            },
            expect_content="Sorry, you don't have permission to use this resource.",
            expect_status=403,
        )

        # fetch groups badges when user doesn't have teacher access to the context group
        # and is not included in the context group
        self.get(
            f"/groups_badges/{it_25_cats.id}/{it_25.name}",
            expect_content="Sorry, you don't have permission to use this resource.",
            expect_status=403,
        )

        self.login_test2()

        # fetch groups badges when user doesn't have teacher access to the context group
        # and is included in the subgroup
        self.get(
            f"/groups_badges/{it_25_cats.id}/{it_25.name}",
            expect_content=[],
            expect_status=200,
        )

        self.test_user_3.grant_access(doc_it_25.block, AccessType.teacher)
        self.login_test3()

        # fetch groups badges when user has teacher access to the context group
        # and is not included in the context group
        self.get(
            f"/groups_badges/{it_25_cats.id}/{it_25.name}",
            expect_content="Sorry, you don't have permission to use this resource.",
            expect_status=403,
        )

        self.test_user_3.remove_access(doc_it_25.block.id, "teacher")

        # give a badge when user doesn't have teacher access to the context group
        self.post(
            f"/give_badge",
            data={
                "context_group": it_25.name,
                "group_id": it_25_cats.id,
                "badge_id": 1,
                "message": "Awesome!",
            },
            expect_content="Sorry, you don't have permission to use this resource.",
            expect_status=403,
        )

        self.login_test1()

        # give a badge
        self.post(
            f"/give_badge",
            data={
                "context_group": it_25.name,
                "group_id": it_25_cats.id,
                "badge_id": 1,
                "message": "Awesome!",
            },
            expect_status=200,
        )

        self.login_test3()

        # withdraw a badge when user doesn't have teacher access to the context group
        self.post(
            f"/withdraw_badge",
            data={
                "badge_given_id": 4,
                "context_group": it_25.name,
            },
            expect_content="Sorry, you don't have permission to use this resource.",
            expect_status=403,
        )

        # withdraw all badges with id=1 from it_25-cats when user doesn't have teacher access to the context group
        self.post(
            f"/withdraw_all_badges",
            data={
                "badge_id": 1,
                "usergroup_id": it_25_cats.id,
                "context_group": it_25.name,
            },
            expect_content="Sorry, you don't have permission to use this resource.",
            expect_status=403,
        )

        # fetch subgroups when user doesn't have teacher access to the context group
        self.get(
            f"/subgroups/{it_25.name}",
            expect_content="Sorry, you don't have permission to use this resource.",
            expect_status=403,
        )

        # fetch usergroup's members when user doesn't have teacher access to the context group
        self.get(
            f"/usergroups_members/{it_25.name}",
            expect_content="Sorry, you don't have permission to use this resource.",
            expect_status=403,
        )

        self.login_test1()

        # give 2 badges from different context groups to testuser1
        result_giba_4 = self.post(
            f"/give_badge",
            data={
                "context_group": it_25.name,
                "group_id": self.test_user_1.get_personal_group().id,
                "badge_id": 1,
                "message": "Yippee!",
            },
            expect_status=200,
        )
        result_giba_5 = self.post(
            f"/give_badge",
            data={
                "context_group": it_26.name,
                "group_id": self.test_user_1.get_personal_group().id,
                "badge_id": 3,
                "message": "Yahoo!",
            },
            expect_status=200,
        )

        # fetch personal groups badges of testuser1 from context group it_25
        self.get(
            f"/groups_badges/{self.test_user_1.get_personal_group().id}/{it_25.name}",
            expect_content=[
                {
                    "id": 1,
                    "message": "Yippee!",
                    "badgegiven_id": 5,
                    "active": True,
                    "given_by": self.test_user_1.id,
                    "given_by_name": self.test_user_1.real_name,
                    "given": result_giba_4["given"],
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
                }
            ],
            expect_status=200,
        )

        # fetch personal groups badges of testuser1 from context group it_26
        self.get(
            f"/groups_badges/{self.test_user_1.get_personal_group().id}/{it_26.name}",
            expect_content=[
                {
                    "id": 3,
                    "message": "Yahoo!",
                    "badgegiven_id": 6,
                    "active": True,
                    "given_by": self.test_user_1.id,
                    "given_by_name": self.test_user_1.real_name,
                    "given": result_giba_5["given"],
                    "withdrawn_by": None,
                    "withdrawn_by_name": None,
                    "withdrawn": None,
                    "undo_withdrawn_by": None,
                    "undo_withdrawn_by_name": None,
                    "undo_withdrawn": None,
                    "color": "orange",
                    "context_group": it_26.id,
                    "created": result_cb_3["created"],
                    "created_by": self.test_user_1.id,
                    "created_by_name": self.test_user_1.real_name,
                    "deleted": None,
                    "deleted_by": None,
                    "deleted_by_name": None,
                    "description": "Very fast",
                    "image": 4,
                    "modified": None,
                    "modified_by": None,
                    "modified_by_name": None,
                    "restored": None,
                    "restored_by": None,
                    "restored_by_name": None,
                    "shape": "rectangle",
                    "title": "Quick",
                }
            ],
            expect_status=200,
        )

        # delete already given badge and check that it's not given anymore
        self.post(
            f"/deactivate_badge",
            data={
                "badge_id": 3,
                "context_group": it_26.name,
            },
            expect_status=200,
        )
        self.get(
            f"/groups_badges/{self.test_user_1.get_personal_group().id}/{it_26.name}",
            expect_content=[],
            expect_status=200,
        )


class BadgeTestErroneousData(TimRouteTest):
    def create_group(self, name: str, users: list) -> UserGroup:
        ug = UserGroup.create(name)
        for u in users:
            ug.users.append(u)
        return ug

    def test_badge_erroneous_data(self):
        # initialization
        it_27 = self.create_group("it_27", [self.test_user_1])
        doc_it_27 = DocEntry.create("groups/it_27", it_27)
        db.session.flush()
        self.test_user_1.grant_access(doc_it_27.block, AccessType.teacher)
        self.commit_db()
        doc_it_27.block  # TODO: Why isn't the test working without this?
        self.login_test1()
        self.post(
            f"/create_badge",
            data={
                "context_group": "it_27",
                "title": "Coordinator",
                "color": "blue",
                "shape": "hexagon",
                "image": 1,
                "description": "Great coordination",
            },
            expect_status=200,
        )

        # fetch all badges in context with erroneous data
        self.get(
            f"/all_badges_in_context/nonexistent_group",
            expect_status=404,
            expect_content='User group "nonexistent_group" not found',
        )

        # create a badge with erroneous data
        self.post(
            f"/create_badge",
            data={
                "context_group": "nonexistent_group",
                "title": "Coordinator",
                "color": "blue",
                "shape": "hexagon",
                "image": 1,
                "description": "Great coordination",
            },
            expect_status=404,
            expect_content='User group "nonexistent_group" not found',
        )

        # modify a badge with different erroneous data
        self.post(
            f"/modify_badge",
            data={
                "badge_id": 2,
                "context_group": it_27.id,
                "title": "Coordinator",
                "color": "blue",
                "shape": "hexagon",
                "image": 1,
                "description": "Great coordination",
            },
            expect_status=404,
            expect_content='Badge of id "2" not found',
        )
        self.post(
            f"/modify_badge",
            data={
                "badge_id": 1,
                "context_group": 100,
                "title": "Coordinator",
                "color": "blue",
                "shape": "hexagon",
                "image": 1,
                "description": "Great coordination",
            },
            expect_status=404,
            expect_content='User group of id "100" not found',
        )

        # delete a badge with different erroneous data
        self.post(
            f"/deactivate_badge",
            data={
                "badge_id": 2,
                "context_group": it_27.name,
            },
            expect_status=404,
            expect_content='Badge of id "2" not found',
        )
        self.post(
            f"/deactivate_badge",
            data={
                "badge_id": 1,
                "context_group": 100,
            },
            expect_status=404,
            expect_content='User group of id "100" not found',
        )

    # TODO: Test these routes with erroneous data
    #   groups_badges
    #   badge_given
    #   badge_holders
    #   give_badge
    #   withdraw_badge
    #   withdraw_all_badges
    #   undo_withdraw_badge
    #   podium
    #   subgroups
    #   users_subgroups
    #   user_and_personal_group
    #   usergroups_members
    #   current_group_name
    #   editGroupName
