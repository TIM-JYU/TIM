from timApp.auth.accesstype import AccessType
from timApp.tests.server.timroutetest import TimRouteTest
from timApp.timdb.sqa import db
from timApp.user.groups import do_create_group_impl


class BadgeTestAllBadges(TimRouteTest):
    def test_badge_all_badges(self):
        # initialization
        self.login_test1()
        group1_name = "es_25"
        group2_name = "es_26"
        (group1, doc1) = do_create_group_impl(f"{group1_name}", group1_name)
        (group2, doc2) = do_create_group_impl(f"{group2_name}", group2_name)
        db.session.commit()
        self.test_user_1.grant_access(group1.admin_doc, AccessType.teacher)
        self.test_user_1.grant_access(group2.admin_doc, AccessType.teacher)
        self.commit_db()

        # fetch all badges when no badges created
        result_ab_empty = self.get(f"/all_badges/{group1_name}")
        self.assertEqual([], result_ab_empty)

        # create a badge in 2 different context groups
        result_cb_1 = self.post(
            "/create_badge",
            data={
                "context_group": group1_name,
                "title": "Coordinator",
                "color": "blue",
                "shape": "hexagon",
                "image": 1,
                "description": "Great coordination",
            },
        )
        result_cb_2 = self.post(
            "/create_badge",
            data={
                "context_group": group2_name,
                "title": "Quick",
                "color": "orange",
                "shape": "rectangle",
                "image": 4,
                "description": "Very fast",
            },
        )

        # fetch all badges in 2 different context groups after creating a badge in both of them
        result_ab_1 = self.get(f"/all_badges/{group1_name}")
        self.assertEqual(
            [
                {
                    "active": True,
                    "color": "blue",
                    "context_group": 9,
                    "created": result_cb_1["created"],
                    "created_by": self.test_user_1.id,
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
                }
            ],
            result_ab_1,
        )
        result_ab_2 = self.get(f"/all_badges/{group2_name}")
        self.assertEqual(
            [
                {
                    "active": True,
                    "color": "orange",
                    "context_group": 10,
                    "created": result_cb_2["created"],
                    "created_by": self.test_user_1.id,
                    "deleted": None,
                    "deleted_by": None,
                    "description": "Very fast",
                    "id": 2,
                    "image": 4,
                    "modified": None,
                    "modified_by": None,
                    "restored": None,
                    "restored_by": None,
                    "shape": "rectangle",
                    "title": "Quick",
                }
            ],
            result_ab_2,
        )

        self.login_test2()

        # fetch all badges when user doesn't have teacher access to the context group
        self.get(
            f"/all_badges/{group1_name}",
            expect_content=f'Sorry, you don\'t have permission to use this resource. If you are a teacher of "{group1_name}", please contact TIM admin.',
            expect_status=403,
        )


class BadgeTestCreate(TimRouteTest):
    def test_badge_create(self):
        # initialization
        self.login_test1()
        group1_name = "es_25"
        (group1, doc1) = do_create_group_impl(f"{group1_name}", group1_name)
        db.session.commit()
        self.test_user_1.grant_access(group1.admin_doc, AccessType.teacher)
        self.commit_db()

        # create a badge
        result_cb = self.post(
            "/create_badge",
            data={
                "context_group": group1_name,
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
                "context_group": 9,
                "created": result_cb["created"],
                "created_by": self.test_user_1.id,
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
            result_cb,
        )

        # fetch all badges after a badge is created
        result_ab_nonempty = self.get(f"/all_badges/{group1_name}")
        self.assertEqual(
            [
                {
                    "active": True,
                    "color": "blue",
                    "context_group": 9,
                    "created": result_cb["created"],
                    "created_by": self.test_user_1.id,
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
                }
            ],
            result_ab_nonempty,
        )

        self.login_test2()

        # create a badge when user doesn't have teacher access to the context group
        self.post(
            f"/create_badge",
            data={
                "context_group": group1_name,
                "title": "The Boss",
                "color": "gold",
                "shape": "hexagon",
                "image": "4",
                "description": "You are the boss!",
            },
            expect_content=f'Sorry, you don\'t have permission to use this resource. If you are a teacher of "{group1_name}", please contact TIM admin.',
            expect_status=403,
        )


class BadgeTestModify(TimRouteTest):
    def test_badge_modify(self):
        # initialization
        self.login_test1()
        group1_name = "es_25"
        (group1, doc1) = do_create_group_impl(f"{group1_name}", group1_name)
        db.session.commit()
        self.test_user_1.grant_access(group1.admin_doc, AccessType.teacher)
        self.commit_db()

        # create a badge
        result_cb = self.post(
            "/create_badge",
            data={
                "context_group": group1_name,
                "title": "Coordinator",
                "color": "blue",
                "shape": "hexagon",
                "image": 1,
                "description": "Great coordination",
            },
        )

        # modify a badge
        result_mb = self.post(
            "/modify_badge",
            data={
                "badge_id": 1,
                "context_group": 9,
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
                "context_group": 9,
                "description": "Most valuable player",
                "image": 3,
                "modified": result_mb["modified"],
                "modified_by": self.test_user_1.id,
                "shape": "hexagon",
                "title": "MVP",
            },
            result_mb,
        )

        # fetch all badges after a badge is created and modified
        result_ab_nonempty = self.get(f"/all_badges/{group1_name}")
        self.assertEqual(
            [
                {
                    "active": True,
                    "color": "gold",
                    "context_group": 9,
                    "created": result_cb["created"],
                    "created_by": self.test_user_1.id,
                    "deleted": None,
                    "deleted_by": None,
                    "description": "Most valuable player",
                    "id": 1,
                    "image": 3,
                    "modified": result_mb["modified"],
                    "modified_by": self.test_user_1.id,
                    "restored": None,
                    "restored_by": None,
                    "shape": "hexagon",
                    "title": "MVP",
                }
            ],
            result_ab_nonempty,
        )

        self.login_test2()

        # modify a badge when user doesn't have teacher access to the context group
        self.post(
            f"/modify_badge",
            data={
                "badge_id": 1,
                "context_group": 9,
                "title": "MVP",
                "color": "gold",
                "shape": "hexagon",
                "image": 3,
                "description": "Most valuable player",
            },
            expect_content=f'Sorry, you don\'t have permission to use this resource. If you are a teacher of "{group1_name}", please contact TIM admin.',
            expect_status=403,
        )


class BadgeTestDelete(TimRouteTest):
    def test_badge_delete(self):
        # initialization
        self.login_test1()
        group1_name = "es_25"
        (group1, doc1) = do_create_group_impl(f"{group1_name}", group1_name)
        db.session.commit()
        self.test_user_1.grant_access(group1.admin_doc, AccessType.teacher)
        self.commit_db()

        # create a badge
        result_cb = self.post(
            "/create_badge",
            data={
                "context_group": group1_name,
                "title": "Coordinator",
                "color": "blue",
                "shape": "hexagon",
                "image": 1,
                "description": "Great coordination",
            },
        )

        self.login_test2()

        # delete a badge when user doesn't have teacher access to the context group
        self.post(
            f"/deactivate_badge",
            data={
                "badge_id": 1,
                "context_group": group1_name,
            },
            expect_content=f'Sorry, you don\'t have permission to use this resource. If you are a teacher of "{group1_name}", please contact TIM admin.',
            expect_status=403,
        )

        self.login_test1()

        # delete a badge
        result_db = self.post(
            "/deactivate_badge",
            data={
                "badge_id": 1,
                "context_group": group1_name,
            },
        )
        self.assertEqual(
            {
                "active": False,
                "deleted": result_db["deleted"],
                "deleted_by": self.test_user_1.id,
            },
            result_db,
        )

        # fetch all badges after a badge is created and deleted
        result_db = self.get(f"/all_badges/{group1_name}")
        self.assertEqual([], result_db)


class BadgeTestDeleteGiven(TimRouteTest):
    def test_badge_delete_given(self):
        # initialization
        self.login_test1()
        group1_name = "es_25"
        (group1, doc1) = do_create_group_impl(f"{group1_name}", group1_name)
        db.session.commit()
        self.test_user_1.grant_access(group1.admin_doc, AccessType.teacher)
        self.commit_db()

        # create a badge
        result_cb = self.post(
            "/create_badge",
            data={
                "context_group": group1_name,
                "title": "Coordinator",
                "color": "blue",
                "shape": "hexagon",
                "image": 1,
                "description": "Great coordination",
            },
        )

        # give a badge
        result_giba = self.post(
            "/give_badge",
            data={
                "context_group": group1_name,
                "group_id": self.test_user_2.get_personal_group().id,
                "badge_id": 1,
                "message": "Great work!",
            },
        )

        # delete an already given badge and check that it's not given anymore
        self.post(
            f"/deactivate_badge",
            data={
                "badge_id": 1,
                "context_group": group1_name,
            },
            expect_status=200,
        )
        self.get(
            f"/groups_badges/{self.test_user_2.get_personal_group().id}/{group1_name}",
            expect_content=[],
            expect_status=200,
        )


class BadgeTestGroupsBadges(TimRouteTest):
    def test_badge_groups_badges(self):
        # initialization
        self.login_test1()
        group1_name = "es_25"
        subgroup1_name = "es_25-cats"
        group2_name = "es_26"
        (group1, doc1) = do_create_group_impl(f"{group1_name}", group1_name)
        (subgroup1, subdoc1) = do_create_group_impl(f"{subgroup1_name}", subgroup1_name)
        (group2, doc2) = do_create_group_impl(f"{group2_name}", group2_name)
        db.session.commit()
        self.test_user_1.grant_access(group1.admin_doc, AccessType.teacher)
        self.test_user_1.grant_access(subgroup1.admin_doc, AccessType.teacher)
        self.test_user_1.grant_access(group2.admin_doc, AccessType.teacher)
        self.commit_db()
        self.post(
            f"/groups/addmember/{group1_name}",
            data={"names": ["testuser2"]},
        )
        self.post(
            f"/groups/addmember/{subgroup1_name}",
            data={"names": ["testuser2"]},
        )
        self.post(
            f"/groups/addmember/{group2_name}",
            data={"names": ["testuser2"]},
        )

        # create 2 badges to different context groups
        result_cb_1 = self.post(
            "/create_badge",
            data={
                "context_group": group1_name,
                "title": "Coordinator",
                "color": "blue",
                "shape": "hexagon",
                "image": 1,
                "description": "Great coordination",
            },
            expect_status=200,
        )
        result_cb_2 = self.post(
            "/create_badge",
            data={
                "context_group": group2_name,
                "title": "King",
                "color": "yellow",
                "shape": "hexagon",
                "image": 2,
                "description": "You rule!",
            },
            expect_status=200,
        )

        # fetch groups badges when no badges given
        result_grba_empty = self.get(f"/groups_badges/10/{group1_name}")
        self.assertEqual([], result_grba_empty)

        # give 2 badges from different context groups to testuser2
        result_giba_1 = self.post(
            f"/give_badge",
            data={
                "context_group": group1_name,
                "group_id": self.test_user_2.get_personal_group().id,
                "badge_id": 1,
                "message": "Yippee!",
            },
            expect_status=200,
        )
        result_giba_2 = self.post(
            f"/give_badge",
            data={
                "context_group": group2_name,
                "group_id": self.test_user_2.get_personal_group().id,
                "badge_id": 2,
                "message": "Yahoo!",
            },
            expect_status=200,
        )

        # fetch personal groups badges of testuser2 from context group es_25
        self.get(
            f"/groups_badges/{self.test_user_2.get_personal_group().id}/{group1_name}",
            expect_content=[
                {
                    "id": 1,
                    "message": "Yippee!",
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
                    "color": "blue",
                    "context_group": 9,
                    "created": result_cb_1["created"],
                    "created_by": self.test_user_1.id,
                    "created_by_name": self.test_user_1.real_name,
                    "deleted": None,
                    "deleted_by": None,
                    "deleted_by_name": None,
                    "description": "Great coordination",
                    "image": 1,
                    "modified": None,
                    "modified_by": None,
                    "modified_by_name": None,
                    "restored": None,
                    "restored_by": None,
                    "restored_by_name": None,
                    "shape": "hexagon",
                    "title": "Coordinator",
                }
            ],
            expect_status=200,
        )

        # fetch personal groups badges of testuser2 from context group es_26
        self.get(
            f"/groups_badges/{self.test_user_2.get_personal_group().id}/{group2_name}",
            expect_content=[
                {
                    "id": 2,
                    "message": "Yahoo!",
                    "badgegiven_id": 2,
                    "active": True,
                    "given_by": self.test_user_1.id,
                    "given_by_name": self.test_user_1.real_name,
                    "given": result_giba_2["given"],
                    "withdrawn_by": None,
                    "withdrawn_by_name": None,
                    "withdrawn": None,
                    "undo_withdrawn_by": None,
                    "undo_withdrawn_by_name": None,
                    "undo_withdrawn": None,
                    "color": "yellow",
                    "context_group": 11,
                    "created": result_cb_2["created"],
                    "created_by": self.test_user_1.id,
                    "created_by_name": self.test_user_1.real_name,
                    "deleted": None,
                    "deleted_by": None,
                    "deleted_by_name": None,
                    "description": "You rule!",
                    "image": 2,
                    "modified": None,
                    "modified_by": None,
                    "modified_by_name": None,
                    "restored": None,
                    "restored_by": None,
                    "restored_by_name": None,
                    "shape": "hexagon",
                    "title": "King",
                }
            ],
            expect_status=200,
        )

        # fetch groups badges when user has teacher access to the context group
        # and is not included in the context group
        self.get(
            f"/groups_badges/10/{group1_name}",
            expect_content=[],
            expect_status=200,
        )

        self.login_test2()

        # fetch groups badges when user doesn't have teacher access to the context group
        # and is included in the subgroup
        self.get(
            f"/groups_badges/10/{group1_name}",
            expect_content=[],
            expect_status=200,
        )

        self.login_test3()

        # fetch groups badges when user doesn't have teacher access to the context group
        # and is not included in the context group
        self.get(
            f"/groups_badges/10/{group1_name}",
            expect_content=f'Sorry, you don\'t have permission to use this resource. If you are a teacher of "{group1_name}", please contact TIM admin.',
            expect_status=403,
        )


class BadgeTestBadgeHolders(TimRouteTest):
    def test_badge_badge_holders(self):
        # initialization
        self.login_test1()
        group1_name = "es_25"
        subgroup1_name = "es_25-cats"
        (group1, doc1) = do_create_group_impl(f"{group1_name}", group1_name)
        (subgroup1, subdoc1) = do_create_group_impl(f"{subgroup1_name}", subgroup1_name)
        db.session.commit()
        self.test_user_1.grant_access(group1.admin_doc, AccessType.teacher)
        self.test_user_1.grant_access(subgroup1.admin_doc, AccessType.teacher)
        self.commit_db()
        self.post(
            f"/groups/addmember/{group1_name}",
            data={"names": ["testuser2"]},
        )
        self.post(
            f"/groups/addmember/{subgroup1_name}",
            data={"names": ["testuser2"]},
        )

        # create a badge
        result_cb = self.post(
            "/create_badge",
            data={
                "context_group": group1_name,
                "title": "Coordinator",
                "color": "blue",
                "shape": "hexagon",
                "image": 1,
                "description": "Great coordination",
            },
        )

        # check if a badge is given to some usergroups when no badges given
        result_bg_empty = self.get("/badge_holders/1")
        self.assertEqual([[], []], result_bg_empty)

        # give a badge to a group
        result_giba_g = self.post(
            "/give_badge",
            data={
                "context_group": group1_name,
                "group_id": 10,
                "badge_id": 1,
                "message": "Great work guys!",
            },
        )

        # give a badge to a personal group
        result_giba_pg = self.post(
            "/give_badge",
            data={
                "context_group": group1_name,
                "group_id": self.test_user_2.get_personal_group().id,
                "badge_id": 1,
                "message": "Great work!",
            },
        )

        # check if a badge is given to some usergroups after a badge given to a group and to a personal group
        result_bg_nonempty = self.get("/badge_holders/1")
        self.assertEqual(
            [
                [
                    {
                        "email": self.test_user_2.email,
                        "id": self.test_user_2.id,
                        "name": self.test_user_2.name,
                        "real_name": self.test_user_2.real_name,
                    }
                ],
                [
                    {
                        "id": 10,
                        "name": subgroup1_name,
                        "personal_user": None,
                    }
                ],
            ],
            result_bg_nonempty,
        )

        self.login_test2()

        # fetch all usergroups that holds certain badge when user doesn't have teacher access to the context group
        self.get(
            "badge_holders/1",
            expect_content=f'Sorry, you don\'t have permission to use this resource. If you are a teacher of "{group1_name}", please contact TIM admin.',
            expect_status=403,
        )


class BadgeTestGiveBadge(TimRouteTest):
    def test_badge_give_badge(self):
        # initialization
        self.login_test1()
        group1_name = "es_25"
        subgroup1_name = "es_25-cats"
        (group1, doc1) = do_create_group_impl(f"{group1_name}", group1_name)
        (subgroup1, subdoc1) = do_create_group_impl(f"{subgroup1_name}", subgroup1_name)
        db.session.commit()
        self.test_user_1.grant_access(group1.admin_doc, AccessType.teacher)
        self.test_user_1.grant_access(subgroup1.admin_doc, AccessType.teacher)
        self.commit_db()
        self.post(
            f"/groups/addmember/{group1_name}",
            data={"names": ["testuser2"]},
        )
        self.post(
            f"/groups/addmember/{subgroup1_name}",
            data={"names": ["testuser2"]},
        )

        # create a badge
        result_cb = self.post(
            "/create_badge",
            data={
                "context_group": group1_name,
                "title": "Coordinator",
                "color": "blue",
                "shape": "hexagon",
                "image": 1,
                "description": "Great coordination",
            },
        )

        # give a badge to a group
        result_giba = self.post(
            "/give_badge",
            data={
                "context_group": group1_name,
                "group_id": 10,
                "badge_id": 1,
                "message": "Great work!",
            },
        )
        self.assertEqual(
            {
                "id": 1,
                "active": True,
                "given_by": self.test_user_1.id,
                "given": result_giba["given"],
                "withdrawn_by": None,
                "withdrawn": None,
                "undo_withdrawn_by": None,
                "undo_withdrawn": None,
                "group_id": 10,
                "badge_id": 1,
                "message": "Great work!",
            },
            result_giba,
        )

        # fetch groups badges after given a badge to it
        result_grba = self.get(f"/groups_badges/10/{group1_name}")
        self.assertEqual(
            [
                {
                    "id": 1,
                    "active": True,
                    "given_by": self.test_user_1.id,
                    "given_by_name": self.test_user_1.real_name,
                    "given": result_giba["given"],
                    "withdrawn_by": None,
                    "withdrawn": None,
                    "undo_withdrawn_by": None,
                    "undo_withdrawn": None,
                    "message": "Great work!",
                    "badgegiven_id": 1,
                    "color": "blue",
                    "context_group": 9,
                    "created": result_cb["created"],
                    "created_by": 2,
                    "created_by_name": "Test user 1",
                    "deleted": None,
                    "deleted_by": None,
                    "deleted_by_name": None,
                    "description": "Great coordination",
                    "image": 1,
                    "modified": None,
                    "modified_by": None,
                    "modified_by_name": None,
                    "restored": None,
                    "restored_by": None,
                    "restored_by_name": None,
                    "shape": "hexagon",
                    "title": "Coordinator",
                    "undo_withdrawn_by_name": None,
                    "withdrawn_by_name": None,
                }
            ],
            result_grba,
        )

        self.login_test2()

        # give a badge when user doesn't have teacher access to the context group
        self.post(
            f"/give_badge",
            data={
                "context_group": group1_name,
                "group_id": 10,
                "badge_id": 1,
                "message": "Awesome!",
            },
            expect_content=f'Sorry, you don\'t have permission to use this resource. If you are a teacher of "{group1_name}", please contact TIM admin.',
            expect_status=403,
        )


class BadgeTestWithdrawBadge(TimRouteTest):
    def test_badge_withdraw_badge(self):
        # initialization
        self.login_test1()
        group1_name = "es_25"
        (group1, doc1) = do_create_group_impl(f"{group1_name}", group1_name)
        db.session.commit()
        self.test_user_1.grant_access(group1.admin_doc, AccessType.teacher)
        self.commit_db()
        self.post(
            f"/groups/addmember/{group1_name}",
            data={"names": ["testuser2"]},
        )

        # create a badge
        result_cb = self.post(
            "/create_badge",
            data={
                "context_group": group1_name,
                "title": "Coordinator",
                "color": "blue",
                "shape": "hexagon",
                "image": 1,
                "description": "Great coordination",
            },
        )

        # give a badge to a personal group
        result_giba = self.post(
            "/give_badge",
            data={
                "context_group": group1_name,
                "group_id": self.test_user_2.get_personal_group().id,
                "badge_id": 1,
                "message": "Great work!",
            },
        )

        self.login_test2()

        # withdraw a badge when user doesn't have teacher access to the context group
        self.post(
            f"/withdraw_badge",
            data={
                "badge_given_id": 1,
                "context_group": group1_name,
            },
            expect_content=f'Sorry, you don\'t have permission to use this resource. If you are a teacher of "{group1_name}", please contact TIM admin.',
            expect_status=403,
        )

        self.login_test1()

        # withdraw a badge
        result_wb = self.post(
            "/withdraw_badge",
            data={
                "badge_given_id": 1,
                "context_group": group1_name,
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

        # fetch personal groups badges after given a badge to it and withdrawn the badge
        result_grba = self.get(
            f"/groups_badges/{self.test_user_2.get_personal_group().id}/{group1_name}"
        )
        self.assertEqual(
            [],
            result_grba,
        )


class BadgeTestSubgroups(TimRouteTest):
    def test_badge_subgroups(self):
        # initialization
        self.login_test1()
        group1_name = "es_25"
        subgroup1_name = "es_25-cats"
        subgroup2_name = "es_25-dogs"
        group2_name = "es_26"
        (group1, doc1) = do_create_group_impl(f"{group1_name}", group1_name)
        (subgroup1, subdoc1) = do_create_group_impl(f"{subgroup1_name}", subgroup1_name)
        (subgroup2, subdoc2) = do_create_group_impl(f"{subgroup2_name}", subgroup2_name)
        (group2, doc2) = do_create_group_impl(f"{group2_name}", group2_name)
        db.session.commit()
        self.test_user_1.grant_access(group1.admin_doc, AccessType.teacher)
        self.test_user_1.grant_access(subgroup1.admin_doc, AccessType.teacher)
        self.test_user_1.grant_access(subgroup2.admin_doc, AccessType.teacher)
        self.test_user_1.grant_access(group2.admin_doc, AccessType.teacher)
        self.commit_db()

        # fetch subgroups when there are not any
        result_sg_empty = self.get(f"/subgroups/{group2_name}")
        self.assertEqual([], result_sg_empty)

        # fetch subgroups when there are 2 of them
        result_sg_nonempty = self.get(f"/subgroups/{group1_name}")
        self.assertEqual(
            [
                {"id": 10, "name": subgroup1_name},
                {"id": 11, "name": subgroup2_name},
            ],
            result_sg_nonempty,
        )

        self.login_test2()

        # fetch subgroups when user doesn't have teacher access to the context group
        self.get(
            f"/subgroups/{group1_name}",
            expect_content=f'Sorry, you don\'t have permission to use this resource. If you are a teacher of "{group1_name}", please contact TIM admin.',
            expect_status=403,
        )


class BadgeTestUsersSubgroups(TimRouteTest):
    def test_badge_users_subgroups(self):
        # initialization
        self.login_test1()
        group1_name = "es_25"
        subgroup1_name = "es_25-cats"
        subgroup2_name = "es_25-dogs"
        (group1, doc1) = do_create_group_impl(f"{group1_name}", group1_name)
        (subgroup1, subdoc1) = do_create_group_impl(f"{subgroup1_name}", subgroup1_name)
        (subgroup2, subdoc2) = do_create_group_impl(f"{subgroup2_name}", subgroup2_name)
        db.session.commit()
        self.test_user_1.grant_access(group1.admin_doc, AccessType.teacher)
        self.test_user_1.grant_access(subgroup1.admin_doc, AccessType.teacher)
        self.test_user_1.grant_access(subgroup2.admin_doc, AccessType.teacher)
        self.commit_db()
        self.post(
            f"/groups/addmember/{group1_name}",
            data={"names": ["testuser2", "testuser3"]},
        )
        self.post(
            f"/groups/addmember/{subgroup1_name}",
            data={"names": ["testuser2"]},
        )

        # fetch users subgroups when there are not any
        result_usg_empty = self.get(
            f"/users_subgroups/{self.test_user_3.id}/{group1_name}"
        )
        self.assertEqual([], result_usg_empty)

        # fetch users subgroups when user has teacher access to the context group
        # and is not the user with given user id
        self.get(
            f"/users_subgroups/{self.test_user_2.id}/{group1_name}",
            expect_content=[{"id": 10, "name": subgroup1_name}],
            expect_status=200,
        )

        self.login_test2()

        # fetch users subgroups when user doesn't have teacher access to the context group
        # and is the user with given user id
        self.get(
            f"/users_subgroups/{self.test_user_2.id}/{group1_name}",
            expect_content=[{"id": 10, "name": subgroup1_name}],
            expect_status=200,
        )

        self.login_test3()

        # fetch users subgroups when user doesn't have teacher access to the context group
        # and is not the user with given user id
        self.get(
            f"/users_subgroups/{self.test_user_2.id}/{group1_name}",
            expect_content=f'Sorry, you don\'t have permission to use this resource. If you are a teacher of "{group1_name}", please contact TIM admin.',
            expect_status=403,
        )


class BadgeTestUserAndPersonalGroup(TimRouteTest):
    def test_badge_user_and_personal_group(self):
        # initialization
        self.login_test2()

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


class BadgeTestUsergroupsMembers(TimRouteTest):
    def test_badge_usergroups_members(self):
        # initialization
        self.login_test1()
        subgroup1_name = "es_25-cats"
        subgroup2_name = "es_25-dogs"
        (subgroup1, subdoc1) = do_create_group_impl(f"{subgroup1_name}", subgroup1_name)
        (subgroup2, subdoc2) = do_create_group_impl(f"{subgroup2_name}", subgroup2_name)
        db.session.commit()
        self.test_user_1.grant_access(subgroup1.admin_doc, AccessType.teacher)
        self.test_user_1.grant_access(subgroup2.admin_doc, AccessType.teacher)
        self.commit_db()
        self.post(
            f"/groups/addmember/{subgroup1_name}",
            data={"names": ["testuser2"]},
        )

        # fetch usergroup's members when there are not any
        result_ugm_empty = self.get(f"/usergroups_members/{subgroup2_name}")
        self.assertEqual(
            [],
            result_ugm_empty,
        )

        # fetch usergroup's members when there are 2
        result_ugm_nonempty = self.get(f"/usergroups_members/{subgroup1_name}")
        self.assertEqual(
            [
                {
                    "id": self.test_user_2.id,
                    "name": self.test_user_2.name,
                    "real_name": self.test_user_2.real_name,
                    "email": self.test_user_2.email,
                }
            ],
            result_ugm_nonempty,
        )

        self.login_test2()

        # fetch usergroup's members when user doesn't have view access to the usergroup and belongs to usergroup
        self.get(
            f"/usergroups_members/{subgroup1_name}",
            expect_content=[
                {
                    "id": self.test_user_2.id,
                    "name": self.test_user_2.name,
                    "real_name": self.test_user_2.real_name,
                    "email": self.test_user_2.email,
                }
            ],
            expect_status=200,
        )

        self.login_test3()

        # fetch usergroup's members when user doesn't have view access to the usergroup and doesn't belong to usergroup
        self.get(
            f"/usergroups_members/{subgroup1_name}",
            expect_content=f"Sorry, you don't have permission to use this resource.",
            expect_status=403,
        )


class BadgeTestPodium(TimRouteTest):
    def test_badge_podium(self):
        # initialization
        self.login_test1()
        group1_name = "es_28"
        (group1, doc1) = do_create_group_impl(f"{group1_name}", group1_name)
        db.session.commit()
        self.test_user_1.grant_access(group1.admin_doc, AccessType.teacher)
        self.commit_db()

        # get podium when no subgroups in context group
        self.get(f"/podium/{group1_name}", expect_content=[], expect_status=200)

        # create 6 subgroups in es_28
        subgroup1_name = "es_28-cats"
        subgroup2_name = "es_28-dogs"
        subgroup3_name = "es_28-bats"
        subgroup4_name = "es_28-pigs"
        subgroup5_name = "es_28-lions"
        subgroup6_name = "es_28-wolves"
        (subgroup1, subdoc1) = do_create_group_impl(f"{subgroup1_name}", subgroup1_name)
        (subgroup2, subdoc2) = do_create_group_impl(f"{subgroup2_name}", subgroup2_name)
        (subgroup3, subdoc3) = do_create_group_impl(f"{subgroup3_name}", subgroup3_name)
        (subgroup4, subdoc4) = do_create_group_impl(f"{subgroup4_name}", subgroup4_name)
        (subgroup5, subdoc5) = do_create_group_impl(f"{subgroup5_name}", subgroup5_name)
        (subgroup6, subdoc6) = do_create_group_impl(f"{subgroup6_name}", subgroup6_name)
        db.session.commit()
        self.test_user_1.grant_access(subgroup1.admin_doc, AccessType.teacher)
        self.test_user_1.grant_access(subgroup2.admin_doc, AccessType.teacher)
        self.test_user_1.grant_access(subgroup3.admin_doc, AccessType.teacher)
        self.test_user_1.grant_access(subgroup4.admin_doc, AccessType.teacher)
        self.test_user_1.grant_access(subgroup5.admin_doc, AccessType.teacher)
        self.test_user_1.grant_access(subgroup6.admin_doc, AccessType.teacher)
        self.commit_db()

        # create a badge and give it 2 times to cats and once to bats
        self.post(
            f"/create_badge",
            data={
                "context_group": group1_name,
                "title": "Coordinator",
                "color": "blue",
                "shape": "hexagon",
                "image": 1,
                "description": "Great coordination",
            },
            expect_status=200,
        )
        self.post(
            f"/give_badge",
            data={
                "context_group": group1_name,
                "group_id": 10,
                "badge_id": 1,
                "message": "Congratulations!",
            },
            expect_status=200,
        )
        self.post(
            f"/give_badge",
            data={
                "context_group": group1_name,
                "group_id": 10,
                "badge_id": 1,
                "message": "Congratulations!",
            },
            expect_status=200,
        )
        self.post(
            f"/give_badge",
            data={
                "context_group": group1_name,
                "group_id": 12,
                "badge_id": 1,
                "message": "Congratulations!",
            },
            expect_status=200,
        )

        # get podium after 6 subgroups created and badges given to some of them
        self.get(
            f"/podium/{group1_name}",
            expect_content=[
                {"badge_count": 2, "group_name": subgroup1_name},
                {"badge_count": 1, "group_name": subgroup3_name},
            ],
            expect_status=200,
        )

        # give badges so that all of 6 subgroups have some
        self.post(
            f"/give_badge",
            data={
                "context_group": group1_name,
                "group_id": 11,
                "badge_id": 1,
                "message": "Congratulations!",
            },
            expect_status=200,
        )
        self.post(
            f"/give_badge",
            data={
                "context_group": group1_name,
                "group_id": 11,
                "badge_id": 1,
                "message": "Congratulations!",
            },
            expect_status=200,
        )
        self.post(
            f"/give_badge",
            data={
                "context_group": group1_name,
                "group_id": 11,
                "badge_id": 1,
                "message": "Congratulations!",
            },
            expect_status=200,
        )
        self.post(
            f"/give_badge",
            data={
                "context_group": group1_name,
                "group_id": 15,
                "badge_id": 1,
                "message": "Congratulations!",
            },
            expect_status=200,
        )
        self.post(
            f"/give_badge",
            data={
                "context_group": group1_name,
                "group_id": 15,
                "badge_id": 1,
                "message": "Congratulations!",
            },
            expect_status=200,
        )
        self.post(
            f"/give_badge",
            data={
                "context_group": group1_name,
                "group_id": 15,
                "badge_id": 1,
                "message": "Congratulations!",
            },
            expect_status=200,
        )
        self.post(
            f"/give_badge",
            data={
                "context_group": group1_name,
                "group_id": 15,
                "badge_id": 1,
                "message": "Congratulations!",
            },
            expect_status=200,
        )
        self.post(
            f"/give_badge",
            data={
                "context_group": group1_name,
                "group_id": 14,
                "badge_id": 1,
                "message": "Congratulations!",
            },
            expect_status=200,
        )
        self.post(
            f"/give_badge",
            data={
                "context_group": group1_name,
                "group_id": 14,
                "badge_id": 1,
                "message": "Congratulations!",
            },
            expect_status=200,
        )
        self.post(
            f"/give_badge",
            data={
                "context_group": group1_name,
                "group_id": 14,
                "badge_id": 1,
                "message": "Congratulations!",
            },
            expect_status=200,
        )
        self.post(
            f"/give_badge",
            data={
                "context_group": group1_name,
                "group_id": 14,
                "badge_id": 1,
                "message": "Congratulations!",
            },
            expect_status=200,
        )
        self.post(
            f"/give_badge",
            data={
                "context_group": group1_name,
                "group_id": 14,
                "badge_id": 1,
                "message": "Congratulations!",
            },
            expect_status=200,
        )
        self.post(
            f"/give_badge",
            data={
                "context_group": group1_name,
                "group_id": 13,
                "badge_id": 1,
                "message": "Congratulations!",
            },
            expect_status=200,
        )
        self.post(
            f"/give_badge",
            data={
                "context_group": group1_name,
                "group_id": 13,
                "badge_id": 1,
                "message": "Congratulations!",
            },
            expect_status=200,
        )
        self.post(
            f"/give_badge",
            data={
                "context_group": group1_name,
                "group_id": 13,
                "badge_id": 1,
                "message": "Congratulations!",
            },
            expect_status=200,
        )
        self.post(
            f"/give_badge",
            data={
                "context_group": group1_name,
                "group_id": 13,
                "badge_id": 1,
                "message": "Congratulations!",
            },
            expect_status=200,
        )
        self.post(
            f"/give_badge",
            data={
                "context_group": group1_name,
                "group_id": 13,
                "badge_id": 1,
                "message": "Congratulations!",
            },
            expect_status=200,
        )
        self.post(
            f"/give_badge",
            data={
                "context_group": group1_name,
                "group_id": 13,
                "badge_id": 1,
                "message": "Congratulations!",
            },
            expect_status=200,
        )

        # get podium after badges given to all of 6 subgroups
        self.get(
            f"/podium/{group1_name}",
            expect_content=[
                {"badge_count": 6, "group_name": subgroup4_name},
                {"badge_count": 5, "group_name": subgroup5_name},
                {"badge_count": 4, "group_name": subgroup6_name},
                {"badge_count": 3, "group_name": subgroup2_name},
                {"badge_count": 2, "group_name": subgroup1_name},
            ],
            expect_status=200,
        )

        self.login_test2()

        # get podium when user is not a part of the context group and doesn't have teacher access to context group
        self.get(
            f"/podium/{group1_name}",
            expect_content=f'Sorry, you don\'t have permission to use this resource. If you are a teacher of "{group1_name}", please contact TIM admin.',
            expect_status=403,
        )

        self.login_test1()

        # delete an already given badge and get podium after that
        self.post(
            f"/deactivate_badge",
            data={
                "badge_id": 1,
                "context_group": group1_name,
            },
            expect_status=200,
        )
        self.get(
            f"/podium/{group1_name}",
            expect_content=[],
            expect_status=200,
        )


class BadgeTestErroneousData(TimRouteTest):
    def test_badge_erroneous_data(self):
        # initialization
        self.login_test1()
        group1_name = "es_27"
        (group1, doc1) = do_create_group_impl(f"{group1_name}", group1_name)
        db.session.commit()
        self.test_user_1.grant_access(group1.admin_doc, AccessType.teacher)
        self.commit_db()
        self.post(
            f"/groups/addmember/{group1_name}",
            data={"names": ["testuser2"]},
        )
        self.post(
            f"/create_badge",
            data={
                "context_group": group1_name,
                "title": "Coordinator",
                "color": "blue",
                "shape": "hexagon",
                "image": 1,
                "description": "Great coordination",
            },
            expect_status=200,
        )
        self.post(
            f"/give_badge",
            data={
                "context_group": group1_name,
                "group_id": self.test_user_2.get_personal_group().id,
                "badge_id": 1,
                "message": "Congratulations!",
            },
            expect_status=200,
        )
        self.post(
            f"/give_badge",
            data={
                "context_group": group1_name,
                "group_id": self.test_user_2.get_personal_group().id,
                "badge_id": 1,
                "message": "Congratulations again!",
            },
            expect_status=200,
        )
        self.post(
            f"/withdraw_badge",
            data={
                "badge_given_id": 2,
                "context_group": group1_name,
            },
            expect_status=200,
        )

        # fetch all badges in context with erroneous data
        self.get(
            f"/all_badges/nonexistent_group",
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
                "badge_id": 100,
                "context_group": 9,
                "title": "Coordinator",
                "color": "blue",
                "shape": "hexagon",
                "image": 1,
                "description": "Great coordination",
            },
            expect_status=404,
            expect_content='Badge with id "100" not found',
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
            expect_content='User group with id "100" not found',
        )

        # delete a badge with different erroneous data
        self.post(
            f"/deactivate_badge",
            data={
                "badge_id": 100,
                "context_group": group1_name,
            },
            expect_status=404,
            expect_content='Badge with id "100" not found',
        )
        self.post(
            f"/deactivate_badge",
            data={
                "badge_id": 1,
                "context_group": "nonexistent_group",
            },
            expect_status=404,
            expect_content='User group "nonexistent_group" not found',
        )

        # fetch groups badges with different erroneous data
        self.get(
            f"/groups_badges/100/{group1_name}",
            expect_status=404,
            expect_content='User group with id "100" not found',
        )
        self.get(
            f"/groups_badges/{self.test_user_2.get_personal_group().id}/nonexistent_group",
            expect_status=404,
            expect_content='User group "nonexistent_group" not found',
        )

        # check with erroneous data if a badge is given to someone
        self.get(
            "/badge_holders/100",
            expect_status=404,
            expect_content='Badge with id "100" not found',
        )

        # give a badge with different erroneous data
        self.post(
            f"/give_badge",
            data={
                "context_group": "nonexistent_group",
                "group_id": self.test_user_2.get_personal_group().id,
                "badge_id": 1,
                "message": "You've done your best!",
            },
            expect_status=404,
            expect_content='User group "nonexistent_group" not found',
        )
        self.post(
            f"/give_badge",
            data={
                "context_group": group1_name,
                "group_id": 100,
                "badge_id": 1,
                "message": "You've done your best!",
            },
            expect_status=404,
            expect_content='User group with id "100" not found',
        )
        self.post(
            f"/give_badge",
            data={
                "context_group": group1_name,
                "group_id": self.test_user_2.get_personal_group().id,
                "badge_id": 100,
                "message": "You've done your best!",
            },
            expect_status=404,
            expect_content='Badge with id "100" not found',
        )

        # withdraw a badge with different erroneous data
        self.post(
            f"/withdraw_badge",
            data={
                "badge_given_id": 100,
                "context_group": group1_name,
            },
            expect_status=404,
            expect_content='Given badge with id "100" not found',
        )
        self.post(
            f"/withdraw_badge",
            data={
                "badge_given_id": 1,
                "context_group": "nonexistent_group",
            },
            expect_status=404,
            expect_content='User group "nonexistent_group" not found',
        )

        # get podium with erroneous data
        self.get(
            f"/podium/nonexistent_group",
            expect_status=404,
            expect_content='User group "nonexistent_group" not found',
        )

        # fetch subgroups with erroneous data
        self.get(
            f"/subgroups/nonexistent_group",
            expect_status=404,
            expect_content='User group "nonexistent_group" not found',
        )

        # fetch users subgroups with different erroneous data
        self.get(
            f"/users_subgroups/100/{group1_name}",
            expect_status=404,
            expect_content='User with id "100" not found',
        )
        self.get(
            f"/users_subgroups/{self.test_user_2.id}/nonexistent_group",
            expect_status=404,
            expect_content='User group "nonexistent_group" not found',
        )

        # fetch user and his/her personal usergroup with erroneous data
        self.get(
            f"/user_and_personal_group/nonexistent_user",
            expect_status=404,
            expect_content='User "nonexistent_user" not found',
        )

        # fetch usergroup's members with erroneous data
        self.get(
            f"/usergroups_members/nonexistent_group",
            expect_status=404,
            expect_content='User group "nonexistent_group" not found',
        )
