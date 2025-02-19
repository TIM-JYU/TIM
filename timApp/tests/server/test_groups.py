from unittest.mock import patch, Mock

from timApp import tim_celery
from timApp.auth.accesstype import AccessType
from timApp.document.docentry import DocEntry
from timApp.folder.folder import Folder
from timApp.notification.group_notification import JOIN_MESSAGE_TASKID
from timApp.notification.send_email import sent_mails_in_testing
from timApp.tests.server.timroutetest import TimRouteTest
from timApp.tim_app import app
from timApp.timdb.sqa import db
from timApp.user.user import User, UserInfo
from timApp.user.usergroup import UserGroup


class GroupTest(TimRouteTest):
    def error_resp(self, name):
        return {
            "error": "User group must contain at least one digit and one letter and must "
            'not have uppercase or special chars: "' + name + '"'
        }

    def enum_admin_and_groupadmin(self):
        yield self.init_admin()
        yield self.init_groupadmin()

    def test_groups(self):
        for is_admin in self.enum_admin_and_groupadmin():
            names = [f"t{i}{is_admin}" for i in range(1, 5)]
            t1 = names[0]
            t2 = names[1]
            t3 = names[2]
            t4 = names[3]
            t5 = f"t5{is_admin}"
            users_and_groups = [
                User.create_with_group(
                    UserInfo(username=name, full_name=name, email=name + "@example.com")
                )
                for name in names
            ]
            db.session.flush()
            t1gid = users_and_groups[0][1].id
            uids = [u.id for u, g in users_and_groups]
            db.session.commit()
            groupname = f"testgroup1{str(is_admin).lower()}"
            self.get(
                f"/groups/show/{groupname}",
                expect_status=400,
                expect_content={"error": f'User group "{groupname}" not found'},
            )
            self.get(f"/groups/create/{groupname}")
            self.get(
                f"/groups/create/{groupname}",
                expect_content={"error": "User group already exists."},
                expect_status=400,
            )
            self.json_post(
                f"/groups/addmember/{groupname}",
                {"names": f"{t1},{t3}".split(",")},
                expect_content={
                    "added": [t1, t3],
                    "already_belongs": [],
                    "not_exist": [],
                },
            )
            self.json_post(
                f"/groups/addmember/{groupname}",
                {"names": f"{t1},{t3}".split(",")},
                expect_content={
                    "already_belongs": [t1, t3],
                    "added": [],
                    "not_exist": [],
                },
            )
            self.json_post(
                f"/groups/addmember/{groupname}",
                {"names": f"{t1},{t2},{t3},{t4}@example.com,{t4},{t5}".split(",")},
                expect_content={
                    "already_belongs": [t1, t3],
                    "added": [t2, t4],
                    "not_exist": [t5],
                },
            )
            self.json_post(
                f"/groups/addmember/{groupname}",
                {"names": f"{t1},{t2}".split(",")},
                expect_content={
                    "already_belongs": [t1, t2],
                    "added": [],
                    "not_exist": [],
                },
            )
            ug = UserGroup.get_by_name(groupname)
            if is_admin:
                self.get(
                    f"/groups/usergroups/{t1}",
                    expect_content=[
                        {"id": t1gid, "name": t1},
                        {"id": ug.id, "name": groupname},
                    ],
                )
                self.get(
                    f"/groups/belongs/{t1}/{groupname}", expect_content={"status": True}
                )
            else:
                self.get(
                    f"/groups/usergroups/{t1}",
                    expect_status=403,
                    expect_content="This action requires administrative rights.",
                )
                self.get(
                    f"/groups/belongs/{t1}/{groupname}",
                )

            self.get(
                f"/groups/show/{groupname}",
                expect_content=[
                    {
                        "email": t1 + "@example.com",
                        "id": uids[0],
                        "name": names[0],
                        "real_name": names[0],
                    },
                    {
                        "email": t2 + "@example.com",
                        "id": uids[1],
                        "name": names[1],
                        "real_name": names[1],
                    },
                    {
                        "email": t3 + "@example.com",
                        "id": uids[2],
                        "name": names[2],
                        "real_name": names[2],
                    },
                    {
                        "email": t4 + "@example.com",
                        "id": uids[3],
                        "name": names[3],
                        "real_name": names[3],
                    },
                ],
            )

            self.json_post(
                f"/groups/removemember/{groupname}",
                {"names": f"{t1},{t3}".split(",")},
                expect_content={
                    "removed": [t1, t3],
                    "does_not_belong": [],
                    "not_exist": [],
                },
            )
            self.json_post(
                f"/groups/removemember/{groupname}",
                {"names": f"{t1},{t3}".split(",")},
                expect_content={
                    "removed": [],
                    "does_not_belong": [t1, t3],
                    "not_exist": [],
                },
            )
            self.json_post(
                f"/groups/removemember/{groupname}",
                {"names": f"{t1},{t2},{t3},{t4},{t5}".split(",")},
                expect_content={
                    "removed": [t2, t4],
                    "does_not_belong": [t1, t3],
                    "not_exist": [t5],
                },
            )
            self.get(f"/groups/show/{groupname}", expect_content=[])

    def init_admin(self):
        u = self.test_user_3
        self.make_admin(u)
        self.login_test3()
        return True

    def init_groupadmin(self):
        u = self.test_user_2
        u.add_to_group(UserGroup.get_groupadmin_group(), added_by=None)
        db.session.commit()
        self.login_test2()
        return False

    def test_invalid_groups(self):
        for is_admin in self.enum_admin_and_groupadmin():
            is_admin_postfix = str(is_admin).lower()
            self.get(
                "/groups/create/testgroup",
                expect_status=400,
                expect_content=self.error_resp("testgroup"),
            )
            self.get(
                "/groups/create/1",
                expect_status=400,
                expect_content=self.error_resp("1"),
            )
            self.get(
                "/groups/create/a1@a",
                expect_status=400,
                expect_content=self.error_resp("a1@a"),
            )
            self.get(
                "/groups/create/ok ok",
                expect_status=400,
                expect_content=self.error_resp("ok ok"),
            )
            self.get(
                "/groups/create/TestGroup1",
                expect_status=400,
                expect_content=self.error_resp("TestGroup1"),
            )
            self.get(f"/groups/create/test x1{is_admin_postfix}")

            self.json_post(
                "/groups/addmember/Logged-in users",
                {"names": f"testuser1".split(",")},
                expect_status=400,
                expect_content={"error": "Cannot edit special group: Logged-in users"},
            )
            if not is_admin:
                self.json_post(
                    "/groups/addmember/Group admins",
                    {"names": f"testuser1".split(",")},
                    expect_status=403,
                    expect_content={
                        "error": "This action requires administrative rights."
                    },
                )
                self.json_post(
                    "/groups/addmember/Administrators",
                    {"names": f"testuser1".split(",")},
                    expect_status=403,
                    expect_content={
                        "error": "This action requires administrative rights."
                    },
                )
                self.json_post(
                    "/groups/removemember/Administrators",
                    {"names": f"testuser1".split(",")},
                    expect_status=403,
                    expect_content={
                        "error": "This action requires administrative rights."
                    },
                )
            else:
                self.json_post(
                    "/groups/addmember/Group admins", {"names": f"testuser1".split(",")}
                )
            self.json_post(
                "/groups/addmember/testuser1",
                {"names": f"testuser2".split(",")},
                expect_status=400,
                expect_content={"error": "Cannot edit personal group: testuser1"},
            )
            self.json_post(
                "/groups/removemember/testuser1",
                {"names": f"testuser1".split(",")},
                expect_status=400,
                expect_content={"error": "Cannot edit personal group: testuser1"},
            )
            self.json_post(
                "/groups/removemember/Logged-in users",
                {"names": f"testuser1".split(",")},
                expect_status=400,
                expect_content={"error": "Cannot edit special group: Logged-in users"},
            )
            self.json_post(
                f"/groups/addmember/test x1{is_admin_postfix}",
                {"names": ["Anonymous"]},
                expect_status=400,
                expect_content={"error": "Cannot add special users."},
            )
            self.json_post(
                f"/groups/addmember/test x1{is_admin_postfix}",
                {"names": ["Logged-in user"]},
                expect_status=400,
                expect_content={"error": "Cannot add special users."},
            )

    def test_nonexistent(self):
        self.init_admin()
        self.get(
            f"/groups/belongs/asd/testuser1",
            expect_content={"error": "User not found"},
            expect_status=404,
        )
        self.get(
            f"/groups/belongs/testuser1/asd",
            expect_content={"error": 'User group "asd" not found'},
            expect_status=400,
        )
        self.get(
            f"/groups/usergroups/asd",
            expect_content={"error": "User not found"},
            expect_status=404,
        )
        self.json_post(
            f"/groups/addmember/asd",
            {"names": f"testuser1".split(",")},
            expect_content={"error": 'User group "asd" not found'},
            expect_status=400,
        )

    def test_groups_add_create_missing(self):
        """
        Test create_missing_users option when adding users to a group.
        """
        self.init_groupadmin()

        groupname = "test_add_create_missing1"
        self.get(f"/groups/create/{groupname}")

        # Cannot create missing users if setting not set
        self.json_post(
            f"/groups/addmember/{groupname}",
            {
                "names": ["test_add_create_user1@test.com"],
                "create_missing_users": True,
            },
            expect_status=400,
            expect_content={"error": "Creating missing users is not allowed."},
        )

        with self.temp_config({"GROUPS_MISSING_USER_CREATE_ALLOW": True}):
            self.json_post(
                f"/groups/addmember/{groupname}",
                {
                    "names": ["test_add_create_user1@test.com"],
                    "create_missing_users": True,
                },
                expect_content={
                    "added": ["test_add_create_user1@test.com"],
                    "already_belongs": [],
                    "not_exist": [],
                },
            )

            u = User.get_by_email("test_add_create_user1@test.com")
            self.assertIsNotNone(u)
            self.assertEqual(u.name, "test_add_create_user1@test.com")
            self.assertEqual(u.email, "test_add_create_user1@test.com")

    def test_groups_add_notify(self):
        """
        Test notify option when adding users to a group.
        """

        self.init_groupadmin()
        sent_mails_in_testing.clear()

        groupname = "test_add_notify1"
        self.get(f"/groups/create/{groupname}")

        with self.temp_config(
            {
                "GROUPS_MISSING_USER_CREATE_ALLOW": True,
                "GROUPS_EXISTING_USER_ADD_NOTIFY_EXISTING_HEAD": "GROUPS_EXISTING_USER_ADD_NOTIFY_EXISTING_HEAD",
                "GROUPS_EXISTING_USER_ADD_NOTIFY_EXISTING_BODY": "GROUPS_EXISTING_USER_ADD_NOTIFY_EXISTING_BODY",
                "GROUPS_MISSING_USER_ADD_NOTIFY_HEAD": "GROUPS_MISSING_USER_ADD_NOTIFY_HEAD",
                "GROUPS_MISSING_USER_ADD_NOTIFY_BODY": "GROUPS_MISSING_USER_ADD_NOTIFY_BODY",
            }
        ):
            self.json_post(
                "/groups/addmember/test_add_notify1",
                {
                    "names": [
                        self.test_user_1.name,
                        "test_add_notify_non_existing1@test.com",
                    ],
                    "notify_new": True,
                    "create_missing_users": True,
                },
                expect_content={
                    "added": [
                        "test_add_notify_non_existing1@test.com",
                        self.test_user_1.name,
                    ],
                    "already_belongs": [],
                    "not_exist": [],
                },
            )

            self.assertEqual(len(sent_mails_in_testing), 2)
            self.assertEqual(
                {
                    (
                        "GROUPS_EXISTING_USER_ADD_NOTIFY_EXISTING_HEAD",
                        "GROUPS_EXISTING_USER_ADD_NOTIFY_EXISTING_BODY",
                    ),
                    (
                        "GROUPS_MISSING_USER_ADD_NOTIFY_HEAD",
                        "GROUPS_MISSING_USER_ADD_NOTIFY_BODY",
                    ),
                },
                {(m["subject"], m["msg"]) for m in sent_mails_in_testing},
            )

    def test_groups_trim(self):
        self.init_admin()
        self.get("/groups/create/testing1")
        self.json_post(
            "/groups/addmember/testing1",
            {"names": f"testuser1 ,testuser2  ".split(",")},
            expect_content={
                "added": ["testuser1", "testuser2"],
                "already_belongs": [],
                "not_exist": [],
            },
        )

    def test_invalid_group_setting(self):
        self.login_test1()
        d = self.create_doc(settings={"group": ["a", "b"]})
        html = self.get(d.get_url_for_view("teacher"))
        self.assertIn("The setting &#39;group&#39; must be a string", html)

    def test_doc_group_setting_access(self):
        self.login_test1()
        no_access_msg = "You don&#39;t have access to group &#39;testuser1&#39;."
        d = self.create_doc(
            settings={"group": self.current_user.get_personal_group().name}
        )
        self.assertNotIn(no_access_msg, self.get(d.get_url_for_view("teacher")))
        self.test_user_2.grant_access(d, AccessType.teacher)
        db.session.commit()
        self.login_test2()
        self.assertIn(no_access_msg, self.get(d.get_url_for_view("teacher")))

    def test_group_member_sync(self):
        self.login_test1()
        UserGroup.get_or_create_group("test_group_sync")
        db.session.commit()

        with self.temp_config(
            {
                "SYNC_USER_GROUPS_SEND_SECRET": "xxx",
                "SYNC_USER_GROUPS_RECEIVE_SECRET": "xxx",
                "SYNC_USER_GROUPS_HOSTS": [
                    f'http://{app.config["INTERNAL_PLUGIN_DOMAIN"]}:5001'
                ],
            }
        ):
            with patch.object(
                tim_celery.sync_user_group_memberships,
                "delay",
                wraps=tim_celery.do_send_user_group_info,
            ) as m:  # type: Mock
                with self.internal_container_ctx():
                    tim_celery.sync_user_group_memberships.delay(
                        self.test_user_1.email,
                        [ug.name for ug in self.test_user_1.groups]
                        + ["test_group_sync"],
                    )
                    db.session.commit()
                    self.assertEqual(
                        {
                            u.name
                            for u in UserGroup.get_by_name("test_group_sync").users
                        },
                        {self.test_user_1.name},
                    )

                    tim_celery.sync_user_group_memberships.delay(
                        self.test_user_1.email,
                        [
                            ug.name
                            for ug in self.test_user_1.groups
                            if ug.name != "test_group_sync"
                        ],
                    )
                    db.session.commit()
                    self.assertEqual(
                        len(
                            [
                                u.name
                                for u in UserGroup.get_by_name("test_group_sync").users
                            ]
                        ),
                        0,
                    )
                self.assertEqual(2, m.call_count)


class GroupTest2(TimRouteTest):
    def test_group_edit_access(self):
        self.test_user_3.make_admin()
        db.session.commit()
        self.login_test3()
        self.get("/groups/create/edittest1")
        d = DocEntry.find_by_path("groups/edittest1")
        self.assertEqual("Administrators", d.parent.owners[0].name)
        self.assertEqual(
            {"group": "edittest1", "fields": ["info"], "maxRows": "40em"},
            d.document.get_settings().get_dict()["macros"],
        )
        self.login_test1()
        self.get("/groups/show/edittest1", expect_status=403)
        self.test_user_1.grant_access(d, AccessType.view)
        db.session.commit()
        self.get(d.url)
        self.get(d.parent.url)
        self.get("/groups/show/edittest1")
        self.json_post(
            "/groups/addmember/edittest1",
            {"names": f"testuser1".split(",")},
            expect_status=403,
        )
        self.test_user_1.grant_access(d, AccessType.edit)
        db.session.commit()
        self.json_post(
            "/groups/addmember/edittest1",
            {"names": f"testuser1".split(",")},
            expect_content={
                "added": ["testuser1"],
                "already_belongs": [],
                "not_exist": [],
            },
        )


class GroupTest3(TimRouteTest):
    def test_create_group(self):
        self.login_test1()  # This was needed to create user1 with group.

        # Test user 1:
        # Create a user with group administrator rights and login.
        # Luodaan testikäyttäjä ryhmänhallinnan oikeuksilla ja kirjataan sisään.
        user1, _ = User.create_with_group(
            UserInfo(username="user1", email="user1@jyu.fi")
        )
        user1.add_to_group(UserGroup.get_groupadmin_group(), added_by=None)
        db.session.commit()
        self.login(username=user1.name)

        # Test case 1:
        # Users should not have a right to create new subdirectories directly to the groups root folder.
        # Ei pitäisi olla oikeutta luoda käyttäjäryhmiä suoraan juurihakemiston (groups) alihakemistoon.
        self.get(
            "/groups/create/kurssit/tie/ohj2/2022k",
            expect_status=403,
            expect_content={"error": "You cannot create documents in this folder."},
        )

        # Test case 2:
        # Users are able to create user groups to the subdirectories they already have manage access rights to.
        # Käyttäjät voivat luoda käyttäjäryhmiä juurihakemiston (groups) alihakemistoon, jos siihen on oikeus.
        Folder.create(
            "groups/kurssit/tie/ohj2/2022k/tentit", user1.get_personal_group()
        )
        db.session.commit()
        new_group = self.get(
            "/groups/create/kurssit/tie/ohj2/2022k/tentit/ohj2_valikoe_zoom"
        )
        self.assertEqual(
            new_group["path"],
            "groups/kurssit/tie/ohj2/2022k/tentit/ohj2_valikoe_zoom",
        )
        existing_folder = Folder.find_by_path("groups/kurssit/tie/ohj2/2022k/tentit")
        self.assertIn(
            user1.get_personal_group().name,
            [group.name for group in existing_folder.owners],
        )

        # Test case 3:
        # Users are able to create new subdirectories to the directories they have manage access rights to.
        # Käyttäjät voivat luoda hakemistoon uusia alihakemistoja ja saavat niihin omistusoikeuden.
        self.get("/groups/create/tie/ohj2/2021s/ohj2_ohjaajat")
        new_folder = Folder.find_by_path("groups/tie/ohj2/2021s")
        self.assertIn(
            user1.get_personal_group().name, [group.name for group in new_folder.owners]
        )

        # Test user 2:
        # Create a user with group administrator rights and login.
        # Luodaan testikäyttäjä ryhmänhallinnan oikeuksilla ja kirjataan sisään.
        user2, _ = User.create_with_group(
            UserInfo(username="user2", email="user2@jyu.fi"),
        )
        user2.add_to_group(UserGroup.get_groupadmin_group(), added_by=None)
        db.session.commit()
        self.login(username=user2.name)

        # Test case 4:
        # Another user may not create a group in a subdirectory to which user has no rights.
        # Toinen käyttäjä ei saa luoda ryhmää alihakemistoon, johon hänellä ei ole oikeutta.
        self.get(
            "/groups/create/kurssit/tie/ohj2/2021s",
            expect_status=403,
            expect_content={"error": "You cannot create documents in this folder."},
        )


class GroupNotificationsTest(TimRouteTest):
    """
    Tests that various group notifications work.
    """

    def test_group_join_message(self) -> None:
        self.make_admin(self.test_user_1)
        self.login_test1()

        gname = "group_join_msg_test1"

        self.get(f"/groups/create/{gname}")

        ug = UserGroup.get_by_name(gname)
        self.assertIsNotNone(ug)

        ug_doc: DocEntry = ug.admin_doc.docentries[0]

        self.add_answer(
            ug_doc, JOIN_MESSAGE_TASKID, content="Test message!", content_key="usercode"
        )
        db.session.commit()

        self.json_post(
            f"/groups/addmember/{gname}",
            {"names": f"testuser2".split(",")},
            expect_status=200,
        )
        self.assertEqual(1, len(sent_mails_in_testing), "Should send welcome mail.")
        self.assertEqual(
            sent_mails_in_testing[0]["subject"],
            f"TIM: You were added to group '{gname}'",
        )
        self.assertEqual(
            sent_mails_in_testing[0]["msg"],
            "Test message!",
        )

        self.json_post(
            f"/groups/removemember/{gname}",
            {"names": f"testuser2".split(",")},
            expect_status=200,
        )
        self.assertEqual(
            1, len(sent_mails_in_testing), "Should not send welcome mail after removal."
        )

        self.json_post(
            f"/groups/addmember/{gname}",
            {"names": f"testuser2".split(",")},
            expect_status=200,
        )
        self.assertEqual(
            2, len(sent_mails_in_testing), "Should send welcome mail again."
        )
