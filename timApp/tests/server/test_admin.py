from timApp.admin.user_cli import (
    find_and_merge_users,
    find_and_soft_delete,
    do_merge_users,
    do_soft_delete,
)
from timApp.document.docentry import DocEntry
from timApp.messaging.messagelist.listinfo import Channel
from timApp.tests.db.timdbtest import TEST_USER_1_ID, TEST_USER_2_ID, TEST_USER_3_ID
from timApp.tests.server.timroutetest import TimRouteTest
from timApp.timdb.sqa import db
from timApp.user.special_group_names import SPECIAL_USERNAMES
from timApp.user.user import User, UserInfo
from timApp.user.usercontact import ContactOrigin
from timApp.user.usergroup import UserGroup, get_admin_group_id
from timApp.util.flask.requesthelper import RouteException, NotExist


class SearchTest(TimRouteTest):
    def test_user_search(self):
        self.login_test1()
        self.get("/users/search/test", expect_status=403)
        self.make_admin(self.current_user)
        self.get(
            "/users/search/test",
            expect_content=[
                {
                    "email": "test1@example.com",
                    "id": TEST_USER_1_ID,
                    "name": "testuser1",
                    "real_name": "Test user 1",
                    "contacts": [
                        {
                            "channel": "email",
                            "contact": "test1@example.com",
                            "origin": 1,
                            "primary": True,
                            "verified": True,
                        }
                    ],
                },
                {
                    "email": "test2@example.com",
                    "id": TEST_USER_2_ID,
                    "name": "testuser2",
                    "real_name": "Test user 2",
                    "contacts": [
                        {
                            "channel": "email",
                            "contact": "test2@example.com",
                            "origin": 1,
                            "primary": True,
                            "verified": True,
                        }
                    ],
                },
                {
                    "email": "test3@example.com",
                    "id": TEST_USER_3_ID,
                    "name": "testuser3",
                    "real_name": "Test user 3",
                    "contacts": [
                        {
                            "channel": "email",
                            "contact": "test3@example.com",
                            "origin": 1,
                            "primary": True,
                            "verified": True,
                        }
                    ],
                },
            ],
        )

        self.get(
            "/users/search/test?full=true",
            expect_content=[
                {
                    "consent": None,
                    "contacts": [
                        {
                            "channel": "email",
                            "contact": "test1@example.com",
                            "origin": 1,
                            "primary": True,
                            "verified": True,
                        }
                    ],
                    "email": "test1@example.com",
                    "folder": {
                        "id": self.test_user_1.get_personal_folder().id,
                        "isFolder": True,
                        "location": "users",
                        "modified": "just now",
                        "name": "test-user-1",
                        "owners": [
                            {
                                "id": self.test_user_1.get_personal_group().id,
                                "name": "testuser1",
                            }
                        ],
                        "path": "users/test-user-1",
                        "public": True,
                        "rights": {
                            "browse_own_answers": True,
                            "can_comment": True,
                            "can_mark_as_read": True,
                            "copy": True,
                            "editable": True,
                            "manage": True,
                            "owner": True,
                            "see_answers": True,
                            "teacher": True,
                        },
                        "title": "Test user 1",
                        "unpublished": True,
                    },
                    "group": {
                        "id": self.test_user_1.get_personal_group().id,
                        "name": "testuser1",
                    },
                    "groups": [
                        {
                            "external_id": None,
                            "id": self.test_user_1.get_personal_group().id,
                            "name": "testuser1",
                        },
                        {
                            "external_id": None,
                            "id": get_admin_group_id(),
                            "name": "Administrators",
                        },
                    ],
                    "id": TEST_USER_1_ID,
                    "last_name": None,
                    "name": "testuser1",
                    "real_name": "Test user 1",
                    "tos_accepted_at": None,
                },
                {
                    "consent": None,
                    "contacts": [
                        {
                            "channel": "email",
                            "contact": "test2@example.com",
                            "origin": 1,
                            "primary": True,
                            "verified": True,
                        }
                    ],
                    "email": "test2@example.com",
                    "folder": {
                        "id": self.test_user_2.get_personal_folder().id,
                        "isFolder": True,
                        "location": "users",
                        "modified": "just now",
                        "name": "test-user-2",
                        "owners": [
                            {
                                "id": self.test_user_2.get_personal_group().id,
                                "name": "testuser2",
                            }
                        ],
                        "path": "users/test-user-2",
                        "public": True,
                        "rights": {
                            "browse_own_answers": True,
                            "can_comment": True,
                            "can_mark_as_read": True,
                            "copy": True,
                            "editable": True,
                            "manage": True,
                            "owner": True,
                            "see_answers": True,
                            "teacher": True,
                        },
                        "title": "Test user 2",
                        "unpublished": True,
                    },
                    "group": {
                        "id": self.test_user_2.get_personal_group().id,
                        "name": "testuser2",
                    },
                    "groups": [
                        {
                            "external_id": None,
                            "id": self.test_user_2.get_personal_group().id,
                            "name": "testuser2",
                        }
                    ],
                    "id": TEST_USER_2_ID,
                    "last_name": None,
                    "name": "testuser2",
                    "real_name": "Test user 2",
                    "tos_accepted_at": None,
                },
                {
                    "consent": None,
                    "contacts": [
                        {
                            "channel": "email",
                            "contact": "test3@example.com",
                            "origin": 1,
                            "primary": True,
                            "verified": True,
                        }
                    ],
                    "email": "test3@example.com",
                    "folder": {
                        "id": self.test_user_3.get_personal_folder().id,
                        "isFolder": True,
                        "location": "users",
                        "modified": "just now",
                        "name": "test-user-3",
                        "owners": [
                            {
                                "id": self.test_user_3.get_personal_group().id,
                                "name": "testuser3",
                            }
                        ],
                        "path": "users/test-user-3",
                        "public": True,
                        "rights": {
                            "browse_own_answers": True,
                            "can_comment": True,
                            "can_mark_as_read": True,
                            "copy": True,
                            "editable": True,
                            "manage": True,
                            "owner": True,
                            "see_answers": True,
                            "teacher": True,
                        },
                        "title": "Test user 3",
                        "unpublished": True,
                    },
                    "group": {
                        "id": self.test_user_3.get_personal_group().id,
                        "name": "testuser3",
                    },
                    "groups": [
                        {
                            "external_id": None,
                            "id": self.test_user_3.get_personal_group().id,
                            "name": "testuser3",
                        }
                    ],
                    "id": TEST_USER_3_ID,
                    "last_name": None,
                    "name": "testuser3",
                    "real_name": "Test user 3",
                    "tos_accepted_at": None,
                },
            ],
        )


class MergeTest(TimRouteTest):
    def test_user_merge(self):
        self.login_test1()
        with self.assertRaises(RouteException):
            find_and_merge_users("testuser1", "testuser1")
        User.create_with_group(
            UserInfo(
                username="someguy", full_name="Some Guy", email="some.guy@example.com"
            )
        )
        db.session.commit()
        with self.assertRaises(RouteException):
            find_and_merge_users("testuser1", "someguy")
        with self.assertRaises(NotExist):
            find_and_merge_users("testuser1", "x")
        for u in SPECIAL_USERNAMES:
            with self.assertRaises(RouteException):
                find_and_merge_users("testuser1", u)

        d = self.create_doc(initial_par="#- {plugin=textfield #t}")
        self.post_answer("textfield", f"{d.id}.t", user_input={"c": "x"})
        path = d.path
        tg1 = UserGroup.create("tg1")
        tg2 = UserGroup.create("tg2")
        self.test_user_1.add_to_group(tg1, added_by=self.test_user_3)
        self.test_user_1.add_to_group(tg2, added_by=None)
        t1pg = self.test_user_1.get_personal_group()
        t2pg = self.test_user_2.get_personal_group()
        db.session.commit()

        def membership_set(user: User):
            return {(m.usergroup_id, m.added_by) for m in user.memberships}

        def check_memberships(
            primary: User,
            secondary: User,
            primary_personal: UserGroup,
            secondary_personal: UserGroup,
        ):
            db.session.refresh(primary)
            db.session.refresh(secondary)
            self.assertEqual(
                {(primary_personal.id, None), (tg1.id, TEST_USER_3_ID), (tg2.id, None)},
                membership_set(primary),
            )
            self.assertEqual({(secondary_personal.id, None)}, membership_set(secondary))

        def check_contacts(
            primary: User, secondary: User, extra_emails: list[str] = None
        ):
            db.session.refresh(primary)
            db.session.refresh(secondary)
            extra_emails = extra_emails or []
            self.assertEqual(
                {(primary.email, Channel.EMAIL), (secondary.email, Channel.EMAIL)}
                | {(e, Channel.EMAIL) for e in extra_emails},
                {(c.contact, c.channel) for c in primary.contacts},
            )
            self.assertEqual(
                {(secondary.email, Channel.EMAIL)},
                {(c.contact, c.channel) for c in secondary.contacts},
            )

        r = find_and_merge_users("testuser2", "testuser1")
        self.assertEqual(1, r.accesses)
        self.assertEqual(0, r.annotations)
        self.assertEqual(1, r.answers)
        self.assertEqual(0, r.lectureanswers)
        self.assertEqual(0, r.messages)
        self.assertEqual(0, r.notes)
        self.assertEqual(0, r.owned_lectures)
        self.assertEqual(0, r.readparagraphs)
        self.assertEqual(0, r.velps)
        self.assertEqual(2, r.groups)
        self.assertEqual(1, r.contacts)
        db.session.commit()
        self.assertIsNone(DocEntry.find_by_path(path))
        check_memberships(self.test_user_2, self.test_user_1, t2pg, t1pg)
        check_contacts(self.test_user_2, self.test_user_1)

        r = find_and_merge_users("testuser2", "testuser1")
        self.assertEqual(0, r.accesses)
        self.assertEqual(0, r.annotations)
        self.assertEqual(0, r.answers)
        self.assertEqual(0, r.lectureanswers)
        self.assertEqual(0, r.messages)
        self.assertEqual(0, r.notes)
        self.assertEqual(0, r.owned_lectures)
        self.assertEqual(0, r.readparagraphs)
        self.assertEqual(0, r.velps)
        self.assertEqual(0, r.groups)
        self.assertEqual(0, r.contacts)
        db.session.commit()
        check_memberships(self.test_user_2, self.test_user_1, t2pg, t1pg)
        check_contacts(self.test_user_2, self.test_user_1)

        r = find_and_merge_users("testuser1", "testuser2")
        self.assertEqual(1, r.accesses)
        self.assertEqual(0, r.annotations)
        self.assertEqual(1, r.answers)
        self.assertEqual(0, r.lectureanswers)
        self.assertEqual(0, r.messages)
        self.assertEqual(0, r.notes)
        self.assertEqual(0, r.owned_lectures)
        self.assertEqual(0, r.readparagraphs)
        self.assertEqual(0, r.velps)
        self.assertEqual(2, r.groups)
        self.assertEqual(1, r.contacts)
        db.session.commit()
        check_memberships(self.test_user_1, self.test_user_2, t1pg, t2pg)
        check_contacts(self.test_user_1, self.test_user_2)
        self.assertIsNotNone(DocEntry.find_by_path(path))

        r = find_and_merge_users("testuser1", "testuser2")
        self.assertEqual(0, r.accesses)
        self.assertEqual(0, r.annotations)
        self.assertEqual(0, r.answers)
        self.assertEqual(0, r.lectureanswers)
        self.assertEqual(0, r.messages)
        self.assertEqual(0, r.notes)
        self.assertEqual(0, r.owned_lectures)
        self.assertEqual(0, r.readparagraphs)
        self.assertEqual(0, r.velps)
        self.assertEqual(0, r.groups)
        self.assertEqual(0, r.contacts)
        db.session.commit()
        test_user_2 = self.test_user_2
        check_memberships(self.test_user_1, test_user_2, t1pg, t2pg)
        check_contacts(self.test_user_1, self.test_user_2)

        self.test_user_2.set_emails(
            ["some_email@example.com"],
            ContactOrigin.Custom,
            remove=False,
            force_verify=True,
        )

        r = find_and_merge_users("testuser1", "testuser2")
        self.assertEqual(0, r.accesses)
        self.assertEqual(0, r.annotations)
        self.assertEqual(0, r.answers)
        self.assertEqual(0, r.lectureanswers)
        self.assertEqual(0, r.messages)
        self.assertEqual(0, r.notes)
        self.assertEqual(0, r.owned_lectures)
        self.assertEqual(0, r.readparagraphs)
        self.assertEqual(0, r.velps)
        self.assertEqual(0, r.groups)
        self.assertEqual(1, r.contacts)
        db.session.commit()
        test_user_2 = self.test_user_2
        check_memberships(self.test_user_1, test_user_2, t1pg, t2pg)
        check_contacts(self.test_user_1, self.test_user_2, ["some_email@example.com"])

        find_and_soft_delete("testuser2")
        self.assertIsNone(User.get_by_name("testuser2"))
        u = User.get_by_name(f"testuser2_deleted_{test_user_2.id}")
        db.session.refresh(u)
        self.assertIsNotNone(u)
        self.assertEqual(u.email, u.primary_email_contact.contact)
        with self.assertRaises(RouteException):
            find_and_soft_delete("testuser2")
        with self.assertRaises(RouteException):
            find_and_soft_delete(f"testuser2_deleted_{test_user_2.id}")

    def test_merge_multi(self):
        u1, ug1 = User.create_with_group(
            UserInfo(username="user1", email="user1@example.com", full_name="User 1")
        )

        u1_new, ug1_new = User.create_with_group(
            UserInfo(
                username="user1_new",
                email="user1_new@example.com",
                full_name="User 1 New",
            )
        )

        db.session.commit()

        # First, merge two users
        do_merge_users(u1_new, u1)
        do_soft_delete(u1)
        db.session.commit()
        self.assertEqual(u1.email, f"user1@example.com_deleted_{u1.id}")

        # Create a user again with the same info as original u1
        # This should pass since previous u1 has been disabled
        u1_again, ug1 = User.create_with_group(
            UserInfo(username="user1", email="user1@example.com", full_name="User 1")
        )

        db.session.commit()

        # Simulate u1_new logging in again (e.g. via HAKA)
        do_merge_users(u1_new, u1_again)
        do_soft_delete(u1_again)
        db.session.commit()

        self.assertEqual(u1_again.email, f"user1@example.com_deleted_{u1_again.id}")


class UserDeleteTest(TimRouteTest):
    def test_deleted_user_logout(self):
        u, _ = User.create_with_group(
            UserInfo(username="m@example.com", email="m@example.com", full_name="M")
        )
        db.session.commit()
        self.get("/")
        self.login(username="m@example.com")
        d = self.create_doc()
        find_and_soft_delete("m@example.com")
        db.session.commit()
        self.get(d.url, expect_status=302, expect_content="/")
        self.get(d.url, expect_status=403)
        self.login(username=f"m@example.com_deleted_{u.id}")
        self.post_answer(
            "x",
            f"{d.id}.t",
            user_input={},
            expect_status=403,
            expect_content="Please refresh the page and log in again.",
        )
        self.post_answer(
            "x",
            f"{d.id}.t",
            user_input={},
            expect_status=400,
            expect_content="Task not found in the document: t",
        )
