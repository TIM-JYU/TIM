from timApp.auth.accesshelper import get_doc_or_abort
from timApp.auth.accesstype import AccessType
from timApp.folder.folder import Folder
from timApp.messaging.messagelist.listinfo import ArchiveType, Channel
from timApp.messaging.messagelist.messagelist_models import MessageListModel
from timApp.messaging.messagelist.messagelist_utils import (
    MESSAGE_LIST_ARCHIVE_FOLDER_PREFIX,
)
from timApp.tests.server.test_jsrunner import JsRunnerTestBase
from timApp.tests.server.timroutetest import TimMessageListTest
from timApp.timdb.sqa import db
from timApp.user.special_group_names import ANONYMOUS_GROUPNAME, LOGGED_IN_GROUPNAME
from timApp.user.user import User, UserInfo
from timApp.user.usercontact import ContactOrigin
from timApp.user.usergroup import UserGroup
from timApp.user.verification.verification import SetPrimaryContactVerification


class MessageListTest(TimMessageListTest):
    """Server test for message lists."""

    def test_primary_email_sync(self):
        """Test primary email sync with Mailman"""

        self.login_test1()
        self.make_admin(self.test_user_1)
        list_name = "primary_mail_sync_test1"
        manage_doc, message_list = self.create_list(list_name, ArchiveType.NONE)

        list_member, _ = User.create_with_group(
            UserInfo(
                username="primary_mail_member",
                email="mail1@example.com",
            )
        )
        db.session.commit()
        list_member = User.get_by_name(list_member.name)

        mlist = self.mailman_client.get_list(message_list.email_address)

        def check_message_list():
            self.assertEqual(
                {m.address.email for m in mlist.members},
                {self.test_user_1.email, list_member.email},
                "Message list must have same emails and users in TIM and Mailman",
            )

        self.add_list_member(list_name, [list_member.name])
        check_message_list()
        list_member = User.get_by_name(list_member.name)

        emails_to_test = [
            "secondary_mail1@example.com",
            "secondary_mail2@example.com",
            "secondary_mail3@example.com",
        ]
        list_member.set_emails(
            emails_to_test,
            ContactOrigin.Custom,
            remove=False,
            force_verify=True,
        )
        db.session.commit()

        check_message_list()

        for email in emails_to_test:
            list_member = User.get_by_name(list_member.name)
            db.session.refresh(list_member)
            # Set primary email directly
            list_member.email = email
            db.session.commit()
            check_message_list()

        for email in emails_to_test:
            list_member = User.get_by_name(list_member.name)
            db.session.refresh(list_member)
            # Simulate setting correct email via verification
            SetPrimaryContactVerification(
                contact=list_member.get_contact(Channel.EMAIL, email),
                user=list_member,
                user_id=list_member.id,
            ).approve()
            db.session.commit()
            check_message_list()

    def test_scim_mail_sync(self):
        from timApp.tests.server.test_scim import add_name_parts, a

        eid = "jy-CUR-7777-students"
        display_name = "ITKP103 2022-09-09--2022-12-20: Opiskelijat"
        list_name = "students-7777"
        self.login_test1()
        self.make_admin(self.current_user)
        manage_doc, message_list = self.create_list(list_name, ArchiveType.PUBLIC)
        r = self.json_post(
            "/scim/Groups",
            json_data={
                "externalId": eid,
                "displayName": display_name,
                "members": add_name_parts(
                    [
                        {
                            "value": "sisuuser",
                            "display": "Sisu User",
                            "email": "x@example.com",
                        },
                        {
                            "value": "sisuuser3",
                            "display": "Sisu User 3",
                            "email": "x3@example.com",
                        },
                    ]
                ),
            },
            auth=a,
            expect_status=201,
        )
        group_id = r["id"]

        self.json_post(
            "/sisu/createGroupDocs",
            json_data=[
                {"externalId": eid},
            ],
        )

        ug = UserGroup.get_by_external_id(eid)
        self.add_list_member(list_name, [ug.name])

        mlist = self.mailman_client.get_list(message_list.email_address)
        self.assertEqual(
            len(mlist.members),
            3,
            "New Sisu email list should have owner and its members",
        )

        self.json_put(
            f"/scim/Groups/{group_id}",
            json_data={
                "externalId": eid,
                "displayName": display_name,
                "members": add_name_parts(
                    [
                        {
                            "value": "sisuuser",
                            "display": "Sisu User",
                            "email": "x@example.com",
                        },
                        {
                            "value": "sisuuser3",
                            "display": "Sisu User 3",
                            "email": "x3@example.com",
                        },
                        {
                            "value": "sisuuser4",
                            "display": "Sisu User 4",
                            "email": "x4@example.com",
                        },
                    ]
                ),
            },
            auth=a,
        )

        self.assertEqual(
            len(mlist.members), 4, "Sisu email list should have new added user"
        )

        self.json_put(
            f"/scim/Groups/{group_id}",
            json_data={
                "externalId": eid,
                "displayName": display_name,
                "members": add_name_parts(
                    [
                        {
                            "value": "sisuuser",
                            "display": "Sisu User",
                            "email": "x@example.com",
                        },
                        {
                            "value": "sisuuser3",
                            "display": "Sisu User 3",
                            "email": "x3@example.com",
                        },
                    ]
                ),
            },
            auth=a,
        )

        self.assertEqual(
            len([m for m in mlist.members if m.moderation_action == "reject"]),
            1,
            "One user must be removed from the Sisu email list",
        )

    def test_creation_valid_name(self):
        """Test creating a message list with a valid user and valid list name."""
        # Make testuser1 admin.
        self.login_test1()
        testuser1 = self.current_user
        self.make_admin(testuser1)
        list_name = "valid_list5"
        archive = ArchiveType.PUBLIC
        manage_doc, message_list = self.create_list(list_name, archive)
        # Expected response is in JSON, and it's a DocEntry of the created admin doc. Get the created admin doc of
        # the message list.
        admin_doc = get_doc_or_abort(manage_doc["id"])
        # Verify returned admin doc is the same message list's admin doc in db.
        self.assertEqual(message_list.manage_doc_id, admin_doc.id)
        # Verify name and archive type are as intended in the db.
        self.assertEqual(message_list.name, list_name)
        self.assertEqual(message_list.archive, archive)
        self.assertIsNotNone(Folder.find_by_path(f"archives/{message_list.name}"))

    def test_clear_on_add(self):
        self.make_admin(self.test_user_1)
        self.login_test1()

        list_name = "clear_on_add_test1"
        manage_doc, message_list = self.create_list(list_name, ArchiveType.PUBLIC)

        self.json_post(
            "/messagelist/addmember",
            json_data={
                "msg_list": list_name,
                "clear_list": False,
                "delivery_right": True,
                "send_right": True,
                "member_candidates": ["test1@example.com", "test2@example.com"],
            },
        )

        message_list = MessageListModel.get_by_name(list_name)

        self.assertEqual(
            [m.get_email() for m in message_list.members],
            ["test1@example.com", "test2@example.com"],
        )

        self.json_post(
            "/messagelist/addmember",
            json_data={
                "msg_list": list_name,
                "clear_list": False,
                "delivery_right": True,
                "send_right": True,
                "member_candidates": ["test3@example.com"],
            },
        )

        message_list = MessageListModel.get_by_name(list_name)

        self.assertEqual(
            [m.get_email() for m in message_list.members],
            ["test1@example.com", "test2@example.com", "test3@example.com"],
        )

        self.json_post(
            "/messagelist/addmember",
            json_data={
                "msg_list": list_name,
                "clear_list": True,
                "delivery_right": True,
                "send_right": True,
                "member_candidates": ["test4@example.com"],
            },
        )

        message_list = MessageListModel.get_by_name(list_name)

        self.assertEqual(
            [m.get_email() for m in message_list.members],
            ["test4@example.com"],
        )

    def test_mailman_members(self):
        list_owner, _ = User.create_with_group(
            UserInfo(
                username="list_owner",
                email="list_owner@user.com",
                password="list_owner",
            ),
            is_admin=True,
        )

        list_member, _ = User.create_with_group(
            UserInfo(
                username="list_member",
                email="list_member@user.com",
                password="list_member",
            )
        )
        db.session.commit()

        self.login(list_owner.email, "list_owner", list_owner.name)
        list_name = "testlist1"
        _, message_list = self.create_list(list_name, ArchiveType.PUBLIC)

        mlist = self.mailman_client.get_list(message_list.email_address)

        self.assertEqual(
            len(mlist.members), 1, "New list should have only the owner as the member"
        )
        self.assertEqual(
            len(mlist.owners),
            1,
            "List owner should be counted as owner of the mailing list",
        )

        # Add user to the list
        self.add_list_member(list_name, [list_member.name])
        self.assertEqual(
            {m.address.email for m in mlist.members},
            {list_owner.email, list_member.email},
            "List should have its owner and member added",
        )

        usr = self.mailman_client.get_user(list_member.email)
        self.assertEqual(
            {a.email for a in usr.addresses},
            {list_member.email},
            "TIM user should have its email registered with mailman",
        )

        old_email = list_member.email
        list_member = User.get_by_id(list_member.id)
        list_member.update_info(UserInfo(email="list_member_other@user.com"))
        db.session.commit()

        self.assertEqual(
            {m.address.email for m in mlist.members},
            {list_owner.email, list_member.email},
            "List should have user's new email listed as member",
        )

        self.assertEqual(
            {a.email for a in usr.addresses},
            {list_member.email, old_email},
            "TIM user should still have their old email in mailman database",
        )

    def test_mail_archiving_processing(self):
        self.login_test1()
        self.make_admin(self.test_user_1)
        _, message_list = self.create_list(f"list_archive_test1", ArchiveType.PUBLIC)

        self.trigger_message_send(
            message_list, self.test_user_1, "Test message", "Test message"
        )

        archive_folder = f"{MESSAGE_LIST_ARCHIVE_FOLDER_PREFIX}/{message_list.name}"
        folder = Folder.find_by_path(archive_folder)
        self.assertIsNotNone(folder, f"Archive folder {archive_folder} must be created")

        doc = next(
            (
                d
                for d in folder.get_all_documents()
                if d.short_name.startswith("test-message")
            ),
            None,
        )
        self.assertIsNotNone(
            doc, f"Archived document named 'test-message' must be created"
        )

        doc_count = len(folder.get_all_documents())
        self.trigger_message_send(
            message_list, self.test_user_1, "[SPAM] Test spam message", "Test message"
        )
        self.assertEqual(
            len(folder.get_all_documents()),
            doc_count,
            "Spam messages must not be archived",
        )

        self.trigger_message_send(
            message_list, self.test_user_1, "try/to/fool/title", "Test message"
        )

        doc = next(
            (
                d
                for d in folder.get_all_documents()
                if d.short_name.startswith("try-to-fool-title")
            ),
            None,
        )
        self.assertIsNotNone(
            doc, f"Archived document named 'try-to-fool-title' must be created"
        )

        self.trigger_message_send(
            message_list,
            self.test_user_1,
            f"[{message_list.name}] Test message 2",
            "Test message",
        )

        doc = next(
            (
                d
                for d in folder.get_all_documents()
                if d.short_name.startswith("test-message-2")
            ),
            None,
        )
        self.assertIsNotNone(
            doc, f"Archived document named 'test-message-2' must be created"
        )

        self.trigger_message_send(message_list, self.test_user_1)

        doc = next(
            (
                d
                for d in folder.get_all_documents()
                if d.short_name.startswith("no-subject")
            ),
            None,
        )

        self.assertIsNotNone(
            doc, f"Archived document named 'no-subject' must be created"
        )

    def test_mail_archive_access(self):
        self.login_test1()
        self.make_admin(self.test_user_1)

        for atype in (ArchiveType.PUBLIC, ArchiveType.GROUPONLY, ArchiveType.UNLISTED):
            _, message_list = self.create_list(
                f"list_access_{atype.name.lower()}1", atype
            )

            self.add_list_member(message_list.name, [self.test_user_2.name])

            self.trigger_message_send(
                message_list, self.test_user_2, "Test message", "This is a test message"
            )

            archive_folder = f"{MESSAGE_LIST_ARCHIVE_FOLDER_PREFIX}/{message_list.name}"
            folder = Folder.find_by_path(archive_folder)
            self.assertIsNotNone(
                folder, f"Archive folder {archive_folder} must be created"
            )

            docs = folder.get_all_documents()
            self.assertEqual(len(docs), 1, "Sent message should be archived")

            archive_doc = docs[0]
            self.assertEqual(
                archive_doc.title,
                "Test message",
                "Archived message must have the same title as the message subject",
            )

            self.assertEqual(
                {
                    a.usergroup.name
                    for a in archive_doc.block.accesses.values()
                    if a.access_type == AccessType.owner
                },
                {self.test_user_1.name, self.test_user_2.name},
            )

            expected_viewers = {}
            if atype == ArchiveType.PUBLIC:
                expected_viewers = {ANONYMOUS_GROUPNAME}
            elif atype == ArchiveType.UNLISTED:
                expected_viewers = {LOGGED_IN_GROUPNAME}
            elif atype == ArchiveType.GROUPONLY:
                # List owner is not counted as a normal messagelist member
                # List owner gets full access to the archives either way
                expected_viewers = {self.test_user_2.name}

            self.assertEqual(
                {
                    a.usergroup.name
                    for a in archive_doc.block.accesses.values()
                    if a.access_type == AccessType.view
                },
                expected_viewers,
            )

            self.assertEqual(
                len(
                    [
                        a
                        for a in archive_doc.block.accesses.values()
                        if a.access_type != AccessType.owner
                        and a.access_type != AccessType.view
                    ]
                ),
                0,
                "Archived message must only have view and owner access types",
            )


class JSRunnerMessageListTest(TimMessageListTest, JsRunnerTestBase):
    def test_jsrunner_mail_sync(self):
        """Test JSRunner group actions sync with mailman"""

        self.login_test1()
        self.make_admin(self.current_user)
        manage_doc, message_list = self.create_list(
            "test-jsrunner-list-1", ArchiveType.PUBLIC
        )
        mlist = self.mailman_client.get_list(message_list.email_address)

        d = self.create_group_jsrun([self.test_user_2.id], group="jg1")
        self.do_jsrun(d)

        self.assertIsNotNone(
            UserGroup.get_by_name("jg1"), "Group jg1 should be created"
        )
        self.add_list_member("test-jsrunner-list-1", ["jg1"])

        self.assertEqual(
            {m.address.email for m in mlist.members},
            {self.test_user_1.email, self.test_user_2.email},
            "Group jg1 should have testuser1 and testuser2",
        )

        d = self.create_group_jsrun(
            [self.test_user_3.id], group="jg1", method="addToGroup"
        )
        self.do_jsrun(d)

        self.assertEqual(
            {m.address.email for m in mlist.members},
            {self.test_user_1.email, self.test_user_2.email, self.test_user_3.email},
            "Group jg1 should have testuser3 added",
        )

        d = self.create_group_jsrun(
            [self.test_user_2.id], group="jg1", method="removeFromGroup"
        )
        self.do_jsrun(d)
        self.assertEqual(
            {m.address.email for m in mlist.members},
            {self.test_user_1.email, self.test_user_3.email},
            "Group jg1 should have testuser2 removed",
        )

        d = self.create_group_jsrun([self.test_user_2.id], group="jg1")
        self.do_jsrun(d)
        self.assertEqual(
            {m.address.email for m in mlist.members},
            {self.test_user_1.email, self.test_user_2.email},
            "Group jg1 should have testuser3 removed and testuser2 added",
        )
