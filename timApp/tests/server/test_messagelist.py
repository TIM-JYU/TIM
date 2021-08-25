from timApp.auth.accesshelper import get_doc_or_abort
from timApp.auth.accesstype import AccessType
from timApp.folder.folder import Folder
from timApp.messaging.messagelist.listinfo import ArchiveType
from timApp.messaging.messagelist.messagelist_utils import MESSAGE_LIST_ARCHIVE_FOLDER_PREFIX
from timApp.tests.server.timroutetest import TimMessageListTest
from timApp.timdb.sqa import db
from timApp.user.special_group_names import ANONYMOUS_GROUPNAME, LOGGED_IN_GROUPNAME
from timApp.user.user import User, UserInfo


class MessageListTest(TimMessageListTest):
    """Server test for message lists."""

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
        # Verify name and archive type are as intented in the db.
        self.assertEqual(message_list.name, list_name)
        self.assertEqual(message_list.archive, archive)
        self.assertIsNotNone(Folder.find_by_path(f"archives/{message_list.name}"))

    def test_mailman_members(self):
        list_owner, _ = User.create_with_group(UserInfo(
            username="list_owner",
            email="list_owner@user.com",
            password="list_owner"
        ), is_admin=True)

        list_member, _ = User.create_with_group(UserInfo(
            username="list_member",
            email="list_member@user.com",
            password="list_member"
        ), is_admin=True)
        db.session.commit()

        self.login(list_owner.email, "list_owner", list_owner.name)
        list_name = "testlist1"
        _, message_list = self.create_list(list_name, ArchiveType.PUBLIC)

        mlist = self.mailman_client.get_list(message_list.email_address)

        self.assertEqual(len(mlist.members), 1, "New list should have only the owner as the member")
        self.assertEqual(len(mlist.owners), 1, "List owner should be counted as owner of the mailing list")

        # Add user to the list
        self.json_post("/messagelist/addmember", {
            "member_candidates": [list_member.name],
            "msg_list": list_name,
            "send_right": True,
            "delivery_right": True
        })
        self.assertEqual({m.address.email for m in mlist.members},
                         {list_owner.email, list_member.email},
                         "List should have its owner and member added")

        usr = self.mailman_client.get_user(list_member.email)
        self.assertEqual({a.email for a in usr.addresses},
                         {list_member.email},
                         "TIM user should have its email registered with mailman")

        old_email = list_member.email
        list_member = User.get_by_id(list_member.id)
        list_member.update_info(UserInfo(email="list_member_other@user.com"))
        db.session.commit()

        self.assertEqual({m.address.email for m in mlist.members},
                         {list_owner.email, list_member.email},
                         "List should have user's new email listed as member")

        self.assertEqual({a.email for a in usr.addresses},
                         {list_member.email, old_email},
                         "TIM user should still have their old email in mailman database")

    def test_mail_archiving_processing(self):
        self.login_test1()
        self.make_admin(self.test_user_1)
        _, message_list = self.create_list(f"list_archive_test1", ArchiveType.PUBLIC)

        self.trigger_message_send(message_list, self.test_user_1, "Test message", "Test message")

        archive_folder = f"{MESSAGE_LIST_ARCHIVE_FOLDER_PREFIX}/{message_list.name}"
        folder = Folder.find_by_path(archive_folder)
        self.assertIsNotNone(folder, f"Archive folder {archive_folder} must be created")

        doc = next((d for d in folder.get_all_documents() if d.short_name.startswith("Test-message")), None)
        self.assertIsNotNone(doc, f"Archived document named 'Test-message' must be created")

        doc_count = len(folder.get_all_documents())
        self.trigger_message_send(message_list, self.test_user_1, "[SPAM] Test spam message", "Test message")
        self.assertEqual(len(folder.get_all_documents()), doc_count, "Spam messages must not be archived")

        self.trigger_message_send(message_list, self.test_user_1, "try/to/fool/title", "Test message")

        doc = next((d for d in folder.get_all_documents() if d.short_name.startswith("try-to-fool-title")), None)
        self.assertIsNotNone(doc, f"Archived document named 'try-to-fool-title' must be created")

    def test_mail_archive_access(self):
        self.login_test1()
        self.make_admin(self.test_user_1)

        for atype in (ArchiveType.PUBLIC, ArchiveType.GROUPONLY, ArchiveType.UNLISTED):
            _, message_list = self.create_list(f"list_access_{atype.name.lower()}1", atype)

            self.json_post("/messagelist/addmember", {
                "member_candidates": [self.test_user_2.name],
                "msg_list": message_list.name,
                "send_right": True,
                "delivery_right": True
            })

            self.trigger_message_send(message_list, self.test_user_2, "Test message", "This is a test message")

            archive_folder = f"{MESSAGE_LIST_ARCHIVE_FOLDER_PREFIX}/{message_list.name}"
            folder = Folder.find_by_path(archive_folder)
            self.assertIsNotNone(folder, f"Archive folder {archive_folder} must be created")

            docs = folder.get_all_documents()
            self.assertEqual(len(docs), 1, "Sent message should be archived")

            archive_doc = docs[0]
            self.assertEqual(archive_doc.title, "Test message",
                             "Archived message must have the same title as the message subject")

            self.assertEqual({a.usergroup.name for a in archive_doc.block.accesses.values()
                              if a.access_type == AccessType.owner},
                             {self.test_user_1.name, self.test_user_2.name})

            expected_viewers = {}
            if atype == ArchiveType.PUBLIC:
                expected_viewers = {ANONYMOUS_GROUPNAME}
            elif atype == ArchiveType.UNLISTED:
                expected_viewers = {LOGGED_IN_GROUPNAME}
            elif atype == ArchiveType.GROUPONLY:
                # List owner is not counted as a normal messagelist member
                # List owner gets full access to the archives either way
                expected_viewers = {self.test_user_2.name}

            self.assertEqual({a.usergroup.name for a in archive_doc.block.accesses.values()
                              if a.access_type == AccessType.view},
                             expected_viewers)

            self.assertEqual(len([a for a in archive_doc.block.accesses.values()
                                  if a.access_type != AccessType.owner and a.access_type != AccessType.view]), 0,
                             "Archived message must only have view and owner access types")
