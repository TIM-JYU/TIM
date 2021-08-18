from typing import Dict, Any, Tuple

from timApp.auth.accesshelper import get_doc_or_abort
from timApp.messaging.messagelist.listinfo import ArchiveType
from timApp.messaging.messagelist.messagelist_models import MessageListModel
from timApp.tests.server.timroutetest import TimMessageListTest
from timApp.timdb.sqa import db
from timApp.user.user import User, UserInfo


class MessageListTest(TimMessageListTest):
    """Server test for message lists."""

    def create_list(self, name: str, archive: ArchiveType) -> Tuple[Dict[str, Any], MessageListModel]:
        manage_doc = self.json_post("/messagelist/createlist", {
            "options": {
                "name": name,
                "archive": archive.value,
                "domain": "example.com"
            }
        })
        message_list: MessageListModel = MessageListModel.query.filter_by(name=name).one()
        return manage_doc, message_list

    def test_creation_valid_name(self):
        """Test creating a message list with a valid user and valid list name."""
        # Make testuser1 admin.
        self.login_test1()
        testuser1 = self.current_user
        self.make_admin(testuser1)
        # Create the list. This name corresponds with name requirements. Its long enough, contains only allowed
        # characters and has at least one digit.
        list_name = "valid_list5"
        # The archive type is a mandatory value in list creation, but its exact value doesn't have an impact on this
        # test.
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
        self.assertSetEqual({m.address.email for m in mlist.members},
                            {list_owner.email, list_member.email},
                            "List should have its owner and member added")

        usr = self.mailman_client.get_user(list_member.email)
        self.assertSetEqual({a.email for a in usr.addresses},
                            {list_member.email},
                            "TIM user should have its email registered with mailman")

        old_email = list_member.email
        list_member = User.get_by_id(list_member.id)
        list_member.update_info(UserInfo(email="list_member_other@user.com"))
        db.session.commit()

        self.assertSetEqual({m.address.email for m in mlist.members},
                            {list_owner.email, list_member.email},
                            "List should have user's new email listed as member")

        self.assertSetEqual({a.email for a in usr.addresses},
                            {list_member.email, old_email},
                            "TIM user should still have their old email in mailman database")
