from timApp.auth.accesshelper import get_doc_or_abort
from timApp.messaging.messagelist.listoptions import ArchiveType
from timApp.messaging.messagelist.messagelist_models import MessageListModel
from timApp.tests.server.timroutetest import TimMessageListTest


class MessageListTest(TimMessageListTest):
    """Server test for message lists."""

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
        response_item = self.json_post("/messagelist/createlist",
                                       {"options": {"name": list_name,
                                                    "archive": archive.value,
                                                    "domain": "example.com"}
                                        }, expect_status=200)
        # Expected response is in JSON, and it's a DocEntry of the created admin doc. Get the created admin doc of
        # the message list.
        admin_doc = get_doc_or_abort(response_item["id"])
        # Verify that the list exists in the db.
        message_list: MessageListModel = MessageListModel.query.filter_by(name=list_name).one()
        # Verify returned admin doc is the same message list's admin doc in db.
        self.assertEqual(message_list.manage_doc_id, admin_doc.id)
        # Verify name and archive type are as intented in the db.
        self.assertEqual(message_list.name, list_name)
        self.assertEqual(message_list.archive, archive)
