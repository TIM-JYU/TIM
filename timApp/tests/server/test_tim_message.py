from sqlalchemy import select

from timApp.auth.accesstype import AccessType
from timApp.document.docentry import DocEntry
from timApp.folder.folder import Folder
from timApp.messaging.timMessage.internalmessage_models import (
    InternalMessageDisplay,
    InternalMessage,
)
from timApp.tests.server.timroutetest import TimRouteTest
from timApp.timdb.sqa import db


class UrlTest(TimRouteTest):
    def test_url_check(self):
        self.login_test2()
        self.create_doc(self.get_personal_item_path("testdoc"))
        self.create_folder(self.get_personal_item_path("testfolder"))

        self.login_test1()
        self.json_post(
            "/timMessage/url_check",
            {"urls": "http://www.google.com"},
            expect_status=404,
        )
        self.create_doc(self.get_personal_item_path("testdoc"))
        self.create_folder(self.get_personal_item_path("testfolder"))
        self.json_post(
            "/timMessage/url_check",
            {"urls": "http://localhost/view/users/test-user-1/testdoc"},
            expect_status=200,
        )
        self.json_post(
            "/timMessage/url_check",
            {"urls": "http://localhost/view/users/test-user-1/testdoc"},
            expect_content={"shortened_urls": "users/test-user-1/testdoc"},
        )
        self.json_post(
            "/timMessage/url_check",
            {"urls": "http://localhost/view/users/test-user-1/testfolder"},
            expect_status=200,
        )
        self.json_post(
            "/timMessage/url_check",
            {"urls": "http://localhost/view/users/test-user-2/testdoc"},
            expect_status=401,
        )
        self.json_post(
            "/timMessage/url_check",
            {"urls": "http://localhost/view/users/test-user-2/testfolder"},
            expect_status=401,
        )
        self.json_post(
            "/timMessage/url_check",
            {
                "urls": "http://localhost/teacher/users/test-user-1/testdoc\nhttp://localhost/teacher/users/test-user-1"
            },
            expect_status=200,
        )
        self.json_post(
            "/timMessage/url_check",
            {
                "urls": "http://localhost/teacher/users/test-user-1/testdoc\nhttp://localhost/teacher/users/test-user-1"
            },
            expect_content={
                "shortened_urls": "users/test-user-1/testdoc\nusers/test-user-1"
            },
        )
        self.json_post(
            "/timMessage/url_check",
            {"urls": "  http://localhost/view/users/test-user-1/testdoc"},
            expect_status=200,
        )
        self.json_post(
            "/timMessage/url_check",
            {"urls": "http://localhost/view/users/test-user-1/testdoc  "},
            expect_status=200,
        )
        self.json_post(
            "/timMessage/url_check",
            {"urls": "http://localhost/view/users/test-user-1/testdoc#jjndsg"},
            expect_status=200,
        )
        self.json_post(
            "/timMessage/url_check",
            {"urls": "http://localhost/view/users/test-user-1/testfolder/"},
            expect_status=200,
        )
        self.json_post(
            "/timMessage/url_check",
            {"urls": "http://localhost/view/users/test-user-1/testfolder/testdoc"},
            expect_status=404,
        )


class SendMessageTest(TimRouteTest):
    def test_send_message(self):
        self.login_test1()

        f = self.create_folder(self.get_personal_item_path("testfolder"))
        self.test_user_2.grant_access(Folder.get_by_id(f["id"]), AccessType.edit)

        self.login_test2()
        self.json_post(
            "/timMessage/send",
            {
                "options": {
                    "messageChannel": False,
                    "archive": False,
                    "important": False,
                    "isPrivate": False,
                    "pageList": "users/test-user-1/testfolder",
                    "readReceipt": True,
                    "reply": True,
                    "sender": self.test_user_2.name,
                    "senderEmail": self.test_user_2.email,
                },
                "message": {
                    "messageBody": "test message",
                    "messageSubject": "test subject",
                    "recipients": [self.test_user_1.email],
                },
            },
            expect_status=200,
        )

        self.login_test1()
        self.get(
            "/view/messages/tim-messages", expect_status=200
        )  # tim-messages folder created successfully

        display = (
            db.session.execute(
                select(InternalMessageDisplay)
                .filter_by(usergroup_id=self.get_test_user_1_group_id())
                .limit(1)
            )
            .scalars()
            .first()
        )
        msg = (
            db.session.execute(
                select(InternalMessage).filter_by(id=display.message_id).limit(1)
            )
            .scalars()
            .first()
        )
        msg_doc = (
            db.session.execute(select(DocEntry).filter_by(id=msg.doc_id).limit(1))
            .scalars()
            .first()
        )
        self.get(
            f"/view/{msg_doc.name}", expect_status=200
        )  # document for message created successfully
