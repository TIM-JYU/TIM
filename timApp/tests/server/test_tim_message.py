from timApp.auth.accesstype import AccessType
from timApp.tests.server.timroutetest import TimRouteTest


class UrlTest(TimRouteTest):
    def test_url_check(self):
        self.login_test2()
        self.create_doc(self.get_personal_item_path("testdoc"))
        self.create_folder(self.get_personal_item_path("testfolder"))

        self.login_test1()
        self.json_post('/timMessage/url_check', {"urls": "http://www.google.com"}, expect_status=404)
        self.create_doc(self.get_personal_item_path("testdoc"))
        self.create_folder(self.get_personal_item_path("testfolder"))
        self.json_post('/timMessage/url_check', {"urls": "http://localhost/view/users/test-user-1/testdoc"},
                       expect_status=200)
        self.json_post('/timMessage/url_check', {"urls": "http://localhost/view/users/test-user-1/testdoc"},
                       expect_content={"shortened_urls": "users/test-user-1/testdoc"})
        self.json_post('/timMessage/url_check', {"urls": "http://localhost/view/users/test-user-1/testfolder"},
                       expect_status=200)
        self.json_post('/timMessage/url_check', {"urls": "http://localhost/view/users/test-user-2/testdoc"},
                       expect_status=401)
        self.json_post('/timMessage/url_check', {"urls": "http://localhost/view/users/test-user-2/testfolder"},
                       expect_status=401)
        self.json_post('/timMessage/url_check', {
            "urls": "http://localhost/teacher/users/test-user-1/testdoc\nhttp://localhost/teacher/users/test-user-1"},
                       expect_status=200)
        self.json_post('/timMessage/url_check', {
            "urls": "http://localhost/teacher/users/test-user-1/testdoc\nhttp://localhost/teacher/users/test-user-1"},
                       expect_content={"shortened_urls": "users/test-user-1/testdoc\nusers/test-user-1"})
        self.json_post('/timMessage/url_check', {"urls": "  http://localhost/view/users/test-user-1/testdoc"},
                       expect_status=200)
        self.json_post('/timMessage/url_check', {"urls": "http://localhost/view/users/test-user-1/testdoc  "},
                       expect_status=200)
        self.json_post('/timMessage/url_check', {"urls": "http://localhost/view/users/test-user-1/testdoc#jjndsg"},
                       expect_status=200)
        self.json_post('/timMessage/url_check', {"urls": "http://localhost/view/users/test-user-1/testfolder/"},
                       expect_status=200)
        self.json_post('/timMessage/url_check', {"urls": "http://localhost/view/users/test-user-1/testfolder/testdoc"},
                       expect_status=404)


class SendMessageTest(TimRouteTest):
    def test_send_message(self):
        self.login_test1()

        d = self.create_doc(self.get_personal_item_path('testdoc'))
        # f = self.create_folder(self.get_personal_item_path('testfolder'))
        self.test_user_2.grant_access(d, AccessType.edit)

        self.login_test2()
        self.json_post('/timMessage/send',
                       {'options': {
                           'messageChannel': False,
                           'archive': False,
                           'important': False,
                           'isPrivate': False,
                           'pageList': "users/test-user-2/testfolder",
                           'readReceipt': True,
                           'reply': True,
                           'sender': self.test_user_2.name,
                           'senderEmail': self.test_user_2.email},
                           'message': {
                               'messageBody': "test message",
                               'messageSubject': "test subject",
                               'recipients': [self.test_user_1.email]
                           }},
                       expect_status=200)

        self.get('messages/tim-messages', expect_status=200)
