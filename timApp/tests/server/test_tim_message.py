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
        self.json_post('/timMessage/url_check', {"urls": "http://localhost/view/users/test-user-1/testdoc"}, expect_status=200)
        self.json_post('/timMessage/url_check', {"urls": "http://localhost/view/users/test-user-1/testdoc"},
                       expect_content= {"shortened_urls" : "users/test-user-1/testdoc"})
        self.json_post('/timMessage/url_check', {"urls": "http://localhost/view/users/test-user-1/testfolder"}, expect_status=200)
        self.json_post('/timMessage/url_check', {"urls": "http://localhost/view/users/test-user-2/testdoc"},
                       expect_status=401)
        self.json_post('/timMessage/url_check', {"urls": "http://localhost/view/users/test-user-2/testfolder"},
                       expect_status=401)
        self.json_post('/timMessage/url_check', {"urls": "http://localhost/teacher/users/test-user-1/testdoc\nhttp://localhost/teacher/users/test-user-1"},
                       expect_status=200)
        self.json_post('/timMessage/url_check', {"urls": "http://localhost/teacher/users/test-user-1/testdoc\nhttp://localhost/teacher/users/test-user-1"},
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