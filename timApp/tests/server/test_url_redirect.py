from urllib.parse import quote_plus

from timApp.tests.server.timroutetest import TimRouteTest


class RedirectTest(TimRouteTest):
    def test_redirect(self):
        self.login_test1()
        personal_folder = self.current_user.get_personal_folder().path
        testing_space = f"{personal_folder}/testing-space"
        self.create_doc(testing_space)
        testing_space_cap = f"{personal_folder}/testing-spAce"
        self.create_doc(testing_space_cap)
        testing_spoce = f"{personal_folder}/testing-spoce"
        self.create_doc(testing_spoce)
        testing_remove = f"{personal_folder}/testingremove"
        self.create_doc(testing_remove)
        for route in ("view", "manage"):
            params = "a=b"
            self.get(
                f"{route}/{personal_folder}/testing space",
                expect_status=302,
                expect_content=f"/{route}/{testing_space}",
            )
            self.get(
                f"{route}/{personal_folder}/testing spAce",
                expect_status=302,
                expect_content=f"/{route}/{testing_space_cap}",
            )
            self.get(
                f"{route}/{personal_folder}/testing späce",
                expect_status=302,
                expect_content=f"/{route}/{testing_space}?{params}",
                query_string={"a": "b"},
            )
            self.get(
                f"{route}/{personal_folder}/testing spöce",
                expect_status=302,
                expect_content=f"/{route}/{testing_spoce}?{params}",
                query_string={"a": "b"},
            )
            self.get(
                f"{route}/{personal_folder}/testing spåce",
                expect_status=302,
                expect_content=f"/{route}/{testing_space}?{params}",
                query_string={"a": "b"},
            )
            for c in "<>|½!\"#¤%&()=?`´¨~^',;:@£$€{[]}\\":
                self.get(
                    quote_plus(f"{route}/{personal_folder}/testing{c}remove"),
                    expect_status=302,
                    expect_content=f"/{route}/{testing_remove}?{params}",
                    query_string={"a": "b"},
                )

            self.get(f"{route}/{personal_folder}/testing space", follow_redirects=True)
            self.get(f"{route}/{personal_folder}/testing spAce", follow_redirects=True)
            self.get(f"{route}/{personal_folder}/testing spÄce", follow_redirects=True)

    def test_slash_redirect(self):
        self.get("/", expect_status=200)
        self.get("/test", expect_status=404)
        self.get("/test/", expect_status=302, expect_content="/test")
        self.get("/test//", expect_status=302, expect_content="/test")
        self.get("/test///", expect_status=302, expect_content="/test")
        self.get("/test/////////", expect_status=302, expect_content="/test")
        self.get("/test/?a=b", expect_status=302, expect_content="/test?a=b")
        self.get("/test/x/", expect_status=302, expect_content="/test/x")
        self.get("/test/x/?a=b", expect_status=302, expect_content="/test/x?a=b")
        self.get("/test//x/", expect_status=302, expect_content="/test/x")
        self.get("/test//x///?a=b", expect_status=302, expect_content="/test/x?a=b")
        self.get("/view", expect_status=200)
        self.get("/manage", expect_status=302, expect_content="/view")
        self.get("/ /example.com/", expect_status=302, expect_content="/example.com")
        self.get("/%20/example.com/", expect_status=302, expect_content="/example.com")
        self.get("/%09/example.com/", expect_status=302, expect_content="/example.com")
