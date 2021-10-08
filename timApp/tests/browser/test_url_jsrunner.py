from selenium.webdriver import Keys

from timApp.tests.browser.browsertest import BrowserTest


class TestUrlJsrunner(BrowserTest):
    def test_url_jsrunner(self):
        self.login_browser_quick_test1()
        self.login_test1()
        d = self.create_doc(
            initial_par=[
                """
#- {defaultplugin="numericfield"}
{#grade#}
""",
                """
``` {#jsarvos plugin="jsrunner"}
groups:
 - "*"
fields:
 - grade
program: |!! 
 tools.print("Read: " + tools.getInt("grade"));
!!
```
""",
            ]
        )

        self.goto_document(d, view="teacher")

        # Try adding answer manually
        grade_input = self.find_element_avoid_staleness(
            "numericfield-runner input[type='tel']"
        )
        grade_input.send_keys("1")
        grade_input.send_keys(Keys.RETURN)

        # JSRunner should not run automatically with default options
        self.refresh()
        self.should_not_exist("js-runner pre")

        # Run JSRunner via URL argument
        self.goto_document(
            d, view="teacher", query={"run_jsrunners": f"{d.id}.jsarvos"}
        )

        jsrunner_output = self.find_element_avoid_staleness("js-runner pre")
        self.assertEqual(jsrunner_output.text, "Read: 1")
