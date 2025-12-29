from selenium.webdriver import ActionChains, Keys

from timApp.tests.browser.browsertest import BrowserTest
from timApp.tests.db.timdbtest import running_in_ci


class TableFormTest(BrowserTest):
    def test_input_saving_and_canceling(self):
        if running_in_ci():
            self.skipTest("Fails in CI but not locally (likely flaky test)")
        self.login_browser_quick_test1()
        self.login_test1()
        d = self.create_doc(
            initial_par="""
``` {#text1 plugin="textfield"}
```
``` {#tableForm plugin="tableForm"}
showInView: true
groups: []
fields:
 - text1
autosave: true
```
"""
        )
        self.goto_document(d)
        td = self.find_element_avoid_staleness("#tableForm tbody td:nth-of-type(6)")
        td.click()
        td.click()
        ActionChains(self.drv).send_keys("1").perform()
        ActionChains(self.drv).send_keys(Keys.ENTER).perform()
        ActionChains(self.drv).send_keys("2").perform()
        self.find_element("#tableForm .buttonAcceptEdit").click()
        td.click()
        ActionChains(self.drv).send_keys("3").perform()
        ActionChains(self.drv).send_keys(Keys.ESCAPE).perform()
        td.click()
        self.find_element_avoid_staleness("#tableForm .buttonCloseSmallEditor").click()
        td.click()
        ActionChains(self.drv).send_keys("4").perform()
        self.get_uninteractable_element().click()
        self.wait_until_hidden("tableform tim-loading")
        answers = self.get_task_answers(f"{d.id}.text1", self.test_user_1)
        self.assertEqual(len(answers), 3)
        self.assertEqual(
            answers[0]["content"], '{"c": "4"}'
        )  # editor closed by clicking outside the table
        self.assertEqual(
            answers[1]["content"], '{"c": "2"}'
        )  # editor accepted with ok icon
        self.assertEqual(
            answers[2]["content"], '{"c": "1"}'
        )  # editor accepted with enter button
