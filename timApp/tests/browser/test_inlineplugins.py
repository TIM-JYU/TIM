from selenium.webdriver import ActionChains, Keys

from timApp.tests.browser.browsertest import BrowserTest


class InlinePluginTest(BrowserTest):
    def test_wrapper(self):
        self.login_browser_quick_test1()
        self.login_test1()
        d = self.create_doc(
            initial_par="""
#- {defaultplugin="textfield"}
{#t1#}
        """
        )
        self.goto_document(d)
        self.find_element_avoid_staleness(
            "tim-plugin-loader span[plugin-type='/textfield']"
        )

    def test_multiple_inputs(self):
        # inline text/numericfield should focus to next field if saving via enter
        self.login_browser_quick_test1()
        self.login_test1()
        d = self.create_doc(
            initial_par="""
# - {defaultplugin="textfield"}
{#t1#}{#t2#}{#t3:numericfield#}{#t4#}{#t5#}
                """
        )
        self.goto_document(d)
        inputfield = self.find_element_avoid_staleness(
            f"tim-plugin-loader[task-id='{d.id}.t1'] input"
        )
        inputfield.click()
        for i in range(1, 6):
            ActionChains(self.drv).send_keys(i).perform()
            ActionChains(self.drv).send_keys(Keys.ENTER).perform()
        self.wait_until_hidden(
            f"tim-plugin-loader[task-id='{d.id}.t5'] input.warnFrame"
        )
        self.refresh()
        for i in range(1, 6):
            inputfield = self.find_element_avoid_staleness(
                f"tim-plugin-loader[task-id='{d.id}.t{i}'] input"
            )
            self.assertEqual(inputfield.get_attribute("value"), str(i))
