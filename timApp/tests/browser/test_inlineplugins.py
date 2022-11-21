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

    def test_no_lazy_inline(self):
        self.login_test1()
        self.login_browser_quick_test1()
        d = self.create_doc(
            initial_par="""
``` {#normal plugin="csPlugin"}
type: text/tiny
```

#- {defaultplugin="csPlugin"}
{#inline1 type: text/tiny #} {#inlinelazy1 type: text/tiny, lazy: true #} {#inlinelazy2 type: text/tiny, lazy: true #}
        """
        )
        with self.temp_config({"PLUGIN_COUNT_LAZY_LIMIT": 1}):
            self.goto_document(d)
            self.wait_until_present_and_vis(
                f'tim-plugin-loader[task-id="{d.id}.normal"]'
            )

            def check_lazy_plugin(tid: str):
                self.should_not_exist(
                    f'tim-plugin-loader[task-id="{d.id}.{tid}"] cs-text-runner'
                )
                self.find_element_and_move_to(
                    f'tim-plugin-loader[task-id="{d.id}.{tid}"]'
                )
                self.wait_until_present_and_vis(
                    f'tim-plugin-loader[task-id="{d.id}.{tid}"] cs-text-runner'
                )

            # Regular plugin is lazy because of the count limit
            check_lazy_plugin("normal")
            # Inlineplugin is not lazy by default despite the count limit
            self.find_element(
                f'tim-plugin-loader[task-id="{d.id}.inline1"] cs-text-runner'
            )
            # Explicit lazy: true overrides loading inlineplugins as non-lazy
            check_lazy_plugin("inlinelazy1")
            check_lazy_plugin("inlinelazy2")
