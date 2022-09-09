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
            "tim-plugin-loader span[data-plugin='/textfield']"
        )
