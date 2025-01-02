import time
from typing import Optional

from timApp.document.docinfo import DocInfo
from timApp.tests.browser.browsertest import BrowserTest


class UrlMacroTest(BrowserTest):
    def test_urlmacros(self):
        self.login_browser_quick_test1()
        self.login_test1()
        d = self.create_doc(
            initial_par="""
#- {#p nocache=true}
Macro is: %%m%%.

``` {#t plugin="csPlugin"}
type: python
fullprogram: |!!
print("Hello %%m%%!")
// BYCODEBEGIN
print("Hello World")
// BYCODEEND
!!
```
        """
        )

        def check_first_par(text: str):
            p = self.find_element("#p")
            self.assertEqual(text, p.text)

        # If m is not a URL macro, it should not be possible to edit its value.
        self.check_console(d, "Hello !\nHello World")
        check_first_par("Macro is: .")
        self.check_console(d, "Hello !\nHello World", query={"m": "TIM"})
        check_first_par("Macro is: .")

        d.document.set_settings({"urlmacros": {"m": "someone"}})

        # Now it should be a URL macro.
        self.check_console(d, "Hello someone!\nHello World")
        check_first_par("Macro is: someone.")
        self.check_console(d, "Hello TIM!\nHello World", query={"m": "TIM"})
        check_first_par("Macro is: TIM.")

    def check_console(
        self, d: DocInfo, console_text: str, query: dict[str, str] | None = None
    ):
        self.goto_document(d, query=query)
        runbutton = self.find_element_avoid_staleness(".csRunMenu button")
        runbutton.click()
        self.wait_until_present_and_vis(".console")
        e = self.find_element(".console")
        self.assertEqual(console_text, e.text)

    def test_numeric_urlmacro_default(self):
        self.login_test1()
        d = self.create_doc(
            initial_par="""
%%m1%% %%m2%%
        """,
            settings={"urlmacros": {"m1": 123, "m2": 234.5}},
        )
        self.assert_content(self.get(d.url, as_tree=True), ["", "123 234.5"])
