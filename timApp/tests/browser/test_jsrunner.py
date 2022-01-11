from selenium.webdriver import ActionChains
from selenium.webdriver.common.by import By
from selenium.webdriver.support import expected_conditions

from timApp.tests.browser.browsertest import BrowserTest


class JsRunnerTest(BrowserTest):
    def test_area_visibility_toggle(self):
        self.login_test1()
        d = self.create_doc(
            initial_par="""
``` {#qst1 dquestion="true" plugin="qst"}
answerFieldType: radio
expl: {}
headers: []
hideBrowser: true
questionText: Display the content
questionTitle: qst1
questionType: radio-vertical
rows:
- 'Yes'
- 'No'
```

``` {#runner plugin="jsrunner"}
fields:
 - qst1
groups:
 - %%username%%
showInView: true
button: "Try display"
program: |!!
  let qstValue = tools.getValue("qst1") || [[undefined]];
  let qstSelection = qstValue[0][0];
  let show = qstSelection == "1";
  if (show) tools.println("You can now see the hidden content!");
  else tools.println("You need to say Yes to see the hidden content!");
  gtools.outdata.areaVisibility = {
      "more-tasks": show,
  };
!!
```

#- {area="more-tasks" .hide}

#-
## Hidden content

Test

#- {area_end="more-tasks"}
"""
        )

        self.login_browser_quick_test1()
        self.goto_document(d)

        def screenshot(name: str) -> None:
            # Ensure JS is loaded fully
            pars = self.find_element("div#pars")
            self.assert_same_screenshot(
                pars, f"jsrunner/area_visibility_{name}", move_to_element=True
            )

        def wait_refresh_done():
            self.wait_until_present(".csRunDiv.qst")

        def wait_jsrunner_done():
            self.wait_until_hidden("js-runner tim-loading")

        def click_jsrunner():
            self.drv.find_element(
                By.CSS_SELECTOR, "div:nth-child(1) > .timButton:nth-child(1)"
            ).click()
            wait_jsrunner_done()

        def wait_jsrunner_output(text: str):
            self.wait_until_present(f"js-runner .jsrunner-output")
            self.wait.until(
                expected_conditions.text_to_be_present_in_element(
                    (By.CSS_SELECTOR, "js-runner .jsrunner-output"), text
                )
            )

        wait_refresh_done()
        screenshot("initial")

        click_jsrunner()
        wait_jsrunner_output("You need to say Yes to see the hidden content!")
        screenshot("no_answer_click")

        element = self.drv.find_element(
            By.CSS_SELECTOR, ".qst-tr:nth-child(1) .qst-normal"
        )
        actions = ActionChains(self.drv)
        actions.move_to_element(element).perform()
        self.drv.find_element(
            By.CSS_SELECTOR, ".qst-tr:nth-child(1) .qst-normal"
        ).click()
        element = self.drv.find_element(By.CSS_SELECTOR, "body")
        actions = ActionChains(self.drv)
        actions.move_to_element(element).perform()
        self.drv.find_element(By.CSS_SELECTOR, ".csRunDiv > .timButton").click()
        click_jsrunner()
        wait_jsrunner_output("You can now see the hidden content!")
        screenshot("yes_click")

        self.refresh()
        wait_refresh_done()
        screenshot("yes_click_refresh")

        self.drv.find_element(
            By.CSS_SELECTOR, ".qst-tr:nth-child(2) .qst-normal"
        ).click()
        self.drv.find_element(By.CSS_SELECTOR, ".csRunDiv > .timButton").click()
        click_jsrunner()
        wait_jsrunner_output("You need to say Yes to see the hidden content!")
        screenshot("no_click")

        self.refresh()
        wait_refresh_done()
        screenshot("no_click_refresh")
