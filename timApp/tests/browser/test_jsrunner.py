from time import sleep

from selenium.webdriver import ActionChains
from selenium.webdriver.common.by import By
from selenium.webdriver.support import expected_conditions

from timApp.tests.browser.browsertest import BrowserTest
from timApp.tests.server.timroutetest import TimRouteTest
from timApp.timdb.sqa import db
from timApp.user.user import User, UserInfo


class JsRunnerTest(BrowserTest):
    def test_area_visibility_toggle(self):
        User.create_with_group(
            UserInfo(
                username="js_runner_testuser",
                password="testpassword",
                email="js_runner_test@example.com",
                full_name="JS Runner Test User",
            )
        )
        db.session.commit()

        TimRouteTest.login(
            self, "js_runner_test@example.com", "testpassword", "js_runner_testuser"
        )
        d = self.create_doc(
            initial_par="""
``` {#qst1 question="false" plugin="qst"}
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

        self.login_browser_as(
            "js_runner_test@example.com", "testpassword", "JS Runner Test User"
        )
        self.goto_document(d)

        def screenshot(name: str) -> None:
            # Ensure JS is loaded fully
            pars = self.find_element("div#pars")
            self.assert_same_screenshot(
                pars,
                [
                    f"jsrunner/area_visibility_{name}",
                    f"jsrunner/area_visibility_{name}_alt",
                ],
                move_to_element=True,
            )

        def wait_refresh_done():
            self.wait_until_present(".csRunDiv.qst")

        def wait_jsrunner_done():
            self.wait_until_hidden("js-runner tim-loading")

        def click_jsrunner():
            self.drv.find_element(By.CSS_SELECTOR, "js-runner > div > button").click()
            wait_jsrunner_done()

        def wait_jsrunner_output(text: str):
            try:
                self.wait_until_present(f"js-runner .jsrunner-output")
                self.wait.until(
                    expected_conditions.text_to_be_present_in_element(
                        (By.CSS_SELECTOR, "js-runner .jsrunner-output"), text
                    )
                )
            except:
                self.save_screenshot("wait_jsrunner_output_timeout")
                raise

        wait_refresh_done()
        screenshot("initial")

        click_jsrunner()
        wait_jsrunner_output("You need to say Yes to see the hidden content!")
        screenshot("no_answer_click")

        qst_option_1 = self.drv.find_element(
            By.CSS_SELECTOR, ".qst-tr:nth-child(1) .qst-normal"
        )
        qst_save_button = self.drv.find_element(
            By.CSS_SELECTOR, "tim-qst > div > div > div > button"
        )
        body_element = self.drv.find_element(By.CSS_SELECTOR, "body")
        actions = ActionChains(self.drv, duration=1000)
        actions.move_to_element(qst_option_1)
        actions.click(qst_option_1)
        actions.move_to_element(body_element)
        actions.click(qst_save_button)
        actions.perform()
        sleep(1)
        click_jsrunner()
        wait_jsrunner_output("You can now see the hidden content!")
        screenshot("yes_click")

        self.refresh()
        wait_refresh_done()
        screenshot("yes_click_refresh")

        qst_option_2 = self.drv.find_element(
            By.CSS_SELECTOR, ".qst-tr:nth-child(2) .qst-normal"
        )
        qst_save_button = self.drv.find_element(
            By.CSS_SELECTOR, "tim-qst > div > div > div > button"
        )
        actions = ActionChains(self.drv, duration=1000)
        actions.move_to_element(qst_option_2)
        actions.click(qst_option_2)
        actions.move_to_element(qst_save_button)
        actions.click(qst_save_button)
        actions.perform()
        sleep(1)
        click_jsrunner()
        wait_jsrunner_output("You need to say Yes to see the hidden content!")
        screenshot("no_click")

        self.refresh()
        wait_refresh_done()
        screenshot("no_click_refresh")

    def test_update_fields(self):
        self.login_browser_quick_test1()
        self.login_test1()
        d = self.create_doc(
            initial_par="""
#- {defaultplugin="textfield"}
{#t1#}

``` {#t_non_form plugin="textfield"}
form: false
```

``` {#runner plugin="jsrunner"}
showInView: true
groups: []
updateFields:
 - t1
 - t_non_form
fields:
 - t1
 - t_non_form
program: |!!
tools.setString("t1", "Hello (form update)")
tools.setString("t_non_form", "Hello (regular update)")
!!
```
"""
        )
        self.goto_document(d)
        runner = self.find_element_avoid_staleness("js-runner .timButton")
        runner.click()
        self.wait_until_hidden("js-runner tim-loading")
        input = self.find_element(
            "tim-plugin-loader[task-id$='t1'] tim-textfield-runner input"
        )
        self.assertEqual("Hello (form update)", input.get_attribute("value"))
        input = self.find_element(
            "tim-plugin-loader[task-id$='t_non_form'] tim-textfield-runner input"
        )
        self.assertEqual("Hello (regular update)", input.get_attribute("value"))
