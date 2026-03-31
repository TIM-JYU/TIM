from selenium.webdriver.common.by import By
from selenium.webdriver.remote.webelement import WebElement
from selenium.webdriver.support.ui import WebDriverWait

# noinspection PyPep8Naming
from selenium.webdriver.support import expected_conditions as EC

from timApp.tests.browser.browsertest import BrowserTest


def click_radio(wait: WebDriverWait, locator):
    el = wait.until(EC.element_to_be_clickable(locator))
    el.click()
    wait.until(EC.element_to_be_selected(el))
    return el


def submit_and_wait_saved(
    wait: WebDriverWait, button_locator, saved_class, saved_text="Saved"
):
    btn = wait.until(EC.element_to_be_clickable(button_locator))
    btn.click()
    wait.until(
        EC.text_to_be_present_in_element((By.CSS_SELECTOR, saved_class), saved_text)
    )


def wait_for_result(wait: WebDriverWait, selector: str = "pre") -> WebElement:
    return wait.until(EC.visibility_of_element_located((By.CSS_SELECTOR, selector)))


class PostProgramFieldsTest(BrowserTest):
    def test_postprogram_fields(self):
        self.login_test1()

        d = self.create_doc(initial_par="""
``` {#qst1 question="false" plugin="qst"}
answerFieldType: radio
expl: {}
headers: []
questionText: Question 1
questionTitle: Question 1
questionType: radio-vertical
rows:
- 'Yes'
- 'No'
```

``` {#qst2 question="false" plugin="qst"}
answerFieldType: radio
expl: {}
headers: []
questionText: Question 2
questionTitle: Question 2
questionType: radio-vertical
rows:
- 'Yes'
- 'No'
postprogram_fields:
  - qst1=qst1
postprogram: |!!
  println("<pre>");

  for (let k in data.fields.names) {
    println(k + " => " + data.fields.names[k]);
  }

  println("");

  for (let k in data.fields.names) {
    println(k + " => " + data.fields.values[k]);
  }

  println("</pre>");
  return data;
!!
```
""")

        self.login_browser_quick_test1()
        self.goto_document(d)
        wait = WebDriverWait(self.drv, 10)

        # --- QST1 ---
        click_radio(wait, (By.XPATH, "//td/div/label/input"))
        submit_and_wait_saved(
            wait, (By.XPATH, "//tim-qst/div/div/div/button"), ".qstResult"
        )

        # --- QST2 ---
        click_radio(wait, (By.CSS_SELECTOR, ".qst-tr:nth-child(2) input[type='radio']"))
        submit_and_wait_saved(
            wait,
            (By.XPATH, "//div[3]/div/tim-plugin-loader/div/tim-qst/div/div/div/button"),
            ".qstResult",
        )

        # --- odota tulos ---
        el = wait_for_result(wait, "tim-alert .content pre")

        self.assertEqual(
            el.text,
            f"""
qst1 => {d.id}.qst1

qst1 => 1
""".strip(),
        )
