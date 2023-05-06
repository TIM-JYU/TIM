from selenium.webdriver.common.by import By
from selenium.webdriver.remote.webelement import WebElement

from timApp.tests.browser.browsertest import BrowserTest


class PostProgramFieldsTest(BrowserTest):
    def test_postprogram_fields(self):
        self.login_test1()

        d = self.create_doc(
            initial_par="""
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
"""
        )

        self.login_browser_quick_test1()
        self.goto_document(d)
        self.drv.find_element(By.XPATH, "//td/div/label/input").click()
        self.drv.find_element(By.XPATH, "//tim-qst/div/div/div/button").click()
        self.drv.find_element(
            By.CSS_SELECTOR, ".qst-tr:nth-child(2) .ng-untouched"
        ).click()
        self.drv.find_element(
            By.XPATH, "//div[3]/div/tim-plugin-loader/div/tim-qst/div/div/div/button"
        ).click()

        self.wait_until_present("pre")
        el: WebElement = self.drv.find_element(By.CSS_SELECTOR, "pre")
        self.assertEqual(
            el.text,
            f"""
qst1 => {d.id}.qst1

qst1 => 1
""".strip(),
        )
