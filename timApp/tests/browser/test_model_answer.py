from timApp.auth.accesstype import AccessType
from timApp.item.block import Block
from timApp.tests.browser.browsertest import (
    BrowserTest,
)
from timApp.timdb.sqa import db


class ModelAnswerTest(BrowserTest):
    def test_generic_model_answer(self):
        self.login_test1()
        d = self.create_doc(
            initial_par="""
```` {#lock plugin="textfield"}
modelAnswer:
 linkTextCount: 2
 answer: Hello
 linkText: Näytä mallivastaus
button: Tallenna
form: false
````
"""
        )
        self.test_user_2.grant_access(d, AccessType.view)
        db.session.commit()
        db.session.refresh(Block.query.get(d.block.id))
        self.login_test2()
        self.login_browser_quick_test2()
        self.get(f"/getModelAnswer/{d.id}.lock", expect_status=403)
        self.goto_document(d)
        input = self.find_element_avoid_staleness("#lock input")
        input.send_keys("Answer 1")
        save = self.find_element("#lock button")
        save.click()
        self.wait_until_present_and_vis("#lock answerbrowser")
        self.should_not_exist("#lock .modelAnswer a")
        self.get(f"/getModelAnswer/{d.id}.lock", expect_status=200)
        input.send_keys("Answer 2")
        save.click()
        model_answer_link = self.find_element_avoid_staleness("#lock .modelAnswer a")
        model_answer_link.click()
        self.find_element_avoid_staleness("tim-dialog-frame button").click()
        model_answer = self.find_element_avoid_staleness("#lock .modelAnswerContent")
        self.assertEqual("Hello", model_answer.text)

    def test_model_answer_formatting(self):
        self.login_test1()
        self.login_browser_quick_test1()
        d = self.create_doc(
            initial_par="""
```` {#code plugin="textfield"}
modelAnswer:
 answer: |!!
```java
System.out.println("Hello World!");			
```
!! 
form: false
````
```` {#raw plugin="textfield"}
modelAnswer:
 answer: |!!
{%raw%}
{%for i in range(0,100)%} a {%endfor%} 
{%endraw%}
!! 
form: false
````
        """
        )
        self.goto_document(d)
        self.find_element_avoid_staleness("#code input").click()
        self.find_element_avoid_staleness("#code .modelAnswer a").click()
        self.find_element_avoid_staleness("#code .modelAnswerContent .sourceCode")
        self.find_element_avoid_staleness("#raw input").click()
        self.find_element_avoid_staleness("#raw .modelAnswer a").click()
        model_answer = self.find_element_avoid_staleness("#raw .modelAnswerContent")
        self.assertEqual("{%for i in range(0,100)%} a {%endfor%}", model_answer.text)
        pass
