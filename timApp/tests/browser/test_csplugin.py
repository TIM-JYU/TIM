import re

from time import sleep

from selenium.webdriver.common.by import By

from timApp.tests.browser.browsertest import BrowserTest, PREV_ANSWER

class CsPluginTest(BrowserTest):
    def test_csplugin_translation(self):
        self.login_browser_quick_test1()
        self.login_test1()
        d = self.create_doc(initial_par="""
#- {plugin=csPlugin #py}
type: python
pointsRule:
  code: 1
  expectCode: .*Hei maailma.*
        """)
        dt = self.create_translation(d)
        dt.document.set_settings({
            'global_plugin_attrs': {'all': {'lang': 'en'}},
            # Hide the out-of-date decoration so we don't have to update the screenshot because of it.
            'css': '.troutofdate::before { display: none; }',
        })
        tr_par = dt.document.get_paragraphs()[1]
        tr_par.set_markdown("""
type: python
pointsRule:
  code: 1
  expectCode: .*Hello world.*
        """)
        tr_par.save()
        self.goto_document(d)
        self.wait_until_present_and_vis('#py textarea')
        textarea = self.find_element_and_move_to('#py textarea')
        textarea.send_keys('print("Hei maailma!")')
        par = self.find_element_avoid_staleness('#py > tim-plugin-loader > div')
        runbutton = par.find_element(by=By.CSS_SELECTOR, value='button')
        runbutton.click()
        self.wait_until_present_and_vis('.console')
        self.wait_until_present_and_vis('answerbrowser')
        ptxt = self.find_element_by_text('Points:', 'span')
        self.assertEqual('Points: 1', ptxt.text)

        self.goto_document(dt)

        self.wait_until_present_and_vis('#py textarea')
        textarea = self.find_element_and_move_to('#py textarea')
        textarea.clear()
        textarea.send_keys('print("Hello world!")')
        self.get_uninteractable_element().click()
        par = self.find_element_avoid_staleness('#py > tim-plugin-loader > div')
        self.assert_same_screenshot(par, ['csplugin/python_before_answer'])
        runbutton = par.find_element(by=By.CSS_SELECTOR, value='button')
        runbutton.click()
        self.wait_until_present_and_vis('.console')
        self.wait_until_present_and_vis('answerbrowser')
        ptxt = self.find_element_by_text('Points:', 'span')
        self.assertEqual('Points: 1', ptxt.text)
        self.get_uninteractable_element().click()
        self.assert_same_screenshot(par, 'csplugin/python_after_answer', attempts=2)

        # post a second answer because otherwise clicking previous answer does not do anything
        textarea.send_keys(' ')
        runbutton.click()
        self.wait_until_hidden('#py tim-loading')

        self.wait_and_click(PREV_ANSWER)
        self.wait_until_hidden('.console')
        # Wait until answer is replaced in HTML
        # self.wait.until(ec.staleness_of(par.find_element(by=By.CSS_SELECTOR, value='*')))
        par = self.find_element('#py > tim-plugin-loader > div')

        # Wait until the height workaround completes (see answerbrowser3.ts)
        # self.wait.until(expected_conditions.presence_of_element_located((By.XPATH, "//*[@id='py'][@style='opacity: 1;']")))

        # TODO: Why is this slightly different from python_before_answer ?
        self.assert_same_screenshot(par, 'csplugin/python_after_answer_switch')
        self.verify_answer_content(
            f'{d.id}.py', 'usercode', 'print("Hello world!") ', self.test_user_1, expected_count=3,
        )
        # The answers should always be saved under the original document, so the translated document should
        # not have answers.
        self.verify_answer_content(
            f'{dt.id}.py', 'usercode', '', self.test_user_1, expected_count=0,
        )

    def make_text_and_answer(self, d):
        self.goto_document(d)
        self.wait_until_present_and_vis('#text textarea')
        textarea = self.find_element_and_move_to('#text textarea')
        textarea.send_keys('print("Hello world!")')
        self.get_uninteractable_element().click()
        par = self.find_element_avoid_staleness('#text > tim-plugin-loader > div')
        runbutton = par.find_element(by=By.CSS_SELECTOR, value='button')
        runbutton.click()
        self.wait_until_present_and_vis('answerbrowser')
        self.wait_until_hidden('tim-loading')
        return textarea, runbutton

    def test_csplugin_saveindicators(self):
        """
        Check that savebutton is enabled/disabled by whatever is the current desired logic. For now:
        disableUnchanged false or missing: savebutton always enabled
        disableUnchanged true: savebutton disabled if saved and input doesn't change
        Also check yellow margin is (un)hidden and savedText (dis)appears
        """

        self.login_browser_quick_test1()
        self.login_test1()
        d = self.create_doc(initial_par="""
#- {plugin=csPlugin #text}
type: text
        """)
        textarea, runbutton = self.make_text_and_answer(d)
        self.assertTrue(runbutton.is_enabled())
        savedtext = self.find_element('.savedText')
        self.assertTrue( savedtext.is_displayed())
        self.should_not_exist('.csRunNotSaved')
        d = self.create_doc(initial_par="""
#- {plugin=csPlugin #text}
type: text
disableUnchanged: true
                """)
        textarea, runbutton = self.make_text_and_answer(d)
        self.assertFalse(runbutton.is_enabled())
        savedtext = self.find_element('.savedText')
        self.assertTrue(savedtext.is_displayed())
        self.should_not_exist('.csRunNotSaved')
        textarea.send_keys("more input, let me save")
        self.assertTrue(runbutton.is_enabled())
        self.should_not_exist('.savedText')
        margin = self.find_element('.csRunNotSaved')
        self.assertTrue(margin.is_displayed())

    def test_csplugin_require_type(self):
        self.login_test1()
        d = self.create_doc(initial_par="""
#- {plugin=csPlugin}
stem: ""
        """)
        self.assert_content(self.get(d.url, as_tree=True), ['Attribute "type" is required.'])


    def test_csplugin_answernr1(self):
        self.login_browser_quick_test1()
        self.login_test1()
        # Do not change id below because the sequence of question will be with that id:
        # 0: -6 + -3
        # 1: 1 + -5
        # 2: -2 + -1
        # 3: 10 + -3
        d = self.create_doc(initial_par="""
``` {id="suSn2NPH8MC3" #summa2 plugin="csPlugin" rnd="[[-10,10],[-10,-1]]" seed="answernr"}
type: text
buttonNewTask: Uusi 
stem: "Laske: %%rnd[0]%% +  %%rnd[1]%%"
postoutput: web.console
postprogram: |!!
 let a = %%rnd[0]%%;
 let b = %%rnd[1]%%;
 let c = a + b;
 let u = parseInt(data.save_object.usercode);
 let t = "väärin!";
 data.points = 0;
 if (c === u) {
   t = "OK";
   data.points = 1;
 }  
 print(a  + " + " + b + " = " + c + "; " + u + " on " + t);
 return data;
!!
```
""")
        # Pick document
        self.goto_document(d)
        stem = self.find_element(".stem")
        self.assertEqual("Laske: -6 + -3", stem.text)

        # answer to first (new) question
        input = self.find_element(".csEditArea")
        input.send_keys("-9")
        button = self.find_element(".csRunDiv .timButton")
        button.click()
        # sleep(0.3)
        self.wait_until_hidden('tim-loading')
        button_new = self.find_element_by_text("Uusi")
        count = self.find_element(".answer-index-count")
        self.assertEqual("1/1", count.text)
        self.assertEqual("-6 + -3 = -9; -9 on OK", self.find_element(".console").text)

        # make a new anwser, that is not saved (see later)
        input.clear()
        input.send_keys("3")
        button.click()
        self.wait_until_hidden('tim-loading')
        self.assertEqual("1/1", count.text)
        self.assertEqual("-6 + -3 = -9; 3 on väärin!", self.find_element(".console").text)

        # Answer to new task 2
        button_new.click()
        sleep(0.3)
        self.assertEqual("2/1", count.text)
        stem = self.find_element(".stem")
        self.assertEqual("Laske: 1 + -5", stem.text)
        input = self.find_element(".csEditArea")
        input.clear()
        input.send_keys("-4")
        self.find_element(".csRunDiv .timButton").click()
        self.wait_until_hidden('tim-loading')
        self.assertEqual("2/2", count.text)
        self.assertEqual("Laske: 1 + -5", stem.text)
        self.assertEqual("1 + -5 = -4; -4 on OK", self.find_element(".console").text)

        # Make new answer to same task => no changes
        self.find_element(".csRunDiv .timButton").click()
        self.wait_until_hidden('tim-loading')
        self.assertEqual("2/2", count.text)
        self.assertEqual("1 + -5 = -4; -4 on OK", self.find_element(".console").text)

        # got to task 1/2 there should be the first correct answer -9
        self.find_element(".nextAnswer").click()
        sleep(0.3)
        input = self.find_element(".csEditArea")
        self.assertEqual("-9", input.get_attribute('value'))

        # answer again to task 1/2
        self.assertEqual("1/2", count.text)
        self.assertEqual("Laske: -6 + -3", self.find_element(".stem").text)
        input = self.find_element(".csEditArea")
        input.clear()
        input.send_keys("4")
        self.find_element(".csRunDiv .timButton").click()
        self.wait_until_hidden('tim-loading')
        self.assertEqual("1/2", count.text)
        self.assertEqual("Laske: -6 + -3", self.find_element(".stem").text)
        self.assertEqual("-6 + -3 = -9; 4 on väärin!", self.find_element(".console").text)

        # Answer to new task 3
        button_new.click()
        sleep(0.3)
        self.assertEqual("3/2", count.text)
        self.assertEqual("Laske: -2 + -1", self.find_element(".stem").text)
        input = self.find_element(".csEditArea")
        input.clear()
        input.send_keys("-3")
        self.find_element(".csRunDiv .timButton").click()
        self.wait_until_hidden('tim-loading')
        self.assertEqual("3/3", count.text)
        self.assertEqual("Laske: -2 + -1", self.find_element(".stem").text)
        self.assertEqual("-2 + -1 = -3; -3 on OK", self.find_element(".console").text)

        # Let's refresh, should be 4/3 and Uusi button visible and new task
        self.goto_document(d)
        input = self.find_element(".csEditArea")
        input.click()
        input.send_keys("-9")
        # sleep(0.3)
        button_new = self.find_element_by_text("Uusi")
        self.assertEqual("Uusi", button_new.text)
        self.assertEqual("Laske: 10 + -3", self.find_element(".stem").text)
        self.assertEqual("4/3", self.find_element(".answer-index-count").text)


    def test_csplugin_answernr_stack1(self):

        if (self):  # This is slow! comment this if stack answernr test is needed
            return  # Alswo this is a bit unstable with those sleeps, so not good for CI

        self.login_browser_quick_test1()
        self.login_test1()
        # Do not change id below because the sequence of question will be with that id:
        # 0: 4553:15+2
        # 1: 11370:1+5
        # 2:
        # 3:
        d = self.create_doc(initial_par="""
``` {id="suSn2NPH8MC3" #summa3 plugin="csPlugin" rnd="1,20000" seed="answernr"}
type: stack
buttonNewTask: Uusi
undo:
  button: Yay
-pointsRule: {}
open: true
-stackData:
    seed: %%rnd[0]%%
    readOnly: false
    feedback: true
    score: true
    lang: 'fi'
    question: |!!
---
name: Ynnää
question_html: |
  <p>%%rnd[0]%%: {@a@} + {@b@} [[input:ans1]] &nbsp;&nbsp;[[validation:ans1]]</p>
variables: |-
  a : rand(20);
  b : rand(20);
specific_feedback_html: '[[feedback:prt1]]'
note: <p>Tämä on note</p>
inputs:
  ans1:
    type: algebraic
    model_answer: a+b
    box_size: 15
    syntax_hint: ""
    syntax_attribute: value
response_trees:
  prt1:
    first_node: node_0
    nodes:
      node_0:
        answer: ans1
        model_answer: a+b
        T:
          score_mode: equals
          answer_note: prt1-1-T
          feedback_html: <p>OK!</p>
        F:
          answer_note: prt1-1-F
          feedback_html: <p>Wrong! %%rnd[0]%%</p>
stackversion: 0
...    
!!
```
""")
        def removeTex(s):
            s = s.replace("\n", "").replace("\\({", "").replace("}\\)", "").strip().replace(" ", "")
            s = re.sub("\\{[^}]*}", "", s)
            return s

        def stem_text():
            return removeTex(self.find_element(".stackOutput p").text)

        def feedback_text():
            return self.find_element(".stackprtfeedback-ans1").text

        def input_and_send(s):
            input = self.find_element("#stackapi_ans1")
            input.clear()
            input.send_keys(s)
            button = self.find_element(".csRunDiv .timButton")
            button.click()
            sleep(0.8)
            self.wait_until_hidden('tim-loading')

        # Pick document
        self.goto_document(d)
        self.assertEqual("4553:15+2", stem_text())

        # answer to first (new) question
        input_and_send("17")
        button_new = self.find_element_by_text("Uusi")
        count = self.find_element(".answer-index-count")
        self.assertEqual("1/1", count.text)
        self.assertEqual("OK!", feedback_text())

        # make a new anwser, that is not saved (see later)
        input_and_send("16")
        self.assertEqual("1/1", count.text)
        self.assertEqual("4553:15+2", stem_text())
        self.assertEqual("Wrong! 4553",  feedback_text())

        # Answer to new task 2
        button_new.click()
        sleep(0.3)
        self.assertEqual("2/1", count.text)
        self.assertEqual("11370:1+5", stem_text())
        input_and_send("6")
        self.assertEqual("2/2", count.text)
        self.assertEqual("11370:1+5", stem_text())
        self.assertEqual("OK!", feedback_text())

        # Make new answer to same task => no changes
        input_and_send("5")
        self.assertEqual("2/2", count.text)
        self.assertEqual("11370:1+5", stem_text())
        self.assertEqual("Wrong! 11370", feedback_text())

