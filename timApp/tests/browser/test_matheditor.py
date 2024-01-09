"""
Runs tests for math editor
"""

__authors__ = ["Daniel Juola"]
__license__ = "MIT"
__date__ = "31.3.2023"

from selenium.common import StaleElementReferenceException
from selenium.webdriver import ActionChains
from selenium.webdriver.common.keys import Keys

from timApp.answer.answers import save_answer
from timApp.plugin.taskid import TaskId
from timApp.tests.browser.browsertest import BrowserTest
from time import sleep


class MathEditorTest(BrowserTest):
    def test_add_formula(self):
        """Tests adding new formulas with math editor"""
        self.login_browser_quick_test1()
        self.login_test1()
        d = self.create_doc(
            initial_par="""
```{#mqtest plugin="csPlugin"}
type: md
# highlight:
placeholder: "Kirjoita kaavaa"
stem: "Kirjoita alle kaavaa"
button: Tallenna
rows: 1
editorModes: "01"
editorMode: 1
autorun: true
autoupdate: 1000
formulaEditor: true
```
"""
        )
        self.goto_document(d)
        open_formula_button = self.find_element(xpath="//button[@title='Ctrl+e']")
        open_formula_button.click()
        math_field_parent = self.find_element(xpath="//span[@class='mq-textarea']")
        math_field = self.find_element(xpath="//textarea", parent=math_field_parent)
        math_field.send_keys("12345")
        latex_field = self.find_element(xpath="//textarea[@name='math-editor-output']")
        latex_field.send_keys(Keys.BACKSPACE)
        save_formula_button = self.find_element(xpath="//button[@title='Ctrl+s']")
        save_formula_button.click()
        ace_input = self.find_element(xpath="//div[@class='ace_line'][2]")
        self.assertEqual("1234", ace_input.text)

    def test_edit_formula(self):
        """Tests editing existing formulas with math editor"""
        self.login_browser_quick_test1()
        self.login_test1()
        d = self.create_doc(
            initial_par="""
```{#mqtest plugin="csPlugin"}
type: md
# highlight:
placeholder: "Kirjoita kaavaa"
stem: "Kirjoita alle kaavaa"
button: Tallenna
rows: 1
editorModes: "01"
editorMode: 1
autorun: true
autoupdate: 1000
formulaEditor: true
```
"""
        )
        self.goto_document(d)
        ace_input = self.find_element(xpath="//textarea[@class='ace_text-input']")
        ace_input.send_keys("$12345$")
        ace_input.send_keys(Keys.ARROW_LEFT)
        open_formula_button = self.find_element(xpath="//button[@title='Ctrl+e']")
        open_formula_button.click()
        latex_field = self.find_element(xpath="//textarea[@name='math-editor-output']")
        self.assertEqual("12345", latex_field.get_attribute("value"))
        latex_field.send_keys(Keys.BACKSPACE)
        save_formula_button = self.find_element(xpath="//button[@title='Ctrl+s']")
        save_formula_button.click()
        ace_input = self.find_element(xpath="//div[@class='ace_line'][3]")
        self.assertEqual("$1234$", ace_input.text)

    def test_par_editor(self):
        """Tests using math editor in paragraph editor"""
        self.login_browser_quick_test1()
        self.login_test1()
        d = self.create_doc()
        self.goto_document(d)
        self.find_element_avoid_staleness(".addBottom", click=True)
        self.wait_for_editor_load()
        self.wait_until_present_and_vis(".ace_content")
        tex_button = self.find_element_by_text("TeX", "a")
        tex_button.click()
        open_formula_button = self.find_element(xpath="//button[@title='Ctrl+e']")
        open_formula_button.click()
        latex_field = self.find_element(xpath="//textarea[@name='math-editor-output']")
        latex_field.send_keys("12345")
        save_formula_button = self.find_element(xpath="//button[@title='Ctrl+s']")
        save_formula_button.click()
        save_par_editor_button = self.find_element(
            xpath="//button[@title='Save (Ctrl-S)']"
        )
        save_par_editor_button.click()
        sleep(1)
        mord = self.find_element(xpath="//span[@class='mord']")
        self.assertEqual("12345", mord.text)

    def test_use_buttons(self):
        """Tests math editor buttons"""
        self.login_browser_quick_test1()
        self.login_test1()
        d = self.create_doc(
            initial_par="""
```{#mqtest plugin="csPlugin"}
type: md
# highlight:
placeholder: "Kirjoita kaavaa"
stem: "Kirjoita alle kaavaa"
button: Tallenna
rows: 1
editorModes: "01"
editorMode: 1
autorun: true
autoupdate: 1000
formulaEditor: true
buttons: |!!
[ "\\\\[ \\\\sin \\\\]", "\\\\sin", "\\\\sin", "q"]
[ "\\\\[ \\\\cos \\\\]", "\\\\cos", "\\\\cos", "t"]
[ "\\\\[ \\\\tan \\\\]", "\\\\tan", "\\\\tan", "s"]
[ "\\\\[ \\\\frac{\\\\square}{\\\\square} \\\\]", "e"]
!!
```
"""
        )
        self.goto_document(d)
        quick_button = self.find_element(xpath="//button[@title='\\sin']")
        tim_button = self.find_element(xpath="//button[@title='\\cos']")
        quick_button.click()
        tim_button.click()
        get_out_of_the_way_please = self.find_element(
            xpath="//div[@class='readline hover click']"
        )
        get_out_of_the_way_please.click()
        expand_symbol_menu_button = self.find_element(
            xpath="//button[@title='Show more symbols']"
        )
        expand_symbol_menu_button.click()
        symbol_button = self.find_element(xpath="//button[@title='\\tan']")
        symbol_button.click()
        expanded_button = self.find_element(xpath="//button[@title='\\frac{}{}']")
        expanded_button.click()
        open_formula_button = self.find_element(xpath="//button[@title='Ctrl+e']")
        open_formula_button.click()
        self.assertFalse(tim_button.is_displayed())
        self.assertFalse(expanded_button.is_displayed())
        quick_button.click()
        symbol_button.click()
        save_formula_button = self.find_element(xpath="//button[@title='Ctrl+s']")
        save_formula_button.click()
        ace_input = self.find_element(xpath="//div[@class='ace_line'][1]")
        self.assertEqual("\\sin\\cos\\tan\\frac{$\\sin\\tan$}{}", ace_input.text)

    def test_symbolbutton_plugin(self):
        """Tests external symbolbutton on different plugins:
        -   Selected text is replaced correctly
        -   Cursor is repositioned correctly
        -   Clicking the external editor does not generate extra autosaves"""
        self.login_browser_quick_test1()
        self.login_test1()
        d = self.create_doc(
            initial_par="""
``` {plugin="symbolbutton"}
buttons: |!!
[ "$", "$⁞$", "$ $", "t" ]
!!
mdButtons:
 - text: \[ \pi \]
   data: \pi
   expl: \pi
   type: q
```
``` {#textfield plugin="textfield"}
autosave: true
```
``` {#cstiny plugin="csPlugin"}
type: text/tiny
autosave: true
```
``` {#csnormal plugin="csPlugin"}
type: text
autosave: true
```
``` {#cshighlight plugin="csPlugin"}
type: text
autosave: true
```
            """
        )
        save_answer(
            [self.test_user_1],
            TaskId.parse(f"{d.id}.textfield"),
            content={"c": "0987654321"},
            points=None,
        )
        save_answer(
            [self.test_user_1],
            TaskId.parse(f"{d.id}.cstiny"),
            content={"usercode": "0987654321"},
            points=None,
        )
        save_answer(
            [self.test_user_1],
            TaskId.parse(f"{d.id}.csnormal"),
            content={"usercode": "0987654321"},
            points=None,
        )
        save_answer(
            [self.test_user_1],
            TaskId.parse(f"{d.id}.cshighlight"),
            content={"usercode": "0987654321"},
            points=None,
        )
        self.goto_document(d)
        self.wait_until_text_present("#cshighlight", "Highlight")
        highlight_parent = self.find_element("#cshighlight")
        self.find_element_by_text("Highlight", parent=highlight_parent).click()
        dollar_button = self.find_element('[title="$ $"]')
        self.wait_until_present_and_vis("#textfield input")
        element = self.find_element("#textfield input")
        element.click()
        ActionChains(self.drv).send_keys(Keys.END).key_down(Keys.SHIFT).send_keys(
            Keys.LEFT
        ).send_keys(Keys.LEFT).key_up(Keys.SHIFT).perform()
        dollar_button.click()
        ActionChains(self.drv).send_keys("A").perform()
        self.wait_until_present_and_vis("#cstiny input")
        element = self.find_element("#cstiny input")
        element.click()
        ActionChains(self.drv).send_keys(Keys.END).send_keys(Keys.LEFT).key_down(
            Keys.SHIFT
        ).send_keys(Keys.LEFT).send_keys(Keys.LEFT).key_up(Keys.SHIFT).perform()
        dollar_button.click()
        ActionChains(self.drv).send_keys("A").perform()
        self.wait_until_present_and_vis("#csnormal textarea")
        element = self.find_element("#csnormal textarea")
        element.click()
        ActionChains(self.drv).send_keys(Keys.END).send_keys(Keys.LEFT).send_keys(
            Keys.LEFT
        ).key_down(Keys.SHIFT).send_keys(Keys.LEFT).send_keys(Keys.LEFT).key_up(
            Keys.SHIFT
        ).perform()
        dollar_button.click()
        ActionChains(self.drv).send_keys("A").perform()
        self.wait_until_present_and_vis("#cshighlight cs-ace-editor")
        element = self.find_element("#cshighlight cs-ace-editor")
        element.click()
        ActionChains(self.drv).send_keys(Keys.END).send_keys(Keys.LEFT).send_keys(
            Keys.LEFT
        ).send_keys(Keys.LEFT).key_down(Keys.SHIFT).send_keys(Keys.LEFT).send_keys(
            Keys.LEFT
        ).key_up(
            Keys.SHIFT
        ).perform()
        dollar_button.click()
        ActionChains(self.drv).send_keys("A").perform()
        self.get_uninteractable_element().click()
        self.goto_document(d)
        check = self.find_element_avoid_staleness("#textfield input")
        self.assertEqual("09876543$A$", check.get_attribute("value"))
        check = self.find_element_avoid_staleness("#cstiny input")
        self.assertEqual("0987654$A$1", check.get_attribute("value"))
        check = self.find_element_avoid_staleness("#csnormal .ace_line")
        try:
            self.assertEqual("098765$A$21", check.text)
        except StaleElementReferenceException:
            check = self.find_element_avoid_staleness("#csnormal .ace_line")
            self.assertEqual("098765$A$21", check.text)
        check = self.find_element_avoid_staleness("#cshighlight .ace_line")
        try:
            self.assertEqual("09876$A$321", check.text)
        except StaleElementReferenceException:
            check = self.find_element_avoid_staleness("#cshighlight .ace_line")
            self.assertEqual("09876$A$321", check.text)
        for i in ["textfield", "cstiny", "csnormal", "cshighlight"]:
            answers = self.get_task_answers(f"{d.id}.{i}", self.test_user_1)
            self.assertEqual(len(answers), 1)
