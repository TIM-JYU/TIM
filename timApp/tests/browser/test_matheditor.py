"""
Runs tests for math editor
"""

__authors__ = ["Daniel Juola"]
__license__ = "MIT"
__date__ = "31.3.2023"

from selenium.webdriver.common.keys import Keys
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
