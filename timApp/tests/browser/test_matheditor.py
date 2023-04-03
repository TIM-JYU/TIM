from selenium.webdriver.common.keys import Keys
from timApp.tests.browser.browsertest import BrowserTest


class MathEditorTest(BrowserTest):
    def test_add_formula(self):
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
        openformulabutton = self.find_element(xpath="//button[@title='Ctrl+e']")
        openformulabutton.click()
        mathfieldparent = self.find_element(xpath="//span[@class='mq-textarea']")
        mathfield = self.find_element(xpath="//textarea", parent=mathfieldparent)
        mathfield.send_keys("12345")
        latexfield = self.find_element(xpath="//textarea[@name='math-editor-output']")
        latexfield.send_keys(Keys.BACKSPACE)
        saveformulabutton = self.find_element(xpath="//button[@title='Ctrl+s']")
        saveformulabutton.click()
        aceinput = self.find_element(xpath="//div[@class='ace_line'][2]")
        self.assertEqual("1234", aceinput.text)

    def test_edit_formula(self):
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
        aceinput = self.find_element(xpath="//textarea[@class='ace_text-input']")
        aceinput.send_keys("$12345$")
        aceinput.send_keys(Keys.ARROW_LEFT)
        openformulabutton = self.find_element(xpath="//button[@title='Ctrl+e']")
        openformulabutton.click()
        latexfield = self.find_element(xpath="//textarea[@name='math-editor-output']")
        self.assertEqual("12345", latexfield.get_attribute("value"))
        latexfield.send_keys(Keys.BACKSPACE)
        saveformulabutton = self.find_element(xpath="//button[@title='Ctrl+s']")
        saveformulabutton.click()
        aceinput = self.find_element(xpath="//div[@class='ace_line'][3]")
        self.assertEqual("$1234$", aceinput.text)

    def test_pareditor(self):
        self.login_browser_quick_test1()
        self.login_test1()
        d = self.create_doc()
        self.goto_document(d)
        self.find_element_avoid_staleness(".addBottom", click=True)
        self.wait_for_editor_load()
        self.wait_until_present_and_vis(".ace_content")
        texbutton = self.find_element_by_text("TeX", "a")
        texbutton.click()
        openformulabutton = self.find_element(xpath="//button[@title='Ctrl+e']")
        openformulabutton.click()
        latexfield = self.find_element(xpath="//textarea[@name='math-editor-output']")
        latexfield.send_keys("12345")
        saveformulabutton = self.find_element(xpath="//button[@title='Ctrl+s']")
        saveformulabutton.click()
        savepareditorbutton = self.find_element(
            xpath="//button[@title='Save (Ctrl-S)']"
        )
        savepareditorbutton.click()
        mord = self.find_element(xpath="//span[@class='mord']")
        self.assertEqual("12345", mord.text)
        self.save_screenshot()
