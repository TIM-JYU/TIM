from selenium.webdriver.common.keys import Keys
from timApp.tests.browser.browsertest import BrowserTest


class MathEditorTest(BrowserTest):
    def test_formula_editor(self):
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
        self.save_screenshot()
