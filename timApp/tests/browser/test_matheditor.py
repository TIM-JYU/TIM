from timApp.tests.browser.browsertest import BrowserTest


class MathEditorTest(BrowserTest):
    def test_formula_editor(self):
        self.login_browser_quick_test1()
        self.login_test1()
        d = self.create_doc(
            initial_par="""
        {#mqtest plugin="csPlugin"}
type: md
# highlight:
placeholder: "Kirjoita kaavaa"
stem: "Kirjoita alle kaavaa"
button: Tallenna
rows: 1 
editorModes: "01"
editorMode: 1
autorun: true
autoupdate: 200
formulaEditor: true
                """
        )
        self.goto_document(d)
        open_formula_button = self.find_element_by_text("Add formula", "button")
        open_formula_button.click()
        self.save_screenshot()
