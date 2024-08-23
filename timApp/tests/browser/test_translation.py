from selenium.webdriver.common.by import By
from timApp.document.editing.routes import mark_as_translated
from timApp.tests.browser.browsertest import BrowserTest, PREV_ANSWER


class TranslationTest(BrowserTest):
    def test_no_wrong_block_id_hint(self):
        self.login_browser_quick_test1()
        self.login_test1()
        src = self.create_doc(
            initial_par="""
``` {#suttu plugin="csPlugin"}
type: text
```
"""
        )
        src_par = src.document.get_paragraphs()[0]
        src_t = self.create_translation(src)
        src_t_par = src_t.document.get_paragraphs()[0]
        mark_as_translated(src_t_par)
        src_t_par.save()
        dest = self.create_doc(
            initial_par=f"""
#- {{rd="{src.id}" rp="{src_par.id}"}}
"""
        )
        dest_t = self.create_translation(dest)
        dest_t_par = dest_t.document.get_paragraphs()[0]
        dest_t_par.set_attr("rd", str(src_t.id))
        dest_t_par.set_attr("rp", src_t_par.id)
        mark_as_translated(dest_t_par)
        dest_t_par.save()

        def save(text):
            textarea = self.find_element_and_move_to("#suttu textarea")
            textarea.send_keys(text)
            par = self.find_element_avoid_staleness("#suttu > tim-plugin-loader > div")
            par.find_element(by=By.CSS_SELECTOR, value="button").click()
            self.wait_until_present_and_vis("answerbrowser")

        self.goto_document(dest_t)
        self.wait_until_present_and_vis("#suttu textarea")
        save("dest translated")
        self.goto_document(dest)
        self.wait_until_present_and_vis("#suttu textarea")
        save(" | dest unstranslated")
        self.goto_document(dest_t)
        self.wait_until_present_and_vis("#suttu textarea")
        self.find_element_and_move_to("#suttu textarea")
        self.find_element(PREV_ANSWER).click()
        self.wait_until_hidden(f"answerbrowser .loading")
        self.should_not_exist("answerbrowser .alert-danger")
        value = self.find_element("#suttu textarea").get_attribute("value")
        self.assertEqual("dest translated", value)
