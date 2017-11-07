from selenium.webdriver import ActionChains
from selenium.webdriver.common.by import By
from selenium.webdriver.support import expected_conditions as ec

from timApp.tests.browser.browsertest import BrowserTest
from timApp.timdb.models.docentry import DocEntry
from timApp.timdb.userutils import get_admin_group_id


class AnswerBrowserTest(BrowserTest):

    def test_referenced_area_plugin(self):
        self.login_browser_quick_test1()
        self.login_test1()
        d = self.create_doc(from_file='example_docs/multiple_mmcqs.md')
        d2 = self.create_doc(initial_par=f'#- {{rd={d.id} ra=a1}}')
        self.check_reference_answerbrowser_ok(d2)
        db = self.get_db()

        # even if the original document is not accessible, browsing answers should work in the other document
        d.block.set_owner(get_admin_group_id())
        db.session.commit()
        self.check_reference_answerbrowser_ok(d2)

    def check_reference_answerbrowser_ok(self, d: DocEntry):
        self.goto_document(d)
        submitbutton = self.drv.find_element_by_css_selector('#mmcqexample button')

        # hover mouse over par element to activate answer browser
        ActionChains(self.drv).move_to_element(submitbutton).perform()

        # Due to plugin refresh on hover, we need to fetch the element again.
        submitbutton = self.drv.find_element_by_css_selector('#mmcqexample button')
        ActionChains(self.drv).move_to_element(submitbutton).perform()
        submitbutton.click()
        prevanswer = 'answerbrowser .prevAnswer'
        self.wait.until(ec.element_to_be_clickable((By.CSS_SELECTOR, prevanswer)))
        self.drv.find_element_by_css_selector(prevanswer).click()
        self.should_not_exist('answerbrowser .alert-danger')
