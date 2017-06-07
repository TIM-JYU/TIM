from selenium.webdriver import ActionChains
from selenium.webdriver.common.by import By
from selenium.webdriver.common.keys import Keys
from selenium.webdriver.remote.webelement import WebElement
from selenium.webdriver.support import expected_conditions as ec

from tests.browser.browsertest import BrowserTest


class ParEditorTest(BrowserTest):
    def wait_for_preview_to_finish(self):
        self.wait.until_not(ec.text_to_be_present_in_element((By.CSS_SELECTOR, '#previewDiv'), '...'))

    def test_add_bottom_focus_switch(self):
        """Ensures:

        * editor is opened from the "Add paragraph" button at the bottom
        * editor gets focus automatically on open
        * preview works
        * switching between plain text area and Ace works
        """
        self.login_browser_quick_test1()
        self.login_test1()
        d = self.create_doc()
        self.goto_document(d)
        add_bottom = self.drv.find_element_by_css_selector('.addBottom')
        add_bottom.click()
        pareditor = self.drv.find_element_by_css_selector('pareditor')
        ActionChains(self.drv).send_keys('# hello\n\nworld').perform()
        preview = self.find_preview_element(pareditor)
        preview.click()  # stop cursor blinking
        self.wait_for_preview_to_finish()
        self.assert_same_screenshot(pareditor, 'pareditor/ace_hello_world')
        change_editor_button = pareditor.find_element_by_xpath("//button[contains(text(),'Editor')]")
        change_editor_button.click()
        ActionChains(self.drv).send_keys('!').perform()
        preview.click()  # stop cursor blinking
        self.wait_for_preview_to_finish()
        self.assert_same_screenshot(pareditor, 'pareditor/textarea_hello_world')
        change_editor_button.click()

        # after deleting the '!', the screenshot should be the same
        ActionChains(self.drv).send_keys(Keys.PAGE_DOWN, Keys.BACKSPACE).perform()
        preview.click()  # stop cursor blinking
        self.wait_for_preview_to_finish()
        self.assert_same_screenshot(pareditor, 'pareditor/ace_hello_world')

    @staticmethod
    def find_preview_element(pareditor: WebElement) -> WebElement:
        preview = pareditor.find_element_by_css_selector('.previewcontent')
        return preview
