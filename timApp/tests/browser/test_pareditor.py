from time import sleep

from selenium.webdriver import ActionChains
from selenium.webdriver.common.by import By
from selenium.webdriver.common.keys import Keys
from selenium.webdriver.remote.webelement import WebElement
from selenium.webdriver.support import expected_conditions as ec

from timApp.tests.browser.browsertest import BrowserTest, find_button_by_text
from timApp.timdb.sqa import db


def get_change_editor_button(pareditor) -> WebElement:
    change_editor_button = pareditor.find_elements_by_css_selector('.editorOptions label')[1]
    return change_editor_button


def get_cancel_button(pareditor) -> WebElement:
    button = find_button_by_text(pareditor, 'Cancel')
    return button


class ParEditorTest(BrowserTest):
    def wait_for_preview_to_finish(self):
        self.wait.until_not(ec.text_to_be_present_in_element((By.CSS_SELECTOR, '.previewDiv'), '...'))

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
        self.open_editor_from_bottom()
        pareditor = self.get_editor_element()
        ActionChains(self.drv).send_keys('# hello\n\nworld').perform()
        preview = self.find_element_avoid_staleness('.previewcontent', parent=pareditor)
        self.wait_for_preview_to_finish()
        ActionChains(self.drv).move_to_element(preview).perform()  # avoids having mouse above a toolbar button
        self.assert_same_screenshot(pareditor, [
            'pareditor/ace_hello_world',
            'pareditor/ace_hello_world_2',
            'pareditor/ace_hello_world_3',
        ])
        change_editor_button = get_change_editor_button(pareditor)
        change_editor_button.click()
        self.wait_for_editor_load()
        ActionChains(self.drv).send_keys('!').perform()
        preview.click()  # stop cursor blinking
        self.wait_for_preview_to_finish()
        self.assert_same_screenshot(pareditor, 'pareditor/textarea_hello_world', move_to_element=True)
        change_editor_button.click()
        self.wait_for_editor_load()
        self.wait_until_present_and_vis('.ace_content')
        # after deleting the '!', the screenshot should be the same
        ActionChains(self.drv).send_keys(Keys.PAGE_DOWN, Keys.BACKSPACE).perform()
        self.wait_for_preview_to_finish()
        ActionChains(self.drv).move_to_element(preview).perform()
        self.assert_same_screenshot(pareditor, [
            'pareditor/ace_hello_world',
            'pareditor/ace_hello_world_2',
            'pareditor/ace_hello_world_3',
        ])

    def get_editor_element(self) -> WebElement:
        pareditor = self.drv.find_element_by_css_selector('pareditor')
        return pareditor

    def open_editor_from_bottom(self):
        sleep(0.1)
        self.find_element_avoid_staleness('.addBottom', click=True)
        self.wait_for_editor_load()
        self.wait_until_present_and_vis('.ace_content')

    def test_autocomplete(self):
        self.login_browser_quick_test1()
        self.login_test1()
        d = self.create_doc(initial_par='words in the document')
        prefs = self.current_user.get_prefs()
        prefs.use_document_word_list = True
        prefs.word_list = '\n'.join(('cat', 'dog', 'mouse'))
        self.current_user.set_prefs(prefs)
        db.session.commit()
        self.goto_document(d)
        self.open_editor_from_bottom()
        pareditor = self.get_editor_element()
        cb = self.find_element_by_text('Autocomplete', 'label', parent=pareditor)
        cb.click()
        editor = self.find_element_and_move_to('.ace_editor')
        editor.click()
        ActionChains(self.drv).send_keys('d').perform()
        self.wait_for_preview_to_finish()
        self.assert_same_screenshot(pareditor, 'pareditor/autocomplete')
        prefs = self.current_user.get_prefs()
        prefs.use_document_word_list = False
        self.current_user.set_prefs(prefs)
        db.session.commit()
        get_cancel_button(pareditor).click()
        alert = self.drv.switch_to.alert
        alert.accept()
        self.goto_document(d)
        self.open_editor_from_bottom()
        pareditor = self.get_editor_element()
        ActionChains(self.drv).send_keys('d').perform()
        self.wait_for_preview_to_finish()
        self.assert_same_screenshot(pareditor,
                                    ['pareditor/autocomplete_no_document',
                                     ])
