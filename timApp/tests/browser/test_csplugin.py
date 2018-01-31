from time import sleep

from selenium.webdriver.common.by import By
from selenium.webdriver.support import expected_conditions as ec

from timApp.tests.browser.browsertest import BrowserTest, PREV_ANSWER


class CsPluginTest(BrowserTest):
    def test_csplugin_translation(self):
        self.login_browser_quick_test1()
        self.login_test1()
        d = self.create_doc(initial_par="""
#- {plugin=csPlugin #py}
type: python
        """)
        dt = self.create_translation(d)
        dt.document.set_settings({'global_plugin_attrs': {'all': {'lang': 'en'}}})
        self.goto_document(dt)
        par = self.find_element('#py')
        self.touch(par)
        self.wait_until_hidden('answerbrowserlazy')
        textarea = self.find_element_and_move_to('#py textarea')
        textarea.send_keys('print("Hello world!")')
        self.find_element('.breadcrumb .active').click()
        self.assert_same_screenshot(par, ['csplugin/python_before_answer', 'csplugin/python_before_answer_alt'])
        runbutton = par.find_element_by_css_selector('button')
        runbutton.click()
        self.wait_until_present('.console')
        self.wait_until_present('answerbrowser')
        self.assert_same_screenshot(par, 'csplugin/python_after_answer')

        self.wait_and_click(PREV_ANSWER)
        self.wait_until_hidden('.console')
        # Wait until answer is replaced in HTML
        # self.wait.until(ec.staleness_of(par.find_element_by_css_selector('*')))
        par = self.find_element('#py')

        # Wait until the height workaround completes (see answerbrowser3.ts)
        # self.save_screenshot()
        # self.wait.until(ec.presence_of_element_located((By.XPATH, "//*[@id='py'][@style='opacity: 1;']")))

        # TODO: Why is this slightly different from python_before_answer ?
        self.assert_same_screenshot(par, 'csplugin/python_after_answer_switch')
