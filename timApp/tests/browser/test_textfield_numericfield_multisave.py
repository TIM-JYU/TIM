from time import sleep

from selenium.webdriver.common.by import By
from selenium.webdriver.support import expected_conditions

from timApp.tests.browser.browsertest import BrowserTest, PREV_ANSWER


class TextfieldPluginTest(BrowserTest):
    def test_textfield_translation(self):
        self.login_browser_quick_test1()
        self.login_test1()
        self.accept_consent()
        d = self.create_doc(initial_par="""
#- {plugin=textfield #t1} 
cols: 7
autosave: false
#- {plugin=numericfield #t2}
cols: 7
autosave: false
#- {plugin=multisave #t3}
        """)
        dt = self.create_translation(d)
        dt.document.set_settings({
            'global_plugin_attrs': {'all': {'lang': 'en'}},
            # Hide the out-of-date decoration so we don't have to update the screenshot because of it.
            'css': '.tr-outofdate::before { display: none; }',
        })
        self.goto_document(dt)
        par = self.find_element_avoid_staleness('#t1')
        # self.wait_until_hidden('tim-plugin-loader')

        self.wait_until_present('#t1 input')
        input = self.find_element_and_move_to('#t1 input')
        # sleep(0.5)
        input.send_keys('Aku Ankka')
        self.wait_until_present('#t2 input')
        input2 = self.find_element_and_move_to('#t2 input')
        input2.send_keys('2.75')
        self.find_element('.breadcrumb .active').click()
        par = self.find_element_avoid_staleness('#py > tim-plugin-loader > div') # maybe no?
        self.assert_same_screenshot(par, ['csplugin/python_before_answer', 'csplugin/python_before_answer_alt'])
        runbutton = par.find_element_by_name('button', self) # par.find_element_by_css_selector('button')
        runbutton.click()
        self.wait(2)
        self.refresh()
        self.wait_until_present('#t1 input', '#t2 input')
        self.assert_same_screenshot(par, 'csplugin/python_after_answer')

        # post a second answer because otherwise clicking previous answer does not do anything
        input.send_keys(' ')
        input2.send_keys(' ')
        runbutton.click() 
        self.wait_until_hidden('.csRunError')  # this has the "...running..." text

        self.wait_and_click(PREV_ANSWER)
        self.wait_until_hidden('.console')
        # Wait until answer is replaced in HTML
        # self.wait.until(ec.staleness_of(par.find_element_by_css_selector('*')))
        par = self.find_element('#py > tim-plugin-loader > div')

        # Wait until the height workaround completes (see answerbrowser3.ts)
        # self.wait.until(expected_conditions.presence_of_element_located((By.XPATH, "//*[@id='py'][@style='opacity: 1;']")))

        # TODO: Why is this slightly different from python_before_answer ?
        self.assert_same_screenshot(par, 'csplugin/python_after_answer_switch')
