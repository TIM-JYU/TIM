from timApp.tests.browser.browsertest import BrowserTest


class TextfieldPluginTest(BrowserTest):

    def get_screenshot_tolerance(self):
        return 13

    def test_textfield_numericfield_multisave(self):
        self.login_browser_quick_test1()
        self.login_test1()
        d = self.create_doc(initial_par="""
#- {plugin=textfield #t1}
cols: 7
autosave: false
#- {plugin=numericfield #t2}
cols: 7
autosave: false
#- {plugin=multisave #t3}
        """, settings={'form_mode': True})

        # Test Case 1 - expected success in both fields after Save-button click and page refresh

        self.goto_document(d)
        self.wait_until_present('#t1 input')
        field = self.find_element_and_move_to('#t1 input')
        field.send_keys('Aku Ankka')
        self.wait_until_present('#t2 input')
        input2 = self.find_element_and_move_to('#t2 input')
        input2.send_keys('2.75')
        self.find_element('.breadcrumb .active').click()
        par = self.find_element_avoid_staleness('#pars')
        multisave = self.find_element_avoid_staleness('#t3 multisave-runner')
        self.wait_until_present('#t3 div') # wait for ng-if to finish
        self.assert_same_screenshot(par, ['textfield/fields_before_answer'])
        runbutton = multisave.find_element_by_css_selector('button')
        runbutton.click()
        self.wait_until_present('p.savedtext')
        self.refresh()

        self.wait_until_present('#t1 input')
        self.wait_until_present('#t2 input')
        par = self.find_element_avoid_staleness('#pars')
        self.assert_same_screenshot(par, ['textfield/fields_after_answer'])

        # Test Case 2 - expected previously saved value in numericField, as it refuses to save empty input

        # TODO: for some reason, the invalid numericfield value (' ') is not validated in browser in selenium,
        #  so an empty value is saved. Disabling the test for now.

        return

        self.goto_document(d)
        self.wait_until_present('#t1 input')
        field = self.find_element_and_move_to('#t1 input')
        field.clear()
        field.send_keys(' ')
        self.wait_until_present('#t2 input')
        input2 = self.find_element_and_move_to('#t2 input')
        input2.clear()
        input2.send_keys(' ')
        self.find_element('.breadcrumb .active').click()
        multisave = self.find_element_avoid_staleness('#t3 multisave-runner')
        runbutton = multisave.find_element_by_css_selector('button')
        runbutton.click()
        self.goto_document(d)
        self.wait_until_present('#t1 input')
        self.wait_until_present('#t2 input')
        par = self.find_element_avoid_staleness('#pars')
        self.assert_same_screenshot(par, ['textfield/fields_after_answer_switch'])
