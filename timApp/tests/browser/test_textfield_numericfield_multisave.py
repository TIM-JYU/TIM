from timApp.tests.browser.browsertest import BrowserTest


class TextfieldPluginTest(BrowserTest):

    # Textfield content moves slightly to the left (1 pixel?) on reload, and thus needs higher local tolerance to pass the test.
    def get_screenshot_tolerance(self) -> float:
        return 0.1

    def test_textfield_numericfield_multisave(self):
        self.login_browser_quick_test1()
        self.login_test1()
        self.accept_consent()
        d = self.create_doc(initial_par="""
#- {plugin="textfield" #t1}
cols: 7
autosave: false
#- {plugin=numericfield #t2}
cols: 7
autosave: false
#- {plugin=multisave #t3}
        """)

        # Test Case 1 - expected success in both fields after Save-button click and page refresh (= db get)

        self.goto_document(d)
        self.wait_until_present('#t1 input')
        input = self.find_element_and_move_to('#t1 input')
        input.send_keys('Aku Ankka')
        self.wait_until_present('#t2 input')
        input2 = self.find_element_and_move_to('#t2 input')
        input2.send_keys('2.75')
        self.find_element('.breadcrumb .active').click()
        par = self.find_element_avoid_staleness('#pars')  # par for save screenshot
        buttonPar = self.find_element_avoid_staleness('#t3 > tim-plugin-loader > div') # for button click
        #self.save_element_screenshot(par, 'fields_before_answer') # is not needed once taken
        self.assert_same_screenshot(par, ['textfield/fields_before_answer'])
        runbutton = buttonPar.find_element_by_css_selector('button') # Find multisave button element
        runbutton.click()
        self.refresh() # refresh is created in browsertext.py
        self.wait_until_present('#pars')
        par = self.find_element_avoid_staleness('#pars') # recall for save screenshot
        self.wait_until_present('#t1 input') # Wait for first input (textfield)
        self.wait_until_present('#t2 input') # Wait for second input (numericfield)
        #self.save_element_screenshot(par, 'fields_after_answer') # is not needed once taken (again)
        self.assert_same_screenshot(par, ['textfield/fields_after_answer'])


        # Test Case 2 - expected previously saved value in numericField, as it refuses to save empty input

        self.goto_document(d)
        self.refresh()  # refresh is created in browsertext.py
        self.wait_until_present('#t1 input')
        input = self.find_element_and_move_to('#t1 input')
        input.clear()
        input.send_keys(' ')
        self.wait_until_present('#t2 input')
        input2 = self.find_element_and_move_to('#t2 input')
        input2.clear()
        input2.send_keys(' ')
        self.find_element('.breadcrumb .active').click()
        buttonPar = self.find_element_avoid_staleness('#t3 > tim-plugin-loader > div')
        runbutton = buttonPar.find_element_by_css_selector('button') # Find multisave button element
        runbutton.click()
        self.refresh()  # refresh is created in browsertext.py
        self.wait_until_present('#pars')
        par = self.find_element_avoid_staleness('#pars')  # recall for save screenshot
        self.wait_until_present('#t1 input') # Wait for first input (textfield)
        self.wait_until_present('#t2 input') # Wait for second input (numericfield)
        #self.save_element_screenshot(par, 'fields_after_answer_switch')  # is not needed once taken (again)
        self.assert_same_screenshot(par, ['textfield/fields_after_answer_switch'])
