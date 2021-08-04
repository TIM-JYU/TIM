from selenium.webdriver.common.by import By

from timApp.tests.browser.browsertest import BrowserTest, PREV_ANSWER


class CsPluginTest(BrowserTest):
    def test_csplugin_translation(self):
        self.login_browser_quick_test1()
        self.login_test1()
        d = self.create_doc(initial_par="""
#- {plugin=csPlugin #py}
type: python
pointsRule:
  code: 1
  expectCode: .*Hei maailma.*
        """)
        dt = self.create_translation(d)
        dt.document.set_settings({
            'global_plugin_attrs': {'all': {'lang': 'en'}},
            # Hide the out-of-date decoration so we don't have to update the screenshot because of it.
            'css': '.troutofdate::before { display: none; }',
        })
        tr_par = dt.document.get_paragraphs()[1]
        tr_par.set_markdown("""
type: python
pointsRule:
  code: 1
  expectCode: .*Hello world.*
        """)
        tr_par.save()
        self.goto_document(d)
        self.wait_until_present_and_vis('#py textarea')
        textarea = self.find_element_and_move_to('#py textarea')
        textarea.send_keys('print("Hei maailma!")')
        par = self.find_element_avoid_staleness('#py > tim-plugin-loader > div')
        runbutton = par.find_element(by=By.CSS_SELECTOR, value='button')
        runbutton.click()
        self.wait_until_present_and_vis('.console')
        self.wait_until_present_and_vis('answerbrowser')
        ptxt = self.find_element_by_text('Points:', 'span')
        self.assertEqual('Points: 1', ptxt.text)

        self.goto_document(dt)

        self.wait_until_present_and_vis('#py textarea')
        textarea = self.find_element_and_move_to('#py textarea')
        textarea.clear()
        textarea.send_keys('print("Hello world!")')
        self.get_uninteractable_element().click()
        par = self.find_element_avoid_staleness('#py > tim-plugin-loader > div')
        self.assert_same_screenshot(par, ['csplugin/python_before_answer'])
        runbutton = par.find_element(by=By.CSS_SELECTOR, value='button')
        runbutton.click()
        self.wait_until_present_and_vis('.console')
        self.wait_until_present_and_vis('answerbrowser')
        ptxt = self.find_element_by_text('Points:', 'span')
        self.assertEqual('Points: 1', ptxt.text)
        self.get_uninteractable_element().click()
        self.assert_same_screenshot(par, 'csplugin/python_after_answer', attempts=2)

        # post a second answer because otherwise clicking previous answer does not do anything
        textarea.send_keys(' ')
        runbutton.click()
        self.wait_until_hidden('#py tim-loading')

        self.wait_and_click(PREV_ANSWER)
        self.wait_until_hidden('.console')
        # Wait until answer is replaced in HTML
        # self.wait.until(ec.staleness_of(par.find_element(by=By.CSS_SELECTOR, value='*')))
        par = self.find_element('#py > tim-plugin-loader > div')

        # Wait until the height workaround completes (see answerbrowser3.ts)
        # self.wait.until(expected_conditions.presence_of_element_located((By.XPATH, "//*[@id='py'][@style='opacity: 1;']")))

        # TODO: Why is this slightly different from python_before_answer ?
        self.assert_same_screenshot(par, 'csplugin/python_after_answer_switch')
        self.verify_answer_content(
            f'{d.id}.py', 'usercode', 'print("Hello world!") ', self.test_user_1, expected_count=3,
        )
        # The answers should always be saved under the original document, so the translated document should
        # not have answers.
        self.verify_answer_content(
            f'{dt.id}.py', 'usercode', '', self.test_user_1, expected_count=0,
        )

    def make_text_and_answer(self, d):
        self.goto_document(d)
        self.wait_until_present_and_vis('#text textarea')
        textarea = self.find_element_and_move_to('#text textarea')
        textarea.send_keys('print("Hello world!")')
        self.get_uninteractable_element().click()
        par = self.find_element_avoid_staleness('#text > tim-plugin-loader > div')
        runbutton = par.find_element(by=By.CSS_SELECTOR, value='button')
        runbutton.click()
        self.wait_until_present_and_vis('answerbrowser')
        self.wait_until_hidden('tim-loading')
        return textarea, runbutton

    def test_csplugin_saveindicators(self):
        """
        Check that savebutton is enabled/disabled by whatever is the current desired logic. For now:
        disableUnchanged false or missing: savebutton always enabled
        disableUnchanged true: savebutton disabled if saved and input doesn't change
        Also check yellow margin is (un)hidden and savedText (dis)appears
        """

        self.login_browser_quick_test1()
        self.login_test1()
        d = self.create_doc(initial_par="""
#- {plugin=csPlugin #text}
type: text
        """)
        textarea, runbutton = self.make_text_and_answer(d)
        self.assertTrue(runbutton.is_enabled())
        savedtext = self.find_element('.savedText')
        self.assertTrue( savedtext.is_displayed())
        self.should_not_exist('.csRunNotSaved')
        d = self.create_doc(initial_par="""
#- {plugin=csPlugin #text}
type: text
disableUnchanged: true
                """)
        textarea, runbutton = self.make_text_and_answer(d)
        self.assertFalse(runbutton.is_enabled())
        savedtext = self.find_element('.savedText')
        self.assertTrue(savedtext.is_displayed())
        self.should_not_exist('.csRunNotSaved')
        textarea.send_keys("more input, let me save")
        self.assertTrue(runbutton.is_enabled())
        self.should_not_exist('.savedText')
        margin = self.find_element('.csRunNotSaved')
        self.assertTrue(margin.is_displayed())

    def test_csplugin_require_type(self):
        self.login_test1()
        d = self.create_doc(initial_par="""
#- {plugin=csPlugin}
stem: ""
        """)
        self.assert_content(self.get(d.url, as_tree=True), ['Attribute "type" is required.'])
