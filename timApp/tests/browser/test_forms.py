from timApp.auth.accesstype import AccessType
from timApp.tests.browser.browsertest import BrowserTest
from timApp.timdb.sqa import db


class FormTest(BrowserTest):

    def test_plugin_user_modifiers(self):
        """ Save and show answers in browser according to global field and useCurrentUser logic"""

        def wait_fields_loaded():
            self.wait_until_present('#a .textfieldNoSaveDiv input')
            self.wait_until_present('#GLO_b .textfieldNoSaveDiv input')
            self.wait_until_present('#c .textfieldNoSaveDiv input')
            self.wait_until_present('#save div')

        def send_inputs(ans: str):
            field = self.find_element_and_move_to('#a .textfieldNoSaveDiv input')
            field.send_keys(ans)
            field = self.find_element_and_move_to('#GLO_b .textfieldNoSaveDiv input')
            field.send_keys(ans)
            field = self.find_element_and_move_to('#c .textfieldNoSaveDiv input')
            field.send_keys(ans)
            multisave = self.find_element_avoid_staleness('#save tim-multisave')
            runbutton = multisave.find_element_by_css_selector('button')
            runbutton.click()
            self.wait_until_present('p.savedtext')

        def check_field_content(field: str, ans: str):
            ele = self.find_element('#' + field + ' .textfieldNoSaveDiv input')
            val = ele.get_attribute('value')
            self.assertEqual(ans, val)
        try:
            self.login_browser_quick_test1()
            self.login_test1()
            d = self.create_doc(initial_par="""
#- {#a plugin=textfield}
useCurrentUser: true

#- {#GLO_b plugin=textfield}

#- {#c plugin=textfield}

#- {plugin=multisave #save}
        """)

            self.goto_document(d)
            wait_fields_loaded()
            send_inputs('[tu1view]')

            self.test_user_2.grant_access(d, AccessType.teacher)
            db.session.commit()

            self.login_browser_quick_test2()
            self.goto_document(d)
            wait_fields_loaded()
            send_inputs('[tu2view]')

            self.login_browser_quick_test1()
            self.goto_document(d, 'teacher')
            wait_fields_loaded()
            check_field_content('a', '[tu1view]')
            check_field_content('GLO_b', '[tu1view][tu2view]')
            check_field_content('c', '[tu1view]')

            velp_hider = self.find_element('velp-selection i.glyphicon-minus')
            velp_hider.click()
            tu_2_selector = self.find_element('div[title = "Test user 2"]')
            tu_2_selector.click()

            check_field_content('a', '[tu1view]')
            check_field_content('GLO_b', '[tu1view][tu2view]')
            self.wait_until_val_present('#c .textfieldNoSaveDiv input', '[tu2view]')

            send_inputs('[tu1teacher_to_tu2]')

            self.login_browser_quick_test2()
            self.goto_document(d)
            wait_fields_loaded()
            check_field_content('a', '[tu2view]')
            check_field_content('GLO_b', '[tu1view][tu2view][tu1teacher_to_tu2]')
            check_field_content('c', '[tu2view][tu1teacher_to_tu2]')
        except Exception:
            self.save_screenshot("form_failure")
            raise
