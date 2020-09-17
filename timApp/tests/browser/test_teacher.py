import warnings

from timApp.tests.browser.browsertest import BrowserTest
from timApp.timdb.sqa import db
from timApp.user.user import User, UserInfo
from timApp.user.usergroup import UserGroup


class TeacherTest(BrowserTest):

    def test_first_answer_loading(self):
        """ First loaded answers in teacher view matches the first user that browser selects from a group """

        def check_col():
            header_row = self.find_element('.ui-grid-header-cell-row')
            username_div = header_row.find_element_by_xpath(f"//{'span'}[contains(text(),'{'Username'}')]")
            col_class = username_div.find_element_by_xpath("./../../..").get_attribute('class').split()[-1]
            return col_class

        self.login_browser_test1()
        self.login_test1()
        nameless_user, _ = User.create_with_group(UserInfo(username="nameless"))
        lowercase_user, _ = User.create_with_group(UserInfo(username="lowercase", full_name="tim lower"))
        ug_with_nameless = UserGroup.create("ug_with_nameless")
        ug_with_nameless.users.append(nameless_user)
        ug_with_nameless.users.append(self.test_user_1)
        ug_with_lowercase = UserGroup.create("ug_with_lowercase")
        ug_with_lowercase.users.append(lowercase_user)
        ug_with_lowercase.users.append(self.test_user_1)
        ug_with_nameless.admin_doc = self.create_doc().block
        ug_with_lowercase.admin_doc = self.create_doc().block

        d1 = self.create_doc(initial_par="""
#- {plugin=textfield #t}
form: false
        """, settings={'group': "ug_with_nameless"})
        self.add_answer(d1, 't','tu1 here')
        d1_ans_table = {'nameless': '', 'testuser1': 'tu1 here'}

        d2 = self.create_doc(initial_par="""
#- {plugin=textfield #t}
form: false
        """, settings={'group': "ug_with_lowercase"})
        self.add_answer(d2, 't', 'tu1 here')
        d2_ans_table = {'lowercase': '', 'testuser1': 'tu1 here'}

        db.session.commit()

        self.goto_document(d1, 'teacher')
        selected_username = self.find_element('.ui-grid-row-selected .' + check_col()).text
        selected_answer = self.find_element('#t .textfieldNoSaveDiv input').get_attribute('value')
        self.assertEqual(d1_ans_table[selected_username], selected_answer)

        self.goto_document(d2, 'teacher')
        selected_username = self.find_element('.ui-grid-row-selected .' + check_col()).text
        selected_answer = self.find_element('#t .textfieldNoSaveDiv input').get_attribute('value')
        self.assertEqual(d2_ans_table[selected_username], selected_answer)
