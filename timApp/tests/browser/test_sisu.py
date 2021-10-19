from selenium.webdriver.common.by import By

from timApp.auth.accesstype import AccessType
from timApp.document.docinfo import DocInfo
from timApp.sisu.scimusergroup import ScimUserGroup
from timApp.tests.browser.browsertest import BrowserTest
from timApp.timdb.sqa import db
from timApp.user.special_group_names import TEACHERS_GROUPNAME
from timApp.user.usergroup import UserGroup


class SisuTest(BrowserTest):
    def test_sisu_assessments_block(self):
        """Test that SCIM API can be blocked and relevant error message is displayed to the user"""

        # Create dummy Sisu group bindings for test user 1 (teacher) and test user 2 (student)
        self.login_test1()
        ug_teachers = UserGroup.create("testcourse1-responsible-teachers")
        ug_students = UserGroup.create("testcourse1-students")

        self.test_user_1.add_to_group(ug_teachers, None)
        self.test_user_1.add_to_group(UserGroup.get_by_name(TEACHERS_GROUPNAME), None)
        self.test_user_2.add_to_group(ug_students, None)
        self.test_user_3.add_to_group(ug_students, None)

        db.session.commit()

        d = self.create_doc()
        ug_students.admin_doc = d.block
        ug_students.admin_doc.add_rights([ug_teachers], AccessType.manage)

        db.session.add(
            ScimUserGroup(
                group_id=ug_teachers.id,
                external_id="jy-CUR-00000-responsible-teachers",
            )
        )
        db.session.add(
            ScimUserGroup(
                group_id=ug_students.id,
                external_id="jy-CUR-00000-students",
            )
        )

        db.session.commit()

        d = self.create_doc(
            initial_par=f"""
```{{plugin="multisave"}}
buttonText: "Lähetä arvioinnit Sisuun"
destCourse: jy-CUR-00000
showInView: true
group: {ug_students.name}
```"""
        )

        with self.temp_config(
            {
                "SISU_ASSESSMENTS_URL": None,
                "SISU_ASSESSMENTS_DISABLED_MESSAGE": "Assessments blocked message!",
            }
        ):
            self.do_test_scim_block(d)

    def do_test_scim_block(self, test_doc: DocInfo):
        self.login_browser_quick_test1()
        self.goto_document(test_doc)
        self.wait_until_present("#HELP_PAR")

        self.drv.find_element(
            By.CSS_SELECTOR, "div:nth-child(1) > .timButton:nth-child(1)"
        ).click()

        self.wait_until_present("#modal-body")
        dialog_el = self.drv.find_element(
            By.CSS_SELECTOR, "tim-dialog-frame div.modal-dialog"
        )
        self.assert_same_screenshot(
            dialog_el, "sisu/blocked_dialog", move_to_element=True
        )
