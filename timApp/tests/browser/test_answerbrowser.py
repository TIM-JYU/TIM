from timApp.answer.answer import Answer
from timApp.auth.accesstype import AccessType
from timApp.document.docentry import DocEntry
from timApp.tests.browser.browsertest import BrowserTest, PREV_ANSWER
from timApp.timdb.sqa import db
from timApp.user.usergroup import UserGroup
from timApp.util.utils import static_tim_doc


class AnswerBrowserTest(BrowserTest):
    def test_referenced_area_plugin(self):
        self.login_browser_quick_test1()
        self.login_test1()
        d = self.create_doc(from_file=static_tim_doc("multiple_mmcqs.md"))
        d2 = self.create_doc(initial_par=f"#- {{rd={d.id} ra=a1}}")
        self.check_reference_answerbrowser_ok(d2)

    def check_reference_answerbrowser_ok(self, d: DocEntry):
        self.goto_document(d)
        selector = "#mmcqexample tim-mmcq button"
        self.find_element_avoid_staleness(selector, click=True)
        self.wait_and_click(PREV_ANSWER)
        self.should_not_exist("answerbrowser .alert-danger")

    def test_dim(self):
        self.login_browser_quick_test1()
        self.login_test1()
        ug = UserGroup.create("testusers23")
        ug.users.append(self.test_user_2)
        ug.users.append(self.test_user_3)
        ug.admin_doc = self.create_doc().block
        d = self.create_doc(
            initial_par="""
``` {#GLO_Y plugin="textfield"}
form: false
hideBrowser: false
```
``` {#GLO_N plugin="textfield"}
form: false
hideBrowser: false
```
``` {#normal_Y plugin="textfield"}
form: false
```
``` {#normal_N plugin="textfield"}
form: false
```
``` {#runner plugin="jsrunner"}
showInView: true
groups: [testusers23]
updateFields:
 - GLO_Y
 - GLO_N
 - normal_Y
 - normal_N
fields: []
program: |!!
!!
```

""",
            settings={"group": "testusers23"},
        )
        self.add_answer(d, "normal_Y", "tu2_normal", user=self.test_user_2)
        self.add_answer(d, "GLO_Y", "tu3_global", user=self.test_user_3)
        self.add_answer(d, "normal_Y", "tu3_normal", user=self.test_user_3)
        db.session.commit()

        self.goto_document(d, "teacher")

        def wait_fields_loaded():
            def wait_field_load(tid: str):
                self.wait_until_present_and_vis(f"#{tid} .textfieldNoSaveDiv input")

            wait_field_load("GLO_Y")
            wait_field_load("GLO_N")
            wait_field_load("normal_Y")
            wait_field_load("normal_N")

        wait_fields_loaded()
        velp_hider = self.find_element("velp-selection i.glyphicon-minus")
        velp_hider.click()

        # Should dim if no answers from target (or any users if global)
        def check_opacities():
            def check_opacity(tid: str, dimmed: bool):
                self.find_element_and_move_to(f"#{tid} .textfieldNoSaveDiv input")
                self.wait_until_present_and_vis(f"#{tid} answerbrowser")
                self.wait_until_hidden(f"#{tid} answerbrowser .loading")
                self.wait_until_hidden(f"#{tid} answerbrowser .updating")
                if dimmed:
                    self.find_element(f'#{tid} [style*="opacity: 0.3"]')
                else:
                    self.wait_until_hidden(f'#{tid} [style*="opacity: 0.3"]')

            check_opacity("GLO_Y", False)
            check_opacity("GLO_N", True)
            check_opacity("normal_Y", False)
            check_opacity("normal_N", True)

        # initial answerbrowser load triggers dimming
        check_opacities()
        tu_3_selector = self.find_element('div[title = "Test user 3"]')
        tu_3_selector.click()
        # user change triggers dimming
        check_opacities()
        self.goto_document(d, "teacher")
        wait_fields_loaded()
        runner = self.find_element_avoid_staleness("js-runner .timButton")
        runner.click()
        self.wait_until_hidden("js-runner tim-loading")
        # updatefields triggers dimming
        check_opacities()

    def test_no_state_overwrite(self):
        self.login_test1()
        d = self.create_doc(
            initial_par="""
``` {#Plugin1 plugin="csPlugin"}
type: text
```
"""
        )
        self.test_user_2.grant_access(d, AccessType.view)
        db.session.commit()
        self.login_browser_quick_test2()

        def do_browser_check(input: str, expect: str):
            self.goto_document(d)
            self.wait_until_present_and_vis("#Plugin1 textarea")
            self.set_network_state(False)
            textarea = self.find_element_and_move_to("#Plugin1 textarea")
            self.wait_until_present_and_vis(".alert-danger")
            textarea.send_keys(input)
            self.get_uninteractable_element().click()
            self.set_network_state(True)
            self.find_element_and_move_to("#Plugin1 textarea")
            self.wait_until_hidden(".alert-danger")
            val = textarea.get_attribute("value")
            self.assertEqual(val, expect)
            self.find_element("cs-runner button").click()
            self.wait_until_present_and_vis("answerbrowser")
            self.wait_until_hidden("tim-loading")
            self.find_element_and_move_to("tim-site-header")

        do_browser_check("Original input", "Original input")
        do_browser_check(", New input", "Original input, New input")
        answers = (
            self.test_user_2.answers.filter_by(task_id=f"{d.id}.Plugin1")
            .order_by(Answer.answered_on.desc())
            .all()
        )
        self.assertEqual(answers[1].content_as_json["usercode"], "Original input")
        self.assertEqual(
            answers[0].content_as_json["usercode"], "Original input, New input"
        )
        answers[0].users_all = []
        answers[1].valid = False
        db.session.commit()
        do_browser_check("New input over invalid", "New input over invalid")
        new_ans = (
            self.test_user_2.answers.filter_by(task_id=f"{d.id}.Plugin1")
            .order_by(Answer.answered_on.desc())
            .first()
        )
        self.assertEqual(new_ans.content_as_json["usercode"], "New input over invalid")
