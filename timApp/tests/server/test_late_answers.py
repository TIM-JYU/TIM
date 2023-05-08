"""Server tests for xxx."""
from datetime import timedelta

from timApp.auth.accesstype import AccessType
from timApp.tests.server.timroutetest import TimRouteTest
from timApp.timdb.sqa import db
from timApp.util.utils import get_current_time


class LateAnswersTest(TimRouteTest):
    def test_late_answers(self):
        self.login_test1()
        d = self.create_doc(
            initial_par="""
#- {plugin=textfield #t}
``` {#oneAttempt plugin="textfield"}
answerLimit: 1
```
"""
        )
        d.document.set_settings({"answer_submit_time_tolerance": 0})
        self.test_user_2.grant_access(
            d, AccessType.view, accessible_to=get_current_time()
        )
        db.session.commit()
        self.login_test2()
        a = self.post_answer("textfield", f"{d.id}.t", user_input={"c": "x"})
        err = "Your view access to this document has expired, so this answer was saved but marked as invalid."
        self.assertEqual(
            {
                "errors": [err],
                "savedNew": 1,
                "valid": False,
                "web": {"result": "saved"},
            },
            a,
        )
        self.test_user_2.grant_access(
            d, AccessType.view, accessible_to=get_current_time() - timedelta(minutes=5)
        )
        db.session.commit()
        self.post_answer(
            "textfield", f"{d.id}.t", user_input={"c": "x"}, expect_status=403
        )
        d.document.set_settings(
            {"answer_grace_period": 6, "answer_submit_time_tolerance": 0}
        )
        a = self.post_answer("textfield", f"{d.id}.t", user_input={"c": "z"})
        self.assertEqual(
            {
                "errors": [err],
                "savedNew": 2,
                "valid": False,
                "web": {"result": "saved"},
            },
            a,
        )
        d.document.set_settings({"answer_grace_period": "x"})
        self.post_answer(
            "textfield", f"{d.id}.t", user_input={"c": "x"}, expect_status=403
        )
        self.assertTrue(all(not a.valid for a in self.test_user_2.answers.all()))
        self.test_user_2.grant_access(
            d, AccessType.view, accessible_to=get_current_time()
        )
        db.session.commit()

        d.document.set_settings({"answer_submit_time_tolerance": 5000})
        a = self.post_answer("textfield", f"{d.id}.t", user_input={"c": "x"})
        self.assertEqual({"savedNew": 3, "valid": True, "web": {"result": "saved"}}, a)
        # Don't force validity to true if plugin deems answer as false
        self.post_answer("textfield", f"{d.id}.oneAttempt", user_input={"c": "x"})
        a = self.post_answer("textfield", f"{d.id}.oneAttempt", user_input={"c": "y"})
        self.assertEqual(
            {
                "web": {"result": "saved"},
                "savedNew": 5,
                "valid": False,
                "errors": ["You have exceeded the answering limit."],
            },
            a,
        )
