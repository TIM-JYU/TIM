from timApp.tests.db.timdbtest import TimDbTest
from timApp.timdb.sqa import db


class NotifyTest(TimDbTest):
    def test_notify(self):
        d = self.create_doc()
        n = self.test_user_1.get_notify_settings(d)
        self.assertFalse(n["email_doc_modify"])
        self.assertFalse(n["email_comment_add"])
        self.assertFalse(n["email_comment_modify"])
        self.assertFalse(n["email_answer_add"])
        self.assertFalse(n["email_annotation_add"])
        self.assertFalse(n["email_annotation_modify"])
        self.test_user_1.set_notify_settings(
            d,
            doc_modify=True,
            comment_add=True,
            comment_modify=True,
            answer_add=True,
            annotation_add=True,
            annotation_modify=True,
        )
        db.session.commit()
        n = self.test_user_1.get_notify_settings(d)
        self.assertTrue(n["email_doc_modify"])
        self.assertTrue(n["email_comment_add"])
        self.assertTrue(n["email_comment_modify"])
        self.assertTrue(n["email_answer_add"])
        self.assertTrue(n["email_annotation_add"])
        self.assertTrue(n["email_annotation_modify"])
        self.test_user_1.set_notify_settings(
            d,
            doc_modify=False,
            comment_add=True,
            comment_modify=True,
            answer_add=True,
            annotation_add=True,
            annotation_modify=False,
        )
        db.session.commit()
        n = self.test_user_1.get_notify_settings(d)
        self.assertFalse(n["email_doc_modify"])
        self.assertTrue(n["email_comment_add"])
        self.assertTrue(n["email_comment_modify"])
        self.assertTrue(n["email_answer_add"])
        self.assertTrue(n["email_annotation_add"])
        self.assertFalse(n["email_annotation_modify"])
