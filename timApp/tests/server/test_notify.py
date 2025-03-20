from sqlalchemy import select

from timApp.auth.accesstype import AccessType
from timApp.document.docinfo import DocInfo
from timApp.document.randutils import random_id
from timApp.notification.notification import NotificationType
from timApp.notification.notify import (
    process_pending_notifications,
    notify_doc_watchers,
)
from timApp.notification.pending_notification import (
    PendingNotification,
    DocumentNotification,
)
from timApp.notification.send_email import sent_mails_in_testing
from timApp.tests.server.timroutetest import TimRouteTest
from timApp.timdb.sqa import db, run_sql


class NotifyTestBase(TimRouteTest):
    def setUp(self):
        super().setUp()
        sent_mails_in_testing.clear()

    def update_notify_settings(self, d, new_settings):
        self.json_post(f"/notify/{d.id}", new_settings)

    def prepare_doc(self, add_new_par=True) -> tuple[DocInfo, str, str]:
        self.login_test1()
        d = self.create_doc()
        title = d.title
        url = d.url
        self.test_user_2.grant_access(d, AccessType.view)
        db.session.commit()
        self.new_par(d.document, "test")
        self.assertEqual([], sent_mails_in_testing)
        self.login_test2()

        # testuser 2 will get notification from both additions
        # because pending notifications have not yet been processed
        self.update_notify_settings(
            d,
            {
                "email_comment_add": True,
                "email_comment_modify": False,
                "email_doc_modify": True,
                "email_answer_add": False,
                "email_annotation_add": False,
                "email_annotation_modify": False,
            },
        )
        self.login_test1()
        if add_new_par:
            self.new_par(d.document, "test")
        return d, title, url


class NotifyTest(NotifyTestBase):
    def test_notify(self):
        self.login_test1()
        d = self.create_doc()
        notify_url = f"/notify/{d.id}"
        n = self.get(notify_url)
        self.assertDictEqual(
            {
                "email_comment_add": False,
                "email_comment_modify": False,
                "email_doc_modify": False,
                "email_answer_add": False,
                "email_annotation_add": False,
                "email_annotation_modify": False,
            },
            n,
        )
        new_settings = {
            "email_comment_add": True,
            "email_comment_modify": False,
            "email_doc_modify": True,
            "email_answer_add": False,
            "email_annotation_add": True,
            "email_annotation_modify": True,
        }
        self.update_notify_settings(d, new_settings)
        n = self.get(notify_url)
        self.assertDictEqual(new_settings, n)

    def test_notify_email(self):
        d, title, url = self.prepare_doc()
        process_pending_notifications()

        self.assertEqual(1, len(sent_mails_in_testing))
        par_id0 = d.document.get_paragraphs()[0].get_id()
        par_id = d.document.get_paragraphs()[1].get_id()
        mail_from = "no-reply@tim.jyu.fi"
        self.assertEqual(
            {
                "mail_from": mail_from,
                "msg": f"Paragraph added: {url}#{par_id0}\n\nParagraph added: {url}#{par_id}",
                "rcpt": self.test_user_2.email,
                "reply_to": None,
                "subject": f"Someone added 2 paragraphs to the document {title}",
            },
            sent_mails_in_testing[-1],
        )

        self.post_par(d.document, "test2", par_id)
        process_pending_notifications()
        self.assertEqual(
            {
                "mail_from": mail_from,
                "msg": f"Paragraph modified: {url}#{par_id}",
                "rcpt": self.test_user_2.email,
                "reply_to": None,
                "subject": f"Someone modified a paragraph in the document {title}",
            },
            sent_mails_in_testing[-1],
        )

        self.test_user_2.grant_access(d, AccessType.edit)
        db.session.commit()
        self.new_par(d.document, "test")
        process_pending_notifications()
        pars = d.document.get_paragraphs()
        self.assertEqual(
            {
                "mail_from": mail_from,
                "msg": f"Paragraph added: {url}#{pars[2].get_id()}"
                " "
                f"(changes: http://localhost/diff/{d.id}/2/1/3/0 )\n"
                "\n"
                f"{pars[2].get_markdown()}",
                "rcpt": self.test_user_2.email,
                "reply_to": None,
                "subject": f"Someone added a paragraph to the document {title}",
            },
            sent_mails_in_testing[-1],
        )

        self.post_par(d.document, "test3", par_id)
        process_pending_notifications()
        pars = d.document.get_paragraphs()
        par_md = pars[1].get_markdown()
        self.assertEqual(
            {
                "mail_from": mail_from,
                "msg": f"Paragraph modified: {url}#{pars[1].get_id()}"
                " "
                f"(changes: http://localhost/diff/{d.id}/3/0/3/1 )\n"
                "\n"
                f"{par_md}",
                "rcpt": self.test_user_2.email,
                "reply_to": None,
                "subject": f"Someone modified a paragraph in the document {title}",
            },
            sent_mails_in_testing[-1],
        )

        self.delete_par(d, par_id)
        process_pending_notifications()
        self.assertEqual(
            {
                "mail_from": mail_from,
                "msg": f"Paragraph deleted: {url}"
                " "
                f"(changes: http://localhost/diff/{d.id}/3/1/4/0 )\n"
                "\n"
                f"{par_md}",
                "rcpt": self.test_user_2.email,
                "reply_to": None,
                "subject": f"Someone deleted a paragraph from the document {title}",
            },
            sent_mails_in_testing[-1],
        )

        self.test_user_2.grant_access(d, AccessType.teacher)
        db.session.commit()
        self.new_par(d.document, "test")
        process_pending_notifications()
        pars = d.document.get_paragraphs()
        self.assertEqual(
            {
                "mail_from": mail_from,
                "msg": f"Paragraph added by user 1 Test: {url}#{pars[-1].get_id()}"
                " "
                f"(changes: http://localhost/diff/{d.id}/4/0/5/0 )\n"
                "\n"
                f"{pars[-1].get_markdown()}",
                "rcpt": self.test_user_2.email,
                "reply_to": self.test_user_1.email,
                "subject": f"user 1 Test added a paragraph to the document {title}",
            },
            sent_mails_in_testing[-1],
        )

    def test_revoke_view_no_email(self):
        d, title, url = self.prepare_doc()
        process_pending_notifications()
        self.assertEqual(1, len(sent_mails_in_testing))
        self.test_user_2.remove_access(d.id, "view")
        db.session.commit()
        self.new_par(d.document, "test")
        process_pending_notifications()
        self.assertEqual(1, len(sent_mails_in_testing))

    def test_answer_link_email_and_null_doc_text_after_processing(self):
        d, title, url = self.prepare_doc()

        process_pending_notifications()
        [plug] = d.document.add_text(
            """
#- {plugin=csPlugin #t}
stem: test
        """
        )
        self.test_user_2.grant_access(d, AccessType.teacher)
        db.session.commit()
        self.post_comment(plug, public=True, text="Hello")
        process_pending_notifications()
        self.assertEqual(
            {
                "mail_from": "no-reply@tim.jyu.fi",
                "msg": "Comment posted by user 1 Test: "
                f"http://localhost/answers/{d.path}?task=t&user=testuser1&valid_answers_only=false\n"
                "\n"
                "Hello",
                "rcpt": "test2@example.com",
                "reply_to": "test1@example.com",
                "subject": f"user 1 Test posted a comment to the document {title}",
            },
            sent_mails_in_testing[-1],
        )
        pns = (
            run_sql(select(PendingNotification).filter_by(doc_id=d.id)).scalars().all()
        )
        for p in pns:
            if isinstance(p, DocumentNotification):
                self.assertIsNone(p.text)
            else:
                self.assertIsNotNone(p.text)


class NotifyFolderTest(NotifyTestBase):
    def test_folder_email(self):
        self.login_test1()
        t1_f = self.current_user.get_personal_folder()
        self.test_user_2.grant_access(t1_f, AccessType.view)
        db.session.commit()
        self.login_test2()
        self.update_notify_settings(
            t1_f,
            {
                "email_comment_add": True,
                "email_comment_modify": False,
                "email_doc_modify": True,
                "email_answer_add": False,
                "email_annotation_add": False,
                "email_annotation_modify": False,
            },
        )
        r = self.get("/notify/all")
        self.assertEqual(5, len(r))
        self.assertTrue(r[0]["item"]["isFolder"])
        self.login_test1()
        d = self.create_doc()
        self.new_par(d.document, "test")
        self.assertEqual(0, len(sent_mails_in_testing))
        self.test_user_2.grant_access(d, AccessType.view)
        db.session.commit()
        self.new_par(d.document, "test")
        process_pending_notifications()
        self.assertEqual(1, len(sent_mails_in_testing))

        self.login_test2()
        self.update_notify_settings(
            d,
            {
                "email_comment_add": True,
                "email_comment_modify": False,
                "email_doc_modify": True,
                "email_answer_add": False,
                "email_annotation_add": False,
                "email_annotation_modify": False,
            },
        )
        r = self.get("/notify/all")
        self.assertEqual(10, len(r))
        self.assertTrue(r[5]["item"]["isFolder"])
        self.assertFalse(r[0]["item"]["isFolder"])
        self.login_test1()
        self.new_par(d.document, "test")
        process_pending_notifications()
        self.assertEqual(2, len(sent_mails_in_testing))
        prefs = self.test_user_2.get_prefs()
        prefs.email_exclude = d.location
        self.test_user_2.set_prefs(prefs)
        db.session.commit()
        self.new_par(d.document, "test")
        process_pending_notifications()
        self.assertEqual(2, len(sent_mails_in_testing))

        prefs.email_exclude = "**invalid regex**"
        self.test_user_2.set_prefs(prefs)
        db.session.commit()
        self.new_par(d.document, "test")
        process_pending_notifications()
        self.assertEqual(3, len(sent_mails_in_testing))


class CutPasteNotifyTest(NotifyTestBase):
    def test_cut_paste(self):
        self.login_test1()
        d, _, _ = self.prepare_doc()
        par = d.document.get_paragraphs()[0]
        par2 = d.document.get_paragraphs()[1]
        par3 = par2.clone()
        par3.set_id(random_id())
        par3.set_markdown("hello")
        par3.save(add=True)
        process_pending_notifications()
        # d.document.clear_mem_cache()
        self.test_user_2.grant_access(d, AccessType.teacher)
        self.test_user_2.grant_access(d, AccessType.edit)
        db.session.commit()
        self.cut(d, par_start=par, par_end=par2)
        # print(d.document.export_markdown())
        process_pending_notifications()
        mail_from = "no-reply@tim.jyu.fi"
        self.assertEqual(
            {
                "mail_from": mail_from,
                "msg": "Paragraph deleted by user 1 Test: "
                f"{d.url} (changes: "
                f"http://localhost/diff/{d.id}/3/0/5/0 )\n"
                "\n"
                f'#- {{id="{par.get_id()}"}}\n'
                "test\n"
                "\n"
                f'#- {{id="{par2.get_id()}"}}\n'
                "test",
                "rcpt": "test2@example.com",
                "reply_to": "test1@example.com",
                "subject": "user 1 Test deleted a paragraph from the document document 2",
            },
            sent_mails_in_testing[-1],
        )
        self.paste(d, par_after=par3)
        process_pending_notifications()
        self.assertEqual(
            {
                "mail_from": mail_from,
                "msg": "Paragraph added by user 1 Test: "
                f"http://localhost/view/users/test-user-1/doc1#{par.get_id()} (changes: "
                f"http://localhost/diff/{d.id}/5/0/7/0 )\n"
                "\n"
                f'#- {{id="{par.get_id()}"}}\n'
                "test\n"
                "\n"
                f'#- {{id="{par2.get_id()}"}}\n'
                "test",
                "rcpt": "test2@example.com",
                "reply_to": "test1@example.com",
                "subject": "user 1 Test added a paragraph to the document document 2",
            },
            sent_mails_in_testing[-1],
        )


class AnswerNotifyTest(NotifyTestBase):
    def test_answer_notify(self):
        """Test that the notification is sent when a user sends an answer to a question."""

        self.login_test1()
        d = self.create_doc(
            initial_par="""
``` {#t plugin="csPlugin"}
type: text
```
"""
        )
        self.test_user_2.grant_access(d, AccessType.teacher)
        db.session.commit()
        self.login_test2()
        self.update_notify_settings(
            d,
            {
                "email_comment_add": False,
                "email_comment_modify": False,
                "email_doc_modify": False,
                "email_answer_add": True,
                "email_annotation_add": False,
                "email_annotation_modify": False,
            },
        )
        self.login_test1()
        self.post_answer("csplugin", f"{d.id}.t", user_input={"usercode": "test"})
        process_pending_notifications()
        self.assertEqual(1, len(sent_mails_in_testing))
        mail_from = "no-reply@tim.jyu.fi"
        self.assertEqual(
            {
                "mail_from": mail_from,
                "msg": f"Answer posted by user 1 Test to '{d.id}.t': "
                "http://localhost/answers/users/test-user-1/doc1?task=t&answerNumber=1&user=testuser1&valid_answers_only=false",
                "rcpt": "test2@example.com",
                "reply_to": "test1@example.com",
                "subject": "user 1 Test posted an answer to the document document 2",
            },
            sent_mails_in_testing[-1],
        )
        self.test_user_2.remove_access(d.id, "teacher")
        db.session.commit()
        self.post_answer("csplugin", f"{d.id}.t", user_input={"usercode": "test"})
        process_pending_notifications()
        self.assertEqual(
            1, len(sent_mails_in_testing), "No email should be sent to non-teachers"
        )


from timApp.velp.velps import create_new_velp
from timApp.velp.annotations import AnnotationVisibility
from timApp.velp.annotation_model import (
    AnnotationPosition,
    AnnotationCoordinate,
)


class VelpNotifyTest(NotifyTestBase):
    def test_annotation_notification(self):
        self.login_test1()
        initial_par = """
        #- 
        test text 1
        """
        d = self.create_doc(initial_par=initial_par)
        self.test_user_2.grant_access(d, AccessType.view)
        self.test_user_1.set_notify_settings(
            d,
            doc_modify=False,
            comment_modify=False,
            comment_add=False,
            answer_add=False,
            annotation_add=True,
            annotation_modify=False,
        )
        self.test_user_2.set_notify_settings(
            d,
            doc_modify=False,
            comment_modify=False,
            comment_add=False,
            answer_add=False,
            annotation_add=True,
            annotation_modify=False,
        )
        velp, velp_ver = create_new_velp(
            self.test_user_1.id,
            "test velp",
            0,
        )
        db.session.commit()
        par1 = d.document.get_paragraphs()[0]
        ann_pos = AnnotationPosition(
            start=AnnotationCoordinate(par_id=par1.id, offset=5),
            end=AnnotationCoordinate(par_id=par1.id, offset=8),
        )
        ann_resp = self.post_annotation(
            doc_id=d.id,
            velp_id=velp.id,
            coord=ann_pos,
            points=None,
            visible_to=AnnotationVisibility.everyone,
            style=1,
        )

        process_pending_notifications()
        self.assertEqual(1, len(sent_mails_in_testing))
        mail_from = "no-reply@tim.jyu.fi"
        self.assertEqual(
            {
                "mail_from": mail_from,
                "msg": f"Velp posted by user 1 Test: "
                f"http://localhost/view/{d.name}#{par1.id}\n\ntest velp",
                "rcpt": "test2@example.com",
                "reply_to": "test1@example.com",
                "subject": f"user 1 Test posted a velp to the document {d.title}",
            },
            sent_mails_in_testing[-1],
        )

        ann_id = ann_resp.get("id")
        self.delete_annotation(ann_id)
        process_pending_notifications()
        self.assertEqual(1, len(sent_mails_in_testing))
        mail_from = "no-reply@tim.jyu.fi"
        # FIXME: msg and subject strings (should be 'deleted' instead of 'posted')
        self.assertEqual(
            {
                "mail_from": mail_from,
                "msg": f"Velp posted by user 1 Test: "
                f"http://localhost/view/{d.name}#{par1.id}\n\ntest velp",
                "rcpt": "test2@example.com",
                "reply_to": "test1@example.com",
                "subject": f"user 1 Test posted a velp to the document {d.title}",
            },
            sent_mails_in_testing[0],
        )

    def test_annotation_comment(self):
        self.login_test1()
        initial_par = """
        #- 
        test text 1
        """
        d = self.create_doc(initial_par=initial_par)
        self.test_user_2.grant_access(d, AccessType.view)
        self.test_user_1.set_notify_settings(
            d,
            doc_modify=False,
            comment_modify=False,
            comment_add=False,
            answer_add=False,
            annotation_add=True,
            annotation_modify=False,
        )
        self.test_user_2.set_notify_settings(
            d,
            doc_modify=False,
            comment_modify=False,
            comment_add=False,
            answer_add=False,
            annotation_add=True,
            annotation_modify=False,
        )
        velp, velp_ver = create_new_velp(
            self.test_user_1.id,
            "test velp",
            0,
        )
        db.session.commit()
        par1 = d.document.get_paragraphs()[0]
        ann_pos = AnnotationPosition(
            start=AnnotationCoordinate(par_id=par1.id, offset=5),
            end=AnnotationCoordinate(par_id=par1.id, offset=8),
        )
        ann_resp = self.post_annotation(
            doc_id=d.id,
            velp_id=velp.id,
            coord=ann_pos,
            points=None,
            visible_to=AnnotationVisibility.everyone,
            style=1,
        )
        process_pending_notifications()

        comm_resp = self.post_annotation_comment(
            annotation_id=ann_resp.get("id"), comment="test comment"
        )
        process_pending_notifications()

        self.assertEqual(2, len(sent_mails_in_testing))
        mail_from = "no-reply@tim.jyu.fi"
        self.assertEqual(
            {
                "mail_from": mail_from,
                "msg": f"Velp modified by user 1 Test: "
                f"http://localhost/view/{d.name}#{par1.id}\n\ntest comment",
                "rcpt": "test2@example.com",
                "reply_to": "test1@example.com",
                "subject": f"user 1 Test modified a velp in the document {d.title}",
            },
            sent_mails_in_testing[-1],
        )
