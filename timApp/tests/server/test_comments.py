from lxml import html
from lxml.cssselect import CSSSelector
from lxml.html import HtmlElement
from sqlalchemy import select

from timApp.auth.accesstype import AccessType
from timApp.document.docparagraph import DocParagraph
from timApp.item.item import Item
from timApp.note.usernote import UserNote
from timApp.notification.notify import process_pending_notifications
from timApp.notification.send_email import sent_mails_in_testing
from timApp.tests.server.test_notify import NotifyTestBase
from timApp.tests.server.timroutetest import get_note_id_from_json
from timApp.timdb.sqa import db, run_sql
from timApp.user.user import User

comment_selector = CSSSelector("div.notes > div.note")


class CommentTest(NotifyTestBase):
    def test_comments(self):
        self.login_test1()
        d = self.create_doc(initial_par="test")
        doc = d.document
        par = doc.get_paragraphs()[0]
        comment1 = "This is a comment."
        comment2 = "This is a comment 2."
        comment3 = "This is a comment 3."
        comment4 = "This is a comment 4."
        expected_comments = [comment1, comment2, comment3]
        comment_private = "This is a private comment."

        self.post_comment_and_return_html(comment1, par)
        comments = self.post_comment_and_return_html(comment_private, par, public=False)
        self.assertEqual(2, len(comments))
        self.assertEqual(comment1, comments[0].find("p").text_content())
        self.assertEqual(comment_private, comments[1].find("p").text_content())
        User.get_anon().grant_access(d, AccessType.view)
        db.session.commit()
        self.login_anonymous()
        self.assertEqual(-1, self.current_user.id)
        comments = self.post_comment_and_return_html(comment2, par)
        self.assertEqual(2, len(comments))
        comments = self.post_comment_and_return_html(comment3, par)
        self.assertEqual(3, len(comments))
        for e, a in zip(expected_comments, comments):
            self.assertEqual(e, a.find("p").text_content())
        comments = self.post_comment_and_return_html(comment3, par, public=False)
        self.assertEqual(4, len(comments))

        self.login_anonymous()
        self.assertEqual(-2, self.current_user.id)
        comments = self.post_comment_and_return_html(comment4, par, public=False)
        self.assertEqual(4, len(comments))

        self.get_comments(d, expect_status=403)
        self.login_test1()
        cms = self.get_comments(d)
        self.assertEqual(
            ["This is a comment.", "This is a comment 2.", "This is a comment 3."],
            [c["content"] for c in cms["notes"]],
        )
        self.assertEqual({"all": 3, "everyone": 3}, cms["counts"])
        cms = self.get_comments(d, private=True)
        self.assertEqual(
            ["This is a comment.", "This is a comment 2.", "This is a comment 3."],
            [c["content"] for c in cms["notes"]],
        )
        self.assertEqual({"all": 6, "everyone": 3, "justme": 3}, cms["counts"])
        self.make_admin(self.current_user)
        cms = self.get_comments(d, private=True)
        self.assertEqual(
            [
                "This is a comment.",
                "This is a private comment.",
                "This is a comment 2.",
                "This is a comment 3.",
                "This is a comment 3.",
                "This is a comment 4.",
            ],
            [c["content"] for c in cms["notes"]],
        )
        self.assertEqual({"all": 6, "everyone": 3, "justme": 3}, cms["counts"])

    def post_comment_and_return_html(
        self, text: str, par: DocParagraph, public: bool = True
    ) -> list[HtmlElement]:
        resp = self.post_comment(par, public, text)
        h: HtmlElement = html.fromstring(resp["texts"])
        comments = comment_selector(h)
        return comments

    def test_invalid_comment_delete_request(self):
        self.json_post("/deleteNote", {}, expect_status=422)

    def test_invalid_comment_post_request(self):
        self.json_post("/postNote", {}, expect_status=422)

    def test_nonexistent_note(self):
        self.get(
            "/note/999",
            expect_status=404,
            expect_content="Comment not found. It may have been deleted.",
        )

    def get_comments(self, i: Item, private=False, deleted=False, expect_status=200):
        return self.get(
            f"/notes/{i.path}",
            query_string={"private": private, "deleted": deleted},
            expect_status=expect_status,
        )

    def test_nonexistent_note_post(self):
        self.login_test1()
        d = self.create_doc()

        self.post_comment(
            text="a",
            public=True,
            par=DocParagraph.create(d.document, par_id="x"),
            expect_status=404,
            expect_content=f"Document {d.id}: Paragraph not found: x",
        )

    def test_note_notify(self):
        self.login_test1()
        d = self.create_doc(initial_par="test")
        self.test_user_2.grant_access(d, AccessType.view)
        self.test_user_3.grant_access(d, AccessType.edit)
        db.session.commit()
        par = d.document.get_paragraphs()[0]
        self.post_comment(par, True, "hi")
        self.login_test2()
        self.update_notify_settings(
            d,
            {
                "email_comment_add": True,
                "email_comment_modify": True,
                "email_doc_modify": True,
                "email_answer_add": False,
            },
        )
        process_pending_notifications()
        self.login_test1()
        self.post_comment(par, True, "hello")
        self.post_comment(par, True, "hey")
        self.post_comment(par, False, "private")
        self.login_test3()
        self.post_comment(par, True, "good morning")
        self.post_par(
            d.document, "edited", par.get_id()
        )  # test also mixing comments and doc modifications
        self.assertEqual(1, len(sent_mails_in_testing))
        mail_from = "no-reply@tim.jyu.fi"
        self.assertEqual(
            {
                "mail_from": mail_from,
                "msg": "Comment posted: " f"{d.url}#{par.get_id()}\n" "\n" "hi",
                "rcpt": "test2@example.com",
                "reply_to": None,
                "subject": f"Someone posted a comment to the document {d.title}",
            },
            sent_mails_in_testing[-1],
        )
        process_pending_notifications()
        self.assertEqual(3, len(sent_mails_in_testing))
        self.assertEqual(
            {
                "mail_from": mail_from,
                "msg": "Comment posted: "
                f"{d.url}#{par.get_id()}\n"
                "\n"
                "hello\n"
                "\n"
                "Comment posted: "
                f"{d.url}#{par.get_id()}\n"
                "\n"
                "hey\n"
                "\n"
                "Comment posted: "
                f"{d.url}#{par.get_id()}\n"
                "\n"
                "good morning",
                "rcpt": "test2@example.com",
                "reply_to": None,
                "subject": f"2 people posted 3 comments to the document {d.title}",
            },
            sent_mails_in_testing[-2],
        )
        self.assertEqual(
            {
                "mail_from": mail_from,
                "msg": "Paragraph modified: " f"{d.url}#{par.get_id()}",
                "rcpt": "test2@example.com",
                "reply_to": None,
                "subject": f"Someone modified a paragraph in the document {d.title}",
            },
            sent_mails_in_testing[-1],
        )

    def test_only_private_comments(self):
        self.login_test1()
        d = self.create_doc(settings={"comments": "private"}, initial_par="test")
        par = d.document.get_paragraphs()[0]
        self.post_comment(
            par,
            public=True,
            text="test",
            expect_status=403,
            expect_content="Only private comments can be posted on this document.",
        )
        c = self.post_comment(par, public=False, text="test")
        self.edit_comment(
            get_note_id_from_json(c), par, True, "edited", expect_status=403
        )

    def test_comment_at_area_start(self):
        self.login_test1()
        d = self.create_doc(
            initial_par="""
#- {area=a collapse=true}

#-
hi

#- {area_end=a}
        """
        )
        par = d.document.get_paragraphs()[0]
        self.post_comment(par, public=True, text="test")
        r = self.get(d.url, as_tree=True)
        comments = r.cssselect(".notes > .note")
        self.assertTrue(comments)

    def test_comment_anonymized(self):
        self.login_test1()
        d = self.create_doc(initial_par="test")
        par = d.document.get_paragraphs()[0]
        self.post_comment(par, public=True, text="test")
        self.test_user_2.grant_access(d, AccessType.view)
        db.session.commit()
        self.login_test2()
        self.post_comment(par, public=True, text="test2")
        self.login_test1()
        r = self.get(d.url, as_tree=True, query_string={"hide_names": True})
        comments = r.cssselect(".notes > .note > .username")
        self.assertEqual("testuser1", comments[0].text_content())
        self.assertEqual("user3", comments[1].text_content())
        r = self.post_comment_and_return_html("test3", par)
        self.assertEqual("user3", r[1].cssselect(".username")[0].text_content())

    def test_comment_name_not_visible_when_referenced(self):
        self.login_test1()
        d = self.create_doc(initial_par="test")
        par = d.document.get_paragraphs()[0]
        self.post_comment(par, public=True, text="test")
        self.test_user_2.grant_access(d, AccessType.view)
        db.session.commit()
        self.login_test2()
        d2 = self.create_doc()
        d2.document.add_paragraph_obj(par.create_reference(d2.document))
        r = self.get(d2.url, as_tree=True)
        comments = r.cssselect(".notes > .note")
        self.assertEqual(1, len(comments))
        names = r.cssselect(".notes > .note > .username")
        self.assertEqual(0, len(names))

    def test_comment_in_translation(self):
        self.login_test1()
        d = self.create_doc(initial_par="test")
        tr = self.create_translation(d)
        par = tr.document.get_paragraphs()[0]
        r = self.post_comment(
            d.document.get_paragraphs()[0], public=True, text="test", orig=par
        )
        note_id = get_note_id_from_json(r)
        note = self.get(f"/note/{note_id}")
        self.assertEqual("test", note["text"])

    def test_comment_in_referenced_area(self):
        self.login_test1()
        d = self.create_doc(
            initial_par="""
#- {area=a}

#-
a
#-
b
#-
c

#- {area_end=a}
        """
        )
        d2 = self.create_doc(
            initial_par=f"""
#- {{rd={d.id} ra=a}}
        """
        )
        area_a_start = d.document.get_paragraphs()[0]
        area_a_middle = d.document.get_paragraphs()[2]
        orig_par = d2.document.get_paragraphs()[0]
        r = self.post_comment(area_a_middle, public=True, text="test", orig=orig_par)
        note: UserNote = (
            run_sql(select(UserNote).order_by(UserNote.id.desc()).limit(1))
            .scalars()
            .first()
        )
        self.assert_same_html(
            html.fromstring(r["texts"]),
            f"""
<div class="par"
     id="{orig_par.get_id()}"
     t="{orig_par.get_hash()}"
     attrs='{{&#34;ra&#34;: &#34;a&#34;, &#34;rd&#34;: &#34;{d.id}&#34;}}'
     ref-id="{area_a_middle.get_id()}"
     ref-t="{area_a_middle.get_hash()}"
     ref-attrs="{{}}"
     ref-doc-id="{d.id}"
>
    <div ng-non-bindable tabindex="0" class="parContent">
        <p>b</p>
    </div>
    <tim-par-ref
            docid="{d.id}"
            parid="{area_a_middle.get_id()}">
    </tim-par-ref>
    <div class="editline" tabindex="0" title="Click to edit this paragraph"></div>
    <div class="readline"
         title="Click to mark this paragraph as read"
    ></div>
    <div class="notes">
        <div class="note editable"
             ng-non-bindable
             note-id="{note.id}">
            <p>test</p>
            &mdash; <a class="username" title="Test user 1" href="mailto:test1@example.com"
        >testuser1</a> <span class="timestamp" title="{note.created}">just now</span></div>
    </div>
</div>
        """,
        )

        # Posting a comment to an area boundary paragraph should return just the paragraph element and no stray area
        # start/end tags.
        r = self.post_comment(area_a_start, public=True, text="test", orig=orig_par)
        note: UserNote = (
            run_sql(select(UserNote).order_by(UserNote.id.desc()).limit(1))
            .scalars()
            .first()
        )
        self.assert_same_html(
            html.fromstring(r["texts"]),
            f"""
<div class="par"
     id="{orig_par.get_id()}"
     t="{orig_par.get_hash()}"
     attrs='{{&#34;ra&#34;: &#34;a&#34;, &#34;rd&#34;: &#34;{d.id}&#34;}}'
     ref-id="{area_a_start.get_id()}"
     ref-t="{area_a_start.get_hash()}"
     ref-attrs="{{&#34;area&#34;: &#34;a&#34;}}"
     ref-doc-id="{d.id}"
>
    <div ng-non-bindable tabindex="0" class="parContent">

    </div>
    <tim-par-ref
            docid="{d.id}"
            parid="{area_a_start.get_id()}">
    </tim-par-ref>
    <div class="editline" tabindex="0" title="Click to edit this paragraph"></div>
    <div class="readline"
         title="Click to mark this paragraph as read"
    ></div>
    <div class="notes">
        <div class="note editable"
             ng-non-bindable
             note-id="{note.id}">
            <p>test</p>
            &mdash; <a class="username" title="Test user 1" href="mailto:test1@example.com"
        >testuser1</a> <span class="timestamp" title="{note.created}">just now</span></div>
    </div>
</div>
        """,
        )
