from typing import List

from lxml import html
from lxml.cssselect import CSSSelector
from lxml.html import HtmlElement

from timApp.document.docparagraph import DocParagraph
from timApp.notification.notify import process_pending_notifications, sent_mails_in_testing
from timApp.tests.server.test_notify import NotifyTestBase
from timApp.tests.server.timroutetest import get_note_id_from_json
from timApp.user.usergroup import UserGroup
from timApp.user.userutils import grant_view_access

comment_selector = CSSSelector('div.notes > div.note')


class CommentTest(NotifyTestBase):

    def test_comments(self):
        self.login_test1()
        d = self.create_doc(initial_par='test')
        doc = d.document
        par = doc.get_paragraphs()[0]
        comment1 = 'This is a comment.'
        comment2 = 'This is a comment 2.'
        comment3 = 'This is a comment 3.'
        comment4 = 'This is a comment 4.'
        expected_comments = [comment1, comment2, comment3]
        comment_private = 'This is a private comment.'

        self.post_comment_and_return_html(comment1, par)
        comments = self.post_comment_and_return_html(comment_private, par, public=False)
        self.assertEqual(2, len(comments))
        self.assertEqual(comment1, comments[0].find('p').text_content())
        self.assertEqual(comment_private, comments[1].find('p').text_content())
        grant_view_access(UserGroup.get_anonymous_group(), d)

        self.login_anonymous()
        self.assertEqual(-1, self.current_user.id)
        comments = self.post_comment_and_return_html(comment2, par)
        self.assertEqual(2, len(comments))
        comments = self.post_comment_and_return_html(comment3, par)
        self.assertEqual(3, len(comments))
        for e, a in zip(expected_comments, comments):
            self.assertEqual(e, a.find('p').text_content())
        comments = self.post_comment_and_return_html(comment3, par, public=False)
        self.assertEqual(4, len(comments))

        self.login_anonymous()
        self.assertEqual(-2, self.current_user.id)
        comments = self.post_comment_and_return_html(comment4, par, public=False)
        self.assertEqual(4, len(comments))

    def post_comment_and_return_html(self, text: str, par: DocParagraph, public: bool = True) -> List[HtmlElement]:
        resp = self.post_comment(par, public, text)
        h: HtmlElement = html.fromstring(resp['texts'])
        comments = comment_selector(h)
        return comments

    def test_invalid_comment_delete_request(self):
        self.json_post('/deleteNote', {}, expect_status=400)

    def test_invalid_comment_post_request(self):
        self.json_post('/postNote', {}, expect_status=400)

    def test_nonexistent_note(self):
        self.get('/note/999',
                 expect_status=404,
                 expect_content='Comment not found. It may have been deleted.',
                 json_key='error')

    def test_nonexistent_note_post(self):
        self.login_test1()
        d = self.create_doc()

        self.post_comment(text='a',
                          public=True,
                          par=DocParagraph.create(d.document, par_id='x'),
                          expect_status=404,
                          expect_content=f'Document {d.id}: Paragraph not found: x',
                          json_key='error')

    def test_note_notify(self):
        self.login_test1()
        d = self.create_doc(initial_par='test')
        self.test_user_2.grant_access(d, 'view')
        self.test_user_3.grant_access(d, 'edit')
        par = d.document.get_paragraphs()[0]
        self.post_comment(par, True, 'hi')
        self.login_test2()
        self.update_notify_settings(
            d,
            {'email_comment_add': True,
             'email_comment_modify': True,
             'email_doc_modify': True}
        )
        process_pending_notifications()
        self.login_test1()
        self.post_comment(par, True, 'hello')
        self.post_comment(par, True, 'hey')
        self.post_comment(par, False, 'private')
        self.login_test3()
        self.post_comment(par, True, 'good morning')
        self.post_par(d.document, 'edited', par.get_id())  # test also mixing comments and doc modifications
        self.assertEqual(1, len(sent_mails_in_testing))
        self.assertEqual(
            {'mail_from': 'tim@jyu.fi',
             'msg': 'Comment posted: '
                    f'{d.url}#{par.get_id()}\n'
                    '\n'
                    'hi',
             'rcpt': 'test2@example.com',
             'reply_to': None,
             'subject': f'Someone posted a comment to the document {d.title}'},
            sent_mails_in_testing[-1])
        process_pending_notifications()
        self.assertEqual(3, len(sent_mails_in_testing))
        self.assertEqual(
            {'mail_from': 'tim@jyu.fi',
             'msg': 'Comment posted: '
                    f'{d.url}#{par.get_id()}\n'
                    '\n'
                    'hello\n'
                    '\n'
                    'Comment posted: '
                    f'{d.url}#{par.get_id()}\n'
                    '\n'
                    'hey\n'
                    '\n'
                    'Comment posted: '
                    f'{d.url}#{par.get_id()}\n'
                    '\n'
                    'good morning',
             'rcpt': 'test2@example.com',
             'reply_to': None,
             'subject': f'2 people posted 3 comments to the document {d.title}'},
            sent_mails_in_testing[-2])
        self.assertEqual(
            {'mail_from': 'tim@jyu.fi',
             'msg': 'Paragraph modified: '
                    f'{d.url}#{par.get_id()}',
             'rcpt': 'test2@example.com',
             'reply_to': None,
             'subject': f'Someone modified a paragraph in the document {d.title}'},
            sent_mails_in_testing[-1])

    def test_only_private_comments(self):
        self.login_test1()
        d = self.create_doc(settings={'comments': 'private'}, initial_par='test')
        par = d.document.get_paragraphs()[0]
        self.post_comment(par, public=True, text='test',
                          expect_status=403,
                          expect_content='Only private comments can be posted on this document.',
                          json_key='error')
        c = self.post_comment(par, public=False, text='test')
        self.json_post('/editNote', {'id': get_note_id_from_json(c),
                                     'text': 'edited',
                                     'access': 'everyone',
                                     'docId': d.id,
                                     'par': par.get_id()},
                       expect_status=403)

    def test_comment_at_area_start(self):
        self.login_test1()
        d = self.create_doc(initial_par="""
#- {area=a collapse=true}

#-
hi

#- {area_end=a}
        """)
        par = d.document.get_paragraphs()[0]
        self.post_comment(par, public=True, text='test')
        r = self.get(d.url, as_tree=True)
        comments = r.cssselect('.notes > .note')
        self.assertTrue(comments)
