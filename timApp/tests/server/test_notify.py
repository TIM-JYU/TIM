from typing import Tuple

from timApp.document.docinfo import DocInfo
from timApp.document.randutils import random_id
from timApp.notification.notify import sent_mails_in_testing, process_pending_notifications
from timApp.tests.server.timroutetest import TimRouteTest
from timApp.timdb.sqa import db


class NotifyTestBase(TimRouteTest):
    def setUp(self):
        super().setUp()
        sent_mails_in_testing.clear()

    def update_notify_settings(self, d, new_settings):
        self.json_post(f'/notify/{d.id}', new_settings)

    def prepare_doc(self, add_new_par=True) -> Tuple[DocInfo, str, str]:
        self.login_test1()
        d = self.create_doc()
        title = d.title
        url = d.url
        self.test_user_2.grant_access(d, 'view')
        self.new_par(d.document, 'test')
        self.assertEqual([], sent_mails_in_testing)
        self.login_test2()

        # testuser 2 will get notification from both additions
        # because pending notifications have not yet been processed
        self.update_notify_settings(d, {'email_comment_add': True, 'email_comment_modify': False,
                                        'email_doc_modify': True})
        self.login_test1()
        if add_new_par:
            self.new_par(d.document, 'test')
        return d, title, url


class NotifyTest(NotifyTestBase):

    def test_notify(self):
        self.login_test1()
        d = self.create_doc()
        notify_url = f'/notify/{d.id}'
        n = self.get(notify_url)
        self.assertDictEqual({'email_comment_add': False,
                              'email_comment_modify': False,
                              'email_doc_modify': False}, n)
        new_settings = {'email_comment_add': True, 'email_comment_modify': False, 'email_doc_modify': True}
        self.update_notify_settings(d, new_settings)
        n = self.get(notify_url)
        self.assertDictEqual(new_settings, n)

    def test_notify_email(self):
        d, title, url = self.prepare_doc()
        process_pending_notifications()

        self.assertEqual(1, len(sent_mails_in_testing))
        par_id0 = d.document.get_paragraphs()[0].get_id()
        par_id = d.document.get_paragraphs()[1].get_id()
        mail_from = 'no-reply@tim.jyu.fi'
        self.assertEqual({
            'mail_from': mail_from,
            'msg': f'Paragraph added: {url}#{par_id0}\n\nParagraph added: {url}#{par_id}',
            'rcpt': self.test_user_2.email,
            'reply_to': None,
            'subject': f'Someone added 2 paragraphs to the document {title}'
        }, sent_mails_in_testing[-1])

        self.post_par(d.document, 'test2', par_id)
        process_pending_notifications()
        self.assertEqual({
            'mail_from': mail_from,
            'msg': f'Paragraph modified: {url}#{par_id}',
            'rcpt': self.test_user_2.email,
            'reply_to': None,
            'subject': f'Someone modified a paragraph in the document {title}'
        }, sent_mails_in_testing[-1])

        self.test_user_2.grant_access(d, 'edit')
        self.new_par(d.document, 'test')
        process_pending_notifications()
        pars = d.document.get_paragraphs()
        self.assertEqual({
            'mail_from': mail_from,
            'msg': f'Paragraph added: {url}#{pars[2].get_id()}'
            ' '
            f'(changes: http://localhost/diff/{d.id}/2/1/3/0 )\n'
            '\n'
            f'{pars[2].get_markdown()}',
            'rcpt': self.test_user_2.email,
            'reply_to': None,
            'subject': f'Someone added a paragraph to the document {title}'
        }, sent_mails_in_testing[-1])

        self.post_par(d.document, 'test3', par_id)
        process_pending_notifications()
        pars = d.document.get_paragraphs()
        par_md = pars[1].get_markdown()
        self.assertEqual({
            'mail_from': mail_from,
            'msg': f'Paragraph modified: {url}#{pars[1].get_id()}'
            ' '
            f'(changes: http://localhost/diff/{d.id}/3/0/3/1 )\n'
            '\n'
            f'{par_md}',
            'rcpt': self.test_user_2.email,
            'reply_to': None,
            'subject': f'Someone modified a paragraph in the document {title}'
        }, sent_mails_in_testing[-1])

        self.delete_par(d, par_id)
        process_pending_notifications()
        self.assertEqual({
            'mail_from': mail_from,
            'msg': f'Paragraph deleted: {url}'
            ' '
            f'(changes: http://localhost/diff/{d.id}/3/1/4/0 )\n'
            '\n'
            f'{par_md}',
            'rcpt': self.test_user_2.email,
            'reply_to': None,
            'subject': f'Someone deleted a paragraph from the document {title}'
        }, sent_mails_in_testing[-1])

        self.test_user_2.grant_access(d, 'teacher')
        self.new_par(d.document, 'test')
        process_pending_notifications()
        pars = d.document.get_paragraphs()
        self.assertEqual({
            'mail_from': mail_from,
            'msg': f'Paragraph added by Test user 1: {url}#{pars[-1].get_id()}'
            ' '
            f'(changes: http://localhost/diff/{d.id}/4/0/5/0 )\n'
            '\n'
            f'{pars[-1].get_markdown()}',
            'rcpt': self.test_user_2.email,
            'reply_to': self.test_user_1.email,
            'subject': f'Test user 1 added a paragraph to the document {title}'
        }, sent_mails_in_testing[-1])

    def test_revoke_view_no_email(self):
        d, title, url = self.prepare_doc()
        process_pending_notifications()
        self.assertEqual(1, len(sent_mails_in_testing))
        self.test_user_2.remove_access(d.id, 'view')
        db.session.commit()
        self.new_par(d.document, 'test')
        process_pending_notifications()
        self.assertEqual(1, len(sent_mails_in_testing))

    def test_answer_link_email(self):
        d, title, url = self.prepare_doc()

        process_pending_notifications()
        [plug] = d.document.add_text("""
#- {plugin=csPlugin #t}
stem: test
        """)
        self.test_user_2.grant_access(d, 'teacher')
        self.post_comment(plug, public=True, text='Hello')
        process_pending_notifications()
        self.assertEqual(
            {'mail_from': 'no-reply@tim.jyu.fi',
             'msg': 'Comment posted by Test user 1: '
             f'http://localhost/answers/{d.path}?task=t&user=testuser1\n'
                    '\n'
                    'Hello',
             'rcpt': 'test2@example.com',
             'reply_to': 'test1@example.com',
             'subject': f'Test user 1 posted a comment to the document {title}'},
            sent_mails_in_testing[-1])


class NotifyFolderTest(NotifyTestBase):
    def test_folder_email(self):
        self.login_test1()
        t1_f = self.current_user.get_personal_folder()
        self.test_user_2.grant_access(t1_f, 'view')
        self.login_test2()
        self.update_notify_settings(t1_f, {'email_comment_add': True, 'email_comment_modify': False,
                                           'email_doc_modify': True})
        r = self.get('/notify/all')
        self.assertEqual(1, len(r))
        self.assertTrue(r[0]['item']['isFolder'])
        self.login_test1()
        d = self.create_doc()
        self.new_par(d.document, 'test')
        self.assertEqual(0, len(sent_mails_in_testing))
        self.test_user_2.grant_access(d, 'view')
        self.new_par(d.document, 'test')
        process_pending_notifications()
        self.assertEqual(1, len(sent_mails_in_testing))

        self.login_test2()
        self.update_notify_settings(d, {'email_comment_add': True, 'email_comment_modify': False,
                                        'email_doc_modify': True})

        self.login_test1()
        self.new_par(d.document, 'test')
        process_pending_notifications()
        self.assertEqual(2, len(sent_mails_in_testing))
        prefs = self.test_user_2.get_prefs()
        prefs.email_exclude = d.location
        self.test_user_2.set_prefs(prefs)
        db.session.commit()
        self.new_par(d.document, 'test')
        process_pending_notifications()
        self.assertEqual(2, len(sent_mails_in_testing))

        prefs.email_exclude = '**invalid regex**'
        self.test_user_2.set_prefs(prefs)
        db.session.commit()
        self.new_par(d.document, 'test')
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
        par3.set_markdown('hello')
        par3.save(add=True)
        process_pending_notifications()
        # d.document.clear_mem_cache()
        self.test_user_2.grant_access(d, 'teacher')
        self.test_user_2.grant_access(d, 'edit')
        self.cut(d, par_start=par, par_end=par2)
        # print(d.document.export_markdown())
        process_pending_notifications()
        mail_from = 'no-reply@tim.jyu.fi'
        self.assertEqual({
            'mail_from': mail_from,
            'msg': 'Paragraph deleted by Test user 1: '
                   f'{d.url} (changes: '
                   f'http://localhost/diff/{d.id}/3/0/5/0 )\n'
                   '\n'
            f'#- {{id="{par.get_id()}"}}\n'
                   'test\n'
                   '\n'
            f'#- {{id="{par2.get_id()}"}}\n'
                   'test',
            'rcpt': 'test2@example.com',
            'reply_to': 'test1@example.com',
            'subject': 'Test user 1 deleted a paragraph from the document document 2'},
            sent_mails_in_testing[-1])
        self.paste(d, par_after=par3)
        process_pending_notifications()
        self.assertEqual({
            'mail_from': mail_from,
            'msg': 'Paragraph added by Test user 1: '
            f'http://localhost/view/users/test-user-1/doc1#{par.get_id()} (changes: '
                   f'http://localhost/diff/{d.id}/5/0/7/0 )\n'
                   '\n'
            f'#- {{id="{par.get_id()}"}}\n'
                   'test\n'
                   '\n'
            f'#- {{id="{par2.get_id()}"}}\n'
                   'test',
            'rcpt': 'test2@example.com',
            'reply_to': 'test1@example.com',
            'subject': 'Test user 1 added a paragraph to the document document 2'},
            sent_mails_in_testing[-1])
