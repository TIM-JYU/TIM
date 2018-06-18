from timApp.notification.notify import sent_mails_in_testing
from timApp.tests.server.timroutetest import TimRouteTest
from timApp.document.docentry import DocEntry
from timApp.timdb.sqa import db


class NotifyTest(TimRouteTest):
    def setUp(self):
        super().setUp()
        sent_mails_in_testing.clear()

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

    def update_notify_settings(self, d, new_settings):
        self.json_post(f'/notify/{d.id}', new_settings)

    def test_notify_email(self):
        d, title, url = self.prepare_doc()

        self.assertEqual(1, len(sent_mails_in_testing))
        par_id = d.document.get_paragraphs()[1].get_id()
        mail_from = 'tim@jyu.fi'
        self.assertEqual({'group_id': f'docmodify_{d.id}',
                          'group_subject': f'The document {title} has been modified',
                          'mail_from': mail_from,
                          'msg': f'Link to the paragraph: {url}#{par_id}',
                          'rcpt': self.test_user_2.email,
                          'reply_to': None,
                          'subject': f'Someone edited the document {title}'}, sent_mails_in_testing[-1])

        self.post_par(d.document, 'test2', par_id)
        self.assertEqual({'group_id': f'docmodify_{d.id}',
                          'group_subject': f'The document {title} has been modified',
                          'mail_from': mail_from,
                          'msg': f'Link to the paragraph: {url}#{par_id}',
                          'rcpt': self.test_user_2.email,
                          'reply_to': None,
                          'subject': f'Someone edited the document {title}'}, sent_mails_in_testing[-1])

        self.test_user_2.grant_access(d.id, 'edit')
        self.new_par(d.document, 'test')
        pars = d.document.get_paragraphs()
        self.assertEqual({'group_id': f'docmodify_{d.id}',
                          'group_subject': f'The document {title} has been modified',
                          'mail_from': mail_from,
                          'msg': f'Link to the paragraph: {url}#{pars[2].get_id()}\n'
                                 '\n'
                                 f'Link to changes: http://localhost/diff/{d.id}/2/1/3/0\n'
                                 '\n'
                                 'Paragraph was added:\n'
                                 '\n'
                                 f'{pars[2].get_markdown()}',
                          'rcpt': self.test_user_2.email,
                          'reply_to': None,
                          'subject': f'Someone edited the document {title}'}, sent_mails_in_testing[-1])

        self.post_par(d.document, 'test3', par_id)
        pars = d.document.get_paragraphs()
        par_md = pars[1].get_markdown()
        self.assertEqual({'group_id': f'docmodify_{d.id}',
                          'group_subject': f'The document {title} has been modified',
                          'mail_from': mail_from,
                          'msg': f'Link to the paragraph: {url}#{pars[1].get_id()}\n'
                                 '\n'
                                 f'Link to changes: http://localhost/diff/{d.id}/3/0/3/1\n'
                                 '\n'
                                 'Paragraph was edited:\n'
                                 '\n'
                                 f'{par_md}',
                          'rcpt': self.test_user_2.email,
                          'reply_to': None,
                          'subject': f'Someone edited the document {title}'}, sent_mails_in_testing[-1])

        self.delete_par(d, par_id)
        self.assertEqual({'group_id': f'docmodify_{d.id}',
                          'group_subject': f'The document {title} has been modified',
                          'mail_from': mail_from,
                          'msg': f'Link to the document: {url}\n'
                                 '\n'
                                 f'Link to changes: http://localhost/diff/{d.id}/3/1/4/0\n'
                                 '\n'
                                 'Paragraph was deleted:\n'
                                 '\n'
                                 f'{par_md}',
                          'rcpt': self.test_user_2.email,
                          'reply_to': None,
                          'subject': f'Someone edited the document {title}'}, sent_mails_in_testing[-1])

        self.test_user_2.grant_access(d.id, 'teacher')
        self.new_par(d.document, 'test')
        pars = d.document.get_paragraphs()
        self.assertEqual({'group_id': f'docmodify_{d.id}',
                          'group_subject': f'The document {title} has been modified',
                          'mail_from': mail_from,
                          'msg': f'Link to the paragraph: {url}#{pars[-1].get_id()}\n'
                                 '\n'
                                 f'Link to changes: http://localhost/diff/{d.id}/4/0/5/0\n'
                                 '\n'
                                 'Paragraph was added:\n'
                                 '\n'
                                 f'{pars[-1].get_markdown()}',
                          'rcpt': self.test_user_2.email,
                          'reply_to': self.test_user_1.email,
                          'subject': f'Test user 1 edited the document {title}'}, sent_mails_in_testing[-1])

    def test_revoke_view_no_email(self):
        d, title, url = self.prepare_doc()
        self.assertEqual(1, len(sent_mails_in_testing))
        self.test_user_2.remove_access(d.id, 'view')
        db.session.commit()
        self.new_par(d.document, 'test')
        self.assertEqual(1, len(sent_mails_in_testing))

    def prepare_doc(self):
        self.login_test1()
        d = self.create_doc()
        doc_id = d.id
        title = d.title
        url = d.url
        self.test_user_2.grant_access(d.id, 'view')
        self.new_par(d.document, 'test')
        self.assertEqual([], sent_mails_in_testing)
        self.login_test2()
        d = DocEntry.find_by_id(doc_id)  # Avoids DetachedInstanceError
        self.update_notify_settings(d, {'email_comment_add': True, 'email_comment_modify': False,
                                        'email_doc_modify': True})
        self.login_test1()
        self.new_par(d.document, 'test')
        return d, title, url
