from routes.notify import sent_mails_in_testing
from tests.server.timroutetest import TimRouteTest


class NotifyTest(TimRouteTest):
    def test_notify(self):
        self.login_test1()
        d = self.create_doc()
        notify_url = '/notify/{}'.format(d.id)
        n = self.get(notify_url)
        self.assertDictEqual({'email_comment_add': False,
                              'email_comment_modify': False,
                              'email_doc_modify': False}, n)
        new_settings = {'email_comment_add': True, 'email_comment_modify': False, 'email_doc_modify': True}
        self.update_notify_settings(d, new_settings)
        n = self.get(notify_url)
        self.assertDictEqual(new_settings, n)

    def update_notify_settings(self, d, new_settings):
        self.json_post('/notify/{}'.format(d.id), new_settings)

    def test_notify_email(self):
        self.login_test1()
        d = self.create_doc()
        title = d.title
        url = d.url
        self.test_user_2.grant_access(d.id, 'view')
        self.new_par(d.document, 'test')
        self.assertEqual([], sent_mails_in_testing)
        self.login_test2()
        self.update_notify_settings(d, {'email_comment_add': True, 'email_comment_modify': False,
                                        'email_doc_modify': True})
        self.login_test1()
        self.new_par(d.document, 'test')

        self.assertEqual(1, len(sent_mails_in_testing))
        par_id = d.document.get_paragraphs()[1].get_id()
        mail_from = 'tim@jyu.fi'
        self.assertEqual({'group_id': 'docmodify_{}'.format(d.id),
                          'group_subject': 'The document {} has been modified'.format(title),
                          'mail_from': mail_from,
                          'msg': 'Link to the paragraph: {}#{}'.format(url, par_id),
                          'rcpt': self.test_user_2.email,
                          'reply_to': None,
                          'subject': 'Someone edited the document {}'.format(title)}, sent_mails_in_testing[-1])

        self.post_par(d.document, 'test2', par_id)
        self.assertEqual({'group_id': 'docmodify_{}'.format(d.id),
                          'group_subject': 'The document {} has been modified'.format(title),
                          'mail_from': mail_from,
                          'msg': 'Link to the paragraph: {}#{}'.format(url, par_id),
                          'rcpt': self.test_user_2.email,
                          'reply_to': None,
                          'subject': 'Someone edited the document {}'.format(title)}, sent_mails_in_testing[-1])

        self.test_user_2.grant_access(d.id, 'edit')
        self.new_par(d.document, 'test')
        pars = d.document.get_paragraphs()
        self.assertEqual({'group_id': 'docmodify_{}'.format(d.id),
                          'group_subject': 'The document {} has been modified'.format(title),
                          'mail_from': mail_from,
                          'msg': ('Link to the paragraph: {}#{}\n'
                                  '\n'
                                  'Link to changes: http://localhost/diff/5/2/1/3/0\n'
                                  '\n'
                                  'Paragraph was added:\n'
                                  '\n'
                                  '{}').format(url, pars[2].get_id(), pars[2].get_markdown()),
                          'rcpt': self.test_user_2.email,
                          'reply_to': None,
                          'subject': 'Someone edited the document {}'.format(title)}, sent_mails_in_testing[-1])

        self.post_par(d.document, 'test3', par_id)
        pars = d.document.get_paragraphs()
        par_md = pars[1].get_markdown()
        self.assertEqual({'group_id': 'docmodify_{}'.format(d.id),
                          'group_subject': 'The document {} has been modified'.format(title),
                          'mail_from': mail_from,
                          'msg': ('Link to the paragraph: {}#{}\n'
                                  '\n'
                                  'Link to changes: http://localhost/diff/5/3/0/3/1\n'
                                  '\n'
                                  'Paragraph was edited:\n'
                                  '\n'
                                  '{}').format(url, pars[1].get_id(), par_md),
                          'rcpt': self.test_user_2.email,
                          'reply_to': None,
                          'subject': 'Someone edited the document {}'.format(title)}, sent_mails_in_testing[-1])

        self.delete_par(d.document, par_id)
        self.assertEqual({'group_id': 'docmodify_{}'.format(d.id),
                          'group_subject': 'The document {} has been modified'.format(title),
                          'mail_from': mail_from,
                          'msg': ('Link to the document: {}\n'
                                  '\n'
                                  'Link to changes: http://localhost/diff/5/3/1/4/0\n'
                                  '\n'
                                  'Paragraph was deleted:\n'
                                  '\n'
                                  '{}').format(url, par_md),
                          'rcpt': self.test_user_2.email,
                          'reply_to': None,
                          'subject': 'Someone edited the document {}'.format(title)}, sent_mails_in_testing[-1])

        self.test_user_2.grant_access(d.id, 'teacher')
        self.new_par(d.document, 'test')
        pars = d.document.get_paragraphs()
        self.assertEqual({'group_id': 'docmodify_{}'.format(d.id),
                          'group_subject': 'The document {} has been modified'.format(title),
                          'mail_from': mail_from,
                          'msg': ('Link to the paragraph: {}#{}\n'
                                  '\n'
                                  'Link to changes: http://localhost/diff/5/4/0/5/0\n'
                                  '\n'
                                  'Paragraph was added:\n'
                                  '\n'
                                  '{}').format(url, pars[-1].get_id(), pars[-1].get_markdown()),
                          'rcpt': self.test_user_2.email,
                          'reply_to': self.test_user_1.email,
                          'subject': 'user Test edited the document {}'.format(title)}, sent_mails_in_testing[-1])
