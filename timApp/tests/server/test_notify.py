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
        self.assertEqual({'group_id': 'docmodify_{}'.format(d.id),
                          'group_subject': 'The document {} has been modified'.format(title),
                          'mail_from': 'no-reply@tim.jyu.fi',
                          'msg': 'Link to the paragraph: {}#{}'.format(url, d.document.get_paragraphs()[1].get_id()),
                          'rcpt': self.test_user_2.email,
                          'reply_to': None,
                          'subject': 'Someone edited the document {}'.format(title)}, sent_mails_in_testing[0])

        self.test_user_2.grant_access(d.id, 'edit')
        self.new_par(d.document, 'test')
        pars = d.document.get_paragraphs()
        self.maxDiff = None
        self.assertEqual({'group_id': 'docmodify_{}'.format(d.id),
                          'group_subject': 'The document {} has been modified'.format(title),
                          'mail_from': 'no-reply@tim.jyu.fi',
                          'msg': ('Link to the paragraph: {}#{}\n'
                                  '\n'
                                  'Link to changes: http://localhost/diff/5/2/0/3/0\n'
                                  '\n'
                                  'Paragraph was added:\n'
                                  '\n'
                                  '{}').format(url, pars[2].get_id(), pars[2].get_markdown()),
                          'rcpt': 'test2@example.com',
                          'reply_to': None,
                          'subject': 'Someone edited the document {}'.format(title)}, sent_mails_in_testing[1])

        self.test_user_2.grant_access(d.id, 'teacher')
        self.new_par(d.document, 'test')
        pars = d.document.get_paragraphs()
        self.assertEqual({'group_id': 'docmodify_{}'.format(d.id),
                          'group_subject': 'The document {} has been modified'.format(title),
                          'mail_from': 'test1@example.com',
                          'msg': ('Link to the paragraph: {}#{}\n'
                                  '\n'
                                  'Link to changes: http://localhost/diff/5/3/0/4/0\n'
                                  '\n'
                                  'Paragraph was added:\n'
                                  '\n'
                                  '{}').format(url, pars[3].get_id(), pars[3].get_markdown()),
                          'rcpt': 'test2@example.com',
                          'reply_to': None,
                          'subject': 'user Test edited the document {}'.format(title)}, sent_mails_in_testing[2])
