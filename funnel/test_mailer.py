import os
import unittest
from shutil import rmtree

from mailer import Mailer


class MailerTest(unittest.TestCase):
    def setUp(self):
        self.maildir = os.path.join(os.getcwd(), 'test_mail')
        if os.path.exists(self.maildir):
            rmtree(self.maildir)

        self.mailer = Mailer(mail_dir=self.maildir)

    def tearDown(self):
        if os.path.exists(self.maildir):
            rmtree(self.maildir)

    def testInit(self):
        self.assertTrue(os.path.exists(self.maildir))
        self.assertEqual(len(os.listdir(self.maildir)), 0)
        self.assertEqual(self.mailer.get_first_filename(), os.path.join(self.maildir, 'first'))
        self.assertEqual(self.mailer.get_last_filename(), os.path.join(self.maildir, 'last'))

    def testRandomFilename(self):
        self.fail()

    def testSetNext(self):
        self.fail()

    def testEnqueue(self):
        self.fail()

    def testDequeue(self):
        self.fail()

    def testUpdate(self):
        self.fail()

