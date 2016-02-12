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
        filenames = []
        for i in range(10):
            random_abs, random_rel = self.mailer.get_random_filenames()
            self.assertEqual(random_abs, os.path.join(self.maildir, random_rel))
            self.assertFalse(os.path.isfile(random_abs))
            with open(random_abs, 'w') as f:
                pass
            self.assertTrue(os.path.isfile(random_abs))

    def testSetNext(self):
        f1_abs, f1_rel = self.mailer.get_random_filenames()
        f2_abs, f2_rel = self.mailer.get_random_filenames()

        f1_data = ['next_file', 'sender', 'recipient', 'message']
        with open(f1_abs, 'w') as f1:
            f1.write('\n'.join(f1_data))

        self.mailer.set_next(f1_abs, f2_rel)

        with open(f1_abs, 'r') as f1:
            f1_newdata = f1.read().split('\n')

        self.assertEqual(len(f1_newdata), 4)
        self.assertEqual(f1_newdata[0], f2_rel)
        self.assertEqual(f1_newdata[1], f1_data[1])
        self.assertEqual(f1_newdata[2], f1_data[2])
        self.assertEqual(f1_newdata[3], f1_data[3])

    #def testEnqueue(self):
    #    self.fail()

    #def testDequeue(self):
    #    self.fail()

    #def testUpdate(self):
    #    self.fail()

