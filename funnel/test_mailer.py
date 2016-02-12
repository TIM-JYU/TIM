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
        self.assertFalse(self.mailer.has_messages())

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

    def testEnqueue(self):
        # First enqueue
        f1_data = ['', 'sender', 'recipient', 'message']
        f1_file = self.mailer.enqueue(f1_data[1], f1_data[2], f1_data[3])
        self.assertTrue(self.mailer.has_messages())

        files = os.listdir(self.maildir)
        self.assertEqual(len(files), 3)
        self.assertTrue('first' in files)
        self.assertTrue('last' in files)
        self.assertTrue(f1_file in files)
        self.assertEqual(os.readlink(self.mailer.get_first_filename()), f1_file)
        self.assertEqual(os.readlink(self.mailer.get_last_filename()), f1_file)

        with open(os.path.join(self.maildir, f1_file)) as f1:
            f1_readdata = f1.read().split('\n')

        self.assertEqual(len(f1_readdata), 4, 'Read lines: ' + str(f1_readdata))
        for i in range(4):
            self.assertEqual(f1_readdata[i],
                             f1_data[i],
                             'Line {} mismatches! Set "{}", read "{}"'.format(i, f1_data[i], f1_readdata[i]))

        # Second enqueue
        f2_data = ['', 'daemon@example.org', 'human@example2.com', 'first line\nsecond line']
        f2_file = self.mailer.enqueue(f2_data[1], f2_data[2], f2_data[3])
        self.assertTrue(self.mailer.has_messages())

        files = os.listdir(self.maildir)
        self.assertEqual(len(files), 4)
        self.assertTrue('first' in files)
        self.assertTrue('last' in files)
        self.assertTrue(f1_file in files)
        self.assertTrue(f2_file in files)
        self.assertEqual(os.readlink(self.mailer.get_first_filename()), f1_file)
        self.assertEqual(os.readlink(self.mailer.get_last_filename()), f2_file)

        with open(os.path.join(self.maildir, f2_file)) as f2:
            f2_readdata = f2.read().split('\n')

        self.assertEqual(len(f2_readdata), 5, 'Read lines: ' + str(f2_readdata))
        f2_readdata = f2_readdata[:3] + ['\n'.join(f2_readdata[3:])]
        for i in range(4):
            self.assertEqual(f2_readdata[i],
                             f2_data[i],
                             'Line {} mismatches! Set "{}", read "{}"'.format(i, f2_data[i], f2_readdata[i]))

        # Third / nth enqueue
        f3_file = self.mailer.enqueue(f1_data[1], f1_data[2], f1_data[3])
        self.assertTrue(self.mailer.has_messages())

        files = os.listdir(self.maildir)
        self.assertEqual(len(files), 5)
        self.assertTrue('first' in files)
        self.assertTrue('last' in files)
        self.assertTrue(f1_file in files)
        self.assertTrue(f2_file in files)
        self.assertTrue(f3_file in files)
        self.assertEqual(os.readlink(self.mailer.get_first_filename()), f1_file)
        self.assertEqual(os.readlink(self.mailer.get_last_filename()), f3_file)

        with open(os.path.join(self.maildir, f3_file)) as f3:
            f3_readdata = f3.read().split('\n')

        self.assertEqual(len(f3_readdata), 4, 'Read lines: ' + str(f3_readdata))
        for i in range(4):
            self.assertEqual(f3_readdata[i],
                             f1_data[i],
                             'Line {} mismatches! Set "{}", read "{}"'.format(i, f1_data[i], f3_readdata[i]))

    #def testDequeue(self):
    #    self.fail()

    #def testUpdate(self):
    #    self.fail()

