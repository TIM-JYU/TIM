import os
import time
import unittest
from shutil import rmtree

from mailer import Mailer


class MailerTest(unittest.TestCase):
    def setUp(self):
        self.maildir = os.path.join(os.getcwd(), 'test_mail')
        if os.path.exists(self.maildir):
            rmtree(self.maildir)

        self.mailer = Mailer(mail_dir=self.maildir, dry_run=True)

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
        f1_data = ['', 'sender', 'recipient', 'subject', 'message']
        f1_file = self.mailer.enqueue(f1_data[1], f1_data[2], f1_data[3], f1_data[4])
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

        self.assertEqual(len(f1_readdata), 5, 'Read lines: ' + str(f1_readdata))
        for i in range(5):
            self.assertEqual(f1_readdata[i],
                             f1_data[i],
                             'Line {} mismatches! Set "{}", read "{}"'.format(i, f1_data[i], f1_readdata[i]))

        # Second enqueue
        f2_data = ['', 'daemon@example.org', 'human@example2.com', 'Notification', 'first line\nsecond line']
        f2_file = self.mailer.enqueue(f2_data[1], f2_data[2], f2_data[3], f2_data[4])
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

        self.assertEqual(len(f2_readdata), 6, 'Read lines: ' + str(f2_readdata))
        f2_readdata = f2_readdata[:4] + ['\n'.join(f2_readdata[4:])]
        for i in range(5):
            self.assertEqual(f2_readdata[i],
                             f2_data[i],
                             'Line {} mismatches! Set "{}", read "{}"'.format(i, f2_data[i], f2_readdata[i]))

        # Third / nth enqueue
        f3_file = self.mailer.enqueue(f1_data[1], f1_data[2], f1_data[3], f1_data[4])
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

        self.assertEqual(len(f3_readdata), 5, 'Read lines: ' + str(f3_readdata))
        for i in range(5):
            self.assertEqual(f3_readdata[i],
                             f1_data[i],
                             'Line {} mismatches! Set "{}", read "{}"'.format(i, f1_data[i], f3_readdata[i]))

    def testDequeue(self):
        # Empty dequeue
        self.assertEqual(self.mailer.dequeue(), None)
        files = os.listdir(self.maildir)
        self.assertEqual(len(files), 0)

        # Single element

        f1_data = ['', 'sender', 'recipient', 'subject', 'message']
        f1_abs, f1_rel = self.mailer.get_random_filenames()
        with open(f1_abs, 'w') as f1:
            f1.write('\n'.join(f1_data))
        os.symlink(f1_rel, self.mailer.get_first_filename())
        os.symlink(f1_rel, self.mailer.get_last_filename())

        readdata = self.mailer.dequeue()

        files = os.listdir(self.maildir)
        self.assertEqual(len(files), 0)
        self.assertEqual(len(readdata), 4, 'Read message: ' + str(readdata))
        self.assertEqual(readdata['From'], f1_data[1])
        self.assertEqual(readdata['To'], f1_data[2])
        self.assertEqual(readdata['Subject'], f1_data[3])
        self.assertEqual(readdata['Msg'], f1_data[4])

        # Multiple elements
        f_abs, f_rel = zip(*[self.mailer.get_random_filenames() for _ in range(3)])
        f_data = [[f_rel[1], f1_data[1], f1_data[2], f1_data[3], f1_data[4]],
                  [f_rel[2], 'daemon@example.org', 'human@example2.com', 'Notification', 'first line\nsecond line'],
                  ['', 'third@sender.com', 'third@recipient.net', 'test subject', 'message number three']]
        for i in range(3):
            with open(f_abs[i], 'w') as f:
                f.write('\n'.join(f_data[i]))
        os.symlink(f_rel[0], self.mailer.get_first_filename())
        os.symlink(f_rel[2], self.mailer.get_last_filename())

        # 3 -> 2 messages in the queue
        readdata = self.mailer.dequeue()
        files = os.listdir(self.maildir)
        self.assertEqual(len(files), 4, 'Files: ' + str(files))
        self.assertEqual(len(readdata), 4, 'Read message: ' + str(readdata))
        self.assertEqual(readdata['From'], f_data[0][1])
        self.assertEqual(readdata['To'], f_data[0][2])
        self.assertEqual(readdata['Subject'], f_data[0][3])
        self.assertEqual(readdata['Msg'], f_data[0][4])

        # 2 -> 1 messages in the queue
        readdata = self.mailer.dequeue()
        files = os.listdir(self.maildir)
        self.assertEqual(len(files), 3, 'Files: ' + str(files))
        self.assertEqual(len(readdata), 4, 'Read message: ' + str(readdata))
        self.assertEqual(readdata['From'], f_data[1][1])
        self.assertEqual(readdata['To'], f_data[1][2])
        self.assertEqual(readdata['Subject'], f_data[1][3])
        self.assertEqual(readdata['Msg'], f_data[1][4])

        # 1 -> 0 messages in the queue
        readdata = self.mailer.dequeue()
        files = os.listdir(self.maildir)
        self.assertEqual(len(files), 0, 'Files: ' + str(files))
        self.assertEqual(len(readdata), 4, 'Read message: ' + str(readdata))
        self.assertEqual(readdata['From'], f_data[2][1])
        self.assertEqual(readdata['To'], f_data[2][2])
        self.assertEqual(readdata['Subject'], f_data[2][3])
        self.assertEqual(readdata['Msg'], f_data[2][4])

        # Empty dequeue
        self.assertEqual(self.mailer.dequeue(), None)
        files = os.listdir(self.maildir)
        self.assertEqual(len(files), 0)

    def testUpdate(self):
        def wait_and_update(delay):
            t0 = time.time()
            while time.time() - t0 < delay:
                self.mailer.update()

        self.mailer = Mailer(mail_dir=self.maildir, client_rate=2, client_rate_window=1, dry_run=True)
        self.mailer.update()

        self.mailer.enqueue("s1", "r1", "subj1", "m1")
        self.mailer.enqueue("s2", "r2", "subj2", "m2")
        self.mailer.enqueue("s3", "r3", "subj3", "m3")
        self.mailer.enqueue("s4", "r4", "subj4", "m4")
        wait_and_update(0.5)    # window 1: 0.5

        files = os.listdir(self.maildir)
        self.assertEqual(len(files), 4, "Files in maildir: " + str(os.listdir(self.maildir)))
        self.assertTrue('first' in files)
        self.assertTrue('last' in files)
        self.mailer.update()
        self.assertEqual(len(os.listdir(self.maildir)), 4)

        self.mailer.enqueue("s5", "r5", "subj5", "m5")

        wait_and_update(0.7)    # window 1: 1.2
        self.assertEqual(len(os.listdir(self.maildir)), 3)
        self.assertTrue('first' in files)
        self.assertTrue('last' in files)

        wait_and_update(0.9)    # window 2: 0.1
        self.assertEqual(len(os.listdir(self.maildir)), 0)

