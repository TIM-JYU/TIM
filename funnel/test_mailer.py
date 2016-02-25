import json
import logging
import os
import time
import unittest
from shutil import rmtree

from mailer import Mailer


class NoWarnings:
    def __enter__(self):
        logging.getLogger().setLevel("ERROR")

    def __exit__(self, type, value, traceback):
        logging.getLogger().setLevel("WARNING")


class MailerTest(unittest.TestCase):
    def setUp(self):
        self.maildir = os.path.join(os.getcwd(), 'test_mail')
        if os.path.exists(self.maildir):
            rmtree(self.maildir)

        self.mailer = Mailer(mail_dir=self.maildir, dry_run=True)

    def tearDown(self):
        if os.path.exists(self.maildir):
            rmtree(self.maildir)

    def mkheader(self, sender: str, rcpt: str, subj: str) -> dict:
        return {"From": sender, "Rcpt-To": rcpt, "Subject": subj}

    def mkmsg(self, next_msg: str, sender: str, rcpt: str, subj: str, msg: str):
        return {"From": sender, "Rcpt-To": rcpt, "Subject": subj, "Body": msg, "Next-Msg": next_msg}

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

        f1_data = {'From': 'from-addr', 'Rcpt-To': 'to-addr', 'Subject': 'subj', 'Next-Msg': f1_rel}
        with open(f1_abs, 'w') as f1:
            f1.write(json.dumps(f1_data))

        self.mailer.set_next(f1_abs, f2_rel)

        with open(f1_abs, 'r') as f1:
            f1_newdata = json.loads(f1.read())

        self.assertEqual(len(f1_newdata), len(f1_data))
        for key in f1_data:
            self.assertEqual(f1_newdata[key], f1_newdata[key], 'f1_newdata[{0}] == {1} != {2} == f1_data[{0}]'.format(
                key, f1_newdata[key], f1_data[key]))

    def testEnqueue(self):
        # First enqueue
        f1_data = self.mkmsg('', 'sender', 'recipient', 'subject', 'message')
        f1_file = self.mailer.enqueue(f1_data, f1_data['Body'])
        self.assertTrue(self.mailer.has_messages())

        files = os.listdir(self.maildir)
        self.assertEqual(len(files), 3)
        self.assertTrue('first' in files)
        self.assertTrue('last' in files)
        self.assertTrue(f1_file in files)
        self.assertEqual(os.readlink(self.mailer.get_first_filename()), f1_file)
        self.assertEqual(os.readlink(self.mailer.get_last_filename()), f1_file)

        with open(os.path.join(self.maildir, f1_file)) as f1:
            f1_readdata = json.loads(f1.read())

        self.assertEqual(f1_readdata['From'], f1_data['From'])
        self.assertEqual(f1_readdata['Rcpt-To'], f1_data['Rcpt-To'])
        self.assertEqual(f1_readdata['Subject'], f1_data['Subject'])
        self.assertEqual(f1_readdata['Body'], f1_data['Body'])

        # Second enqueue
        f2_data = self.mkmsg('', 'daemon@example.org', 'human@example2.com', 'Notification', 'first line\nsecond line')
        f2_file = self.mailer.enqueue(f2_data, f2_data['Body'])
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
            f2_readdata = json.loads(f2.read())

        self.assertEqual(f2_readdata['From'], f2_data['From'])
        self.assertEqual(f2_readdata['Rcpt-To'], f2_data['Rcpt-To'])
        self.assertEqual(f2_readdata['Subject'], f2_data['Subject'])
        self.assertEqual(f2_readdata['Body'], f2_data['Body'])

        # Third / nth enqueue
        f3_file = self.mailer.enqueue(f1_data, f1_data['Body'])
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
            f3_readdata = json.loads(f3.read())

        self.assertEqual(f3_readdata['From'], f1_data['From'])
        self.assertEqual(f3_readdata['Rcpt-To'], f1_data['Rcpt-To'])
        self.assertEqual(f3_readdata['Subject'], f1_data['Subject'])
        self.assertEqual(f3_readdata['Body'], f1_data['Body'])

    def testDequeue(self):
        # Empty dequeue
        with NoWarnings():
            self.assertEqual(self.mailer.dequeue(), None)
        files = os.listdir(self.maildir)
        self.assertEqual(len(files), 0)

        # Single element

        f1_data = self.mkmsg('', 'sender', 'recipient', 'subject', 'message')
        f1_abs, f1_rel = self.mailer.get_random_filenames()
        with open(f1_abs, 'w') as f1:
            f1.write(json.dumps(f1_data))
        os.symlink(f1_rel, self.mailer.get_first_filename())
        os.symlink(f1_rel, self.mailer.get_last_filename())

        readdata = self.mailer.dequeue()

        files = os.listdir(self.maildir)
        self.assertEqual(len(files), 0)
        self.assertEqual(len(readdata), 4, 'Read message: ' + str(readdata))
        self.assertEqual(readdata['From'], f1_data['From'])
        self.assertEqual(readdata['Rcpt-To'], f1_data['Rcpt-To'])
        self.assertEqual(readdata['Subject'], f1_data['Subject'])
        self.assertEqual(readdata['Body'], f1_data['Body'])

        # Multiple elements
        f_abs, f_rel = zip(*[self.mailer.get_random_filenames() for _ in range(3)])
        f_data = [self.mkmsg(f_rel[1], f1_data["From"], f1_data["Rcpt-To"], f1_data["Subject"], f1_data["Body"]),
                  self.mkmsg(f_rel[2], 'daemon@example.org', 'human@example2.com', 'Notification', 'first line\nsecond line'),
                  self.mkmsg('', 'third@sender.com', 'third@recipient.net', 'test subject', 'message number three')]
        for i in range(3):
            with open(f_abs[i], 'w') as f:
                f.write(json.dumps(f_data[i]))
        os.symlink(f_rel[0], self.mailer.get_first_filename())
        os.symlink(f_rel[2], self.mailer.get_last_filename())

        # 3 -> 2 messages in the queue
        readdata = self.mailer.dequeue()
        files = os.listdir(self.maildir)
        self.assertEqual(len(files), 4, 'Files: ' + str(files))
        self.assertEqual(len(readdata), 4, 'Read message: ' + str(readdata))
        self.assertEqual(readdata['From'], f_data[0]["From"])
        self.assertEqual(readdata['Rcpt-To'], f_data[0]["Rcpt-To"])
        self.assertEqual(readdata['Subject'], f_data[0]["Subject"])
        self.assertEqual(readdata['Body'], f_data[0]["Body"])

        # 2 -> 1 messages in the queue
        readdata = self.mailer.dequeue()
        files = os.listdir(self.maildir)
        self.assertEqual(len(files), 3, 'Files: ' + str(files))
        self.assertEqual(len(readdata), 4, 'Read message: ' + str(readdata))
        self.assertEqual(readdata['From'], f_data[1]["From"])
        self.assertEqual(readdata['Rcpt-To'], f_data[1]["Rcpt-To"])
        self.assertEqual(readdata['Subject'], f_data[1]["Subject"])
        self.assertEqual(readdata['Body'], f_data[1]["Body"])

        # 1 -> 0 messages in the queue
        readdata = self.mailer.dequeue()
        files = os.listdir(self.maildir)
        self.assertEqual(len(files), 0, 'Files: ' + str(files))
        self.assertEqual(len(readdata), 4, 'Read message: ' + str(readdata))
        self.assertEqual(readdata['From'], f_data[2]["From"])
        self.assertEqual(readdata['Rcpt-To'], f_data[2]["Rcpt-To"])
        self.assertEqual(readdata['Subject'], f_data[2]["Subject"])
        self.assertEqual(readdata['Body'], f_data[2]["Body"])

        # Empty dequeue
        with NoWarnings():
            self.assertEqual(self.mailer.dequeue(), None)
        files = os.listdir(self.maildir)
        self.assertEqual(len(files), 0)

    def wait_and_update(self, delay):
        t0 = time.time()
        while time.time() - t0 < delay:
            self.mailer.update()

    def testUpdate(self):
        self.mailer = Mailer(mail_dir=self.maildir, client_rate=2, client_rate_window=1, dry_run=True)
        self.mailer.update()

        self.mailer.enqueue(self.mkheader("s1", "r1", "subj1"), "m1")
        self.mailer.enqueue(self.mkheader("s2", "r2", "subj2"), "m2")
        self.mailer.enqueue(self.mkheader("s3", "r3", "subj3"), "m3")
        self.mailer.enqueue(self.mkheader("s4", "r4", "subj4"), "m4")
        self.wait_and_update(0.5)    # window 1: 0.5

        files = os.listdir(self.maildir)
        self.assertEqual(len(files), 4, "Files in maildir: " + str(os.listdir(self.maildir)))
        self.assertTrue('first' in files)
        self.assertTrue('last' in files)
        self.mailer.update()
        self.assertEqual(len(os.listdir(self.maildir)), 4)

        self.mailer.enqueue(self.mkheader("s5", "r5", "subj5"), "m5")

        self.wait_and_update(0.7)    # window 1: 1.2
        self.assertEqual(len(os.listdir(self.maildir)), 3)
        self.assertTrue('first' in files)
        self.assertTrue('last' in files)

        self.wait_and_update(0.9)    # window 2: 0.1
        self.assertEqual(len(os.listdir(self.maildir)), 0)

    def testNewInstance(self):
        self.mailer.enqueue(self.mkheader("s1", "r1", "subj1"), "m1")
        self.mailer.enqueue(self.mkheader("s2", "r2", "subj2"), "m2")
        self.mailer.enqueue(self.mkheader("s3", "r3", "subj3"), "m3")
        self.mailer.enqueue(self.mkheader("s4", "r4", "subj4"), "m4")

        self.mailer = Mailer(mail_dir=self.maildir, client_rate=2, client_rate_window=1, dry_run=True)
        self.mailer.update()

        self.wait_and_update(0.5)    # window 1: 0.5

        files = os.listdir(self.maildir)
        self.assertEqual(len(files), 4, "Files in maildir: " + str(os.listdir(self.maildir)))
        self.assertTrue('first' in files)
        self.assertTrue('last' in files)
        self.mailer.update()
        self.assertEqual(len(os.listdir(self.maildir)), 4)

        self.mailer.enqueue(self.mkheader("s5", "r5", "subj5"), "m5")

        self.wait_and_update(0.7)    # window 1: 1.2
        self.assertEqual(len(os.listdir(self.maildir)), 3)
        self.assertTrue('first' in files)
        self.assertTrue('last' in files)

        self.wait_and_update(0.9)    # window 2: 0.1
        self.assertEqual(len(os.listdir(self.maildir)), 0)
