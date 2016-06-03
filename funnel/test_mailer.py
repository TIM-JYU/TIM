import json
import logging
import unittest
from shutil import rmtree
from typing import Optional

from fileutils import *
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

        self.mailer = Mailer(mail_dir=self.maildir, client_rate=20, client_rate_window=60, dry_run=True)

    def tearDown(self):
        if os.path.exists(self.maildir):
            rmtree(self.maildir)

    @classmethod
    def mkheader(cls, sender: str, rcpt: str, subj: str, group_id: Optional[str] = None) -> dict:
        return {"From": sender, "Rcpt-To": rcpt, "Subject": subj, "Group-Id": group_id}

    @classmethod
    def mkmsg(cls, next_msg: str, sender: str, rcpt: str, subj: str, msg: str):
        return {"From": sender, "Rcpt-To": rcpt, "Subject": subj, "Body": msg, "_next_file": next_msg}

    def testInit(self):
        self.assertTrue(os.path.exists(self.maildir))
        self.assertEqual(len(listfiles(self.maildir)), 0)
        self.assertEqual(self.mailer.get_first_filename(), os.path.join(self.maildir, 'first'))
        self.assertEqual(self.mailer.get_last_filename(), os.path.join(self.maildir, 'last'))
        self.assertFalse(self.mailer.has_messages())

    def testEnqueue(self):
        # First enqueue
        f1_data = self.mkmsg('', 'sender', 'recipient', 'subject', 'message')
        f1_file = self.mailer.enqueue(f1_data, f1_data['Body'])
        self.assertIsNotNone(f1_file)
        self.assertTrue(self.mailer.has_messages())

        files = listfiles(self.maildir)
        self.assertEqual(len(files), 3)
        self.assertTrue('first' in files)
        self.assertTrue('last' in files)
        self.assertTrue(f1_file in files, '{} not found in {}'.format(f1_file, files))
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

        files = listfiles(self.maildir)
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

        files = listfiles(self.maildir)
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
        files = listfiles(self.maildir)
        self.assertEqual(len(files), 0)

        # Single element

        f1_data = self.mkmsg('', 'sender', 'recipient', 'subject', 'message')
        f1_abs, f1_rel = get_random_filenames(self.maildir)
        with open(f1_abs, 'w') as f1:
            f1.write(json.dumps(f1_data))
        os.symlink(f1_rel, self.mailer.get_first_filename())
        os.symlink(f1_rel, self.mailer.get_last_filename())

        readdata = self.mailer.dequeue()

        files = listfiles(self.maildir)
        self.assertEqual(len(files), 0)
        self.assertEqual(readdata['From'], f1_data['From'])
        self.assertEqual(readdata['Rcpt-To'], f1_data['Rcpt-To'])
        self.assertEqual(readdata['Subject'], f1_data['Subject'])
        self.assertEqual(readdata['Body'], f1_data['Body'])

        # Multiple elements
        f_abs, f_rel = zip(*[get_random_filenames(self.maildir) for _ in range(3)])
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
        files = listfiles(self.maildir)
        self.assertEqual(len(files), 4, 'Files: ' + str(files))
        self.assertEqual(readdata['From'], f_data[0]["From"])
        self.assertEqual(readdata['Rcpt-To'], f_data[0]["Rcpt-To"])
        self.assertEqual(readdata['Subject'], f_data[0]["Subject"])
        self.assertEqual(readdata['Body'], f_data[0]["Body"])

        # 2 -> 1 messages in the queue
        readdata = self.mailer.dequeue()
        files = listfiles(self.maildir)
        self.assertEqual(len(files), 3, 'Files: ' + str(files))
        self.assertEqual(readdata['From'], f_data[1]["From"])
        self.assertEqual(readdata['Rcpt-To'], f_data[1]["Rcpt-To"])
        self.assertEqual(readdata['Subject'], f_data[1]["Subject"])
        self.assertEqual(readdata['Body'], f_data[1]["Body"])

        # 1 -> 0 messages in the queue
        readdata = self.mailer.dequeue()
        files = listfiles(self.maildir)
        self.assertEqual(len(files), 0, 'Files: ' + str(files))
        self.assertEqual(readdata['From'], f_data[2]["From"])
        self.assertEqual(readdata['Rcpt-To'], f_data[2]["Rcpt-To"])
        self.assertEqual(readdata['Subject'], f_data[2]["Subject"])
        self.assertEqual(readdata['Body'], f_data[2]["Body"])

        # Empty dequeue
        with NoWarnings():
            self.assertEqual(self.mailer.dequeue(), None)
        files = listfiles(self.maildir)
        self.assertEqual(len(files), 0)

    def testUpdate(self):
        self.mailer = Mailer(mail_dir=self.maildir, client_rate=2, client_rate_window=60, dry_run=True)

        self.mailer.enqueue(self.mkheader("s1", "r1", "subj1"), "m1")
        self.mailer.enqueue(self.mkheader("s2", "r2", "subj2"), "m2")
        self.mailer.enqueue(self.mkheader("s3", "r3", "subj3"), "m3")
        self.mailer.enqueue(self.mkheader("s4", "r4", "subj4"), "m4")
        self.mailer.update(30)    # window 1: 30 s

        files = listfiles(self.maildir)
        self.assertEqual(len(files), 4, "Files in maildir: " + str(listfiles(self.maildir)))
        self.assertTrue('first' in files)
        self.assertTrue('last' in files)
        self.assertEqual(len(listfiles(self.maildir)), 4)

        self.mailer.enqueue(self.mkheader("s5", "r5", "subj5"), "m5")

        self.mailer.update(45)    # window 2: 25 s
        self.assertEqual(len(listfiles(self.maildir)), 3)
        self.assertTrue('first' in files)
        self.assertTrue('last' in files)

        self.mailer.update(50)    # window 2: 35 s
        self.assertEqual(len(listfiles(self.maildir)), 0)

    def testNewInstance(self):
        self.mailer.enqueue(self.mkheader("s1", "r1", "subj1"), "m1")
        self.mailer.enqueue(self.mkheader("s2", "r2", "subj2"), "m2")
        self.mailer.enqueue(self.mkheader("s3", "r3", "subj3"), "m3")
        self.mailer.enqueue(self.mkheader("s4", "r4", "subj4"), "m4")

        self.mailer = Mailer(mail_dir=self.maildir, client_rate=2, client_rate_window=1, dry_run=True)
        self.mailer.update(0.5)    # window 1: 0.5

        files = listfiles(self.maildir)
        self.assertEqual(len(files), 4, "Files in maildir: " + str(listfiles(self.maildir)))
        self.assertTrue('first' in files)
        self.assertTrue('last' in files)
        self.assertEqual(len(listfiles(self.maildir)), 4)

        self.mailer.enqueue(self.mkheader("s5", "r5", "subj5"), "m5")

        self.mailer.update(0.7)    # window 1: 1.2
        self.assertEqual(len(listfiles(self.maildir)), 3)
        self.assertTrue('first' in files)
        self.assertTrue('last' in files)

        self.mailer.update(0.9)    # window 2: 0.1
        self.assertEqual(len(listfiles(self.maildir)), 0)

    def testNonGrouping(self):
        self.mailer = Mailer(mail_dir=self.maildir, client_rate=4, client_rate_window=10,
                             group_delay=20, dry_run=True)

        groups = [None, '1st', '1st', None, '2nd', '3rd', '2nd', None, '1st', '3rd']
        files = []
        for i in range(0, 10):
            p = [s + str(i + 1) for s in ['s', 'r', 'subj', 'm']]
            name = self.mailer.enqueue(self.mkheader(p[0], p[1], p[2], groups[i]), p[3])
            files.append(os.path.join(self.maildir, name))

        self.assertEqual(len(self.mailer.queue), 3)
        self.assertEqual(len(self.mailer.group_queues), 3)
        self.assertEqual(len(self.mailer.get_group_queue('1st')), 3)
        self.assertEqual(len(self.mailer.get_group_queue('2nd')), 2)
        self.assertEqual(len(self.mailer.get_group_queue('3rd')), 2)

    def testGrouping(self):
        self.mailer = Mailer(mail_dir=self.maildir, client_rate=4, client_rate_window=10,
                             group_delay=20, dry_run=True)

        groups = [None, '1st', '1st', None, '2nd', '3rd', '2nd', None, '1st', '3rd']
        files = []
        for i in range(0, 10):
            p = [s + str(i + 1) for s in ['s', 'r', 'subj', 'm']]
            name = self.mailer.enqueue(self.mkheader(p[0], 'same_recipient', p[2], groups[i]), p[3])
            files.append(os.path.join(self.maildir, name))

        self.assertEqual(len(self.mailer.queue), 3)
        self.assertEqual(len(self.mailer.group_queues), 3)
        self.assertEqual(len(self.mailer.get_group_queue('1st')), 1)
        self.assertEqual(len(self.mailer.get_group_queue('2nd')), 1)
        self.assertEqual(len(self.mailer.get_group_queue('3rd')), 1)

        # todo: test group timing

