import json
import unittest
from shutil import rmtree

from fileutils import *
from persistent_queue import PersistentQueue


class QueueTest(unittest.TestCase):
    def setUp(self):

        self.dir = os.path.join(os.getcwd(), 'test_queue')
        if os.path.exists(self.dir):
            rmtree(self.dir)

        self.queue = PersistentQueue(self.dir)

    def tearDown(self):
        if os.path.exists(self.dir):
            rmtree(self.dir)

    @classmethod
    def mkmsg(cls, sender: str, rcpt: str, subj: str, msg: str):
        return {"From": sender, "Rcpt-To": rcpt, "Subject": subj, "Body": msg}

    def testInit(self):
        self.assertTrue(os.path.exists(self.dir))
        self.assertEqual(len(listfiles(self.dir)), 0)
        self.assertEqual(self.queue.get_first_filename(), os.path.join(self.dir, 'first'))
        self.assertEqual(self.queue.get_last_filename(), os.path.join(self.dir, 'last'))
        self.assertTrue(self.queue.is_empty())

    def testSetNext(self):
        f1_abs, f1_rel = get_random_filenames(self.dir)
        f2_abs, f2_rel = get_random_filenames(self.dir)
    
        f1_data = {'From': 'from-addr', 'Rcpt-To': 'to-addr', 'Subject': 'subj', '_next_file': f1_rel}
        with open(f1_abs, 'w') as f1:
            f1.write(json.dumps(f1_data))
    
        self.queue.set_next(f1_abs, f2_rel)
    
        with open(f1_abs, 'r') as f1:
            f1_newdata = json.loads(f1.read())
    
        self.assertEqual(len(f1_newdata), len(f1_data))
        for key in f1_data:
            self.assertEqual(f1_newdata[key], f1_newdata[key], 'f1_newdata[{0}] == {1} != {2} == f1_data[{0}]'.format(
                key, f1_newdata[key], f1_data[key]))

    def assertFailingIndex(self, index: int):
        try:
            test = self.queue[index]
            self.fail('Element at index -1 returned {} instead of raising exception'.format(test))
        except IndexError:
            pass

    def assertMsgEqual(self, actual_msg: dict, expected_msg: dict):
        for field in ['From', 'Rcpt-To', 'Subject']:
            self.assertEqual(actual_msg[field], expected_msg[field], "Field {} doesn't match".format(field))

    def testIndexing(self):
        self.assertFailingIndex(-1)
        self.assertFailingIndex(0)

        firstItem = self.mkmsg("s1", "r1", "subj1", "m1")
        self.queue.enqueue(firstItem)
        self.assertMsgEqual(self.queue[0], firstItem)
        self.assertFailingIndex(1)

        secondItem = self.mkmsg("s2", "r2", "subj2", "m2")
        self.queue.enqueue(secondItem)
        self.assertMsgEqual(self.queue[0], firstItem)
        self.assertMsgEqual(self.queue[1], secondItem)
        self.assertFailingIndex(2)

        thirdItem = self.mkmsg("s3", "r3", "subj3", "m3")
        self.queue.enqueue(thirdItem)
        self.assertMsgEqual(self.queue[0], firstItem)
        self.assertMsgEqual(self.queue[1], secondItem)
        self.assertMsgEqual(self.queue[2], thirdItem)
        self.assertFailingIndex(3)

        self.queue.dequeue()
        self.assertMsgEqual(self.queue[0], secondItem)
        self.assertMsgEqual(self.queue[1], thirdItem)
        self.assertFailingIndex(2)

        self.queue.dequeue()
        self.assertMsgEqual(self.queue[0], thirdItem)
        self.assertFailingIndex(1)

        self.queue.dequeue()
        self.assertFailingIndex(0)

    def testQueue(self):
        self.queue.enqueue(self.mkmsg("s1", "r1", "subj1", "m1"))
        self.queue.enqueue(self.mkmsg("s2", "r2", "subj2", "m2"))
        self.queue.enqueue(self.mkmsg("s3", "r3", "subj3", "m3"))
        self.queue.enqueue(self.mkmsg("s4", "r4", "subj4", "m4"))

        self.queue.dequeue()
        self.queue.dequeue()

        files = listfiles(self.dir)
        self.assertEqual(len(files), 4, "Files in queue dir: " + str(listfiles(self.dir)))
        self.assertTrue('first' in files)
        self.assertTrue('last' in files)
        self.assertEqual(len(listfiles(self.dir)), 4)

        self.queue.enqueue(self.mkmsg("s5", "r5", "subj5", "m5"))
        self.queue.dequeue()
        self.queue.dequeue()

        self.assertEqual(len(listfiles(self.dir)), 3)
        self.assertTrue('first' in files)
        self.assertTrue('last' in files)

        self.queue.dequeue()
        self.assertEqual(len(listfiles(self.dir)), 0)
