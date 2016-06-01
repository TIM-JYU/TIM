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

    def testInit(self):
        self.assertTrue(os.path.exists(self.dir))
        self.assertEqual(len(listfiles(self.dir)), 0)
        self.assertEqual(self.queue.get_first_filename(), os.path.join(self.dir, 'first'))
        self.assertEqual(self.queue.get_last_filename(), os.path.join(self.dir, 'last'))
        self.assertTrue(self.queue.is_empty())

    def testRandomFilename(self):
        for i in range(10):
            random_abs, random_rel = self.queue.get_random_filenames()
            self.assertEqual(random_abs, os.path.join(self.dir, random_rel))
            self.assertFalse(os.path.isfile(random_abs))
            with open(random_abs, 'w') as f:
                pass
            self.assertTrue(os.path.isfile(random_abs))

    def testSetNext(self):
        f1_abs, f1_rel = self.queue.get_random_filenames()
        f2_abs, f2_rel = self.queue.get_random_filenames()
    
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
