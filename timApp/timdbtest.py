import os
import shutil
import unittest
from documentmodel.document import Document

import dumboclient
from filemodehelper import change_permission_and_retry
from timdb.timdb2 import TimDb


class TimDbTest(unittest.TestCase):

    dumbo = dumboclient.launch_dumbo()
    db = None
    test_files_path = 'doctest_files'

    @classmethod
    def setUpClass(cls):

        TEST_DB_NAME = ':memory:'
        if os.path.exists(cls.test_files_path):
            # Safety mechanism
            assert cls.test_files_path == 'doctest_files'
            shutil.rmtree(cls.test_files_path, onerror=change_permission_and_retry)
        db = TimDb(TEST_DB_NAME, cls.test_files_path)
        db.initialize_tables()
        Document.default_files_root = cls.test_files_path

    @classmethod
    def tearDownClass(cls):
        cls.dumbo.kill()

    def setUp(self):
        self.db = TimDbTest.db
