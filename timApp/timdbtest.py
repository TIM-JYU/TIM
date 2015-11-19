import os
import shutil
import unittest
from documentmodel.docparagraph import DocParagraph
from documentmodel.document import Document

import dumboclient
from filemodehelper import change_permission_and_retry
import initdb2
from timdb.timdb2 import TimDb


class TimDbTest(unittest.TestCase):
    dumbo = dumboclient.launch_dumbo()
    test_files_path = 'doctest_files'
    db_path = 'doctest_files/tim.db'

    @classmethod
    def get_db(cls):
        return TimDb(db_path=cls.db_path, files_root_path=cls.test_files_path)

    @classmethod
    def setUpClass(cls):
        if os.path.exists(cls.test_files_path):
            # Safety mechanism
            assert cls.test_files_path == 'doctest_files'
            shutil.rmtree(cls.test_files_path, onerror=change_permission_and_retry)
        Document.default_files_root = cls.test_files_path
        DocParagraph.default_files_root = cls.test_files_path
        initdb2.initialize_database(cls.db_path, cls.test_files_path, create_docs=False, print_progress=False)

    @classmethod
    def tearDownClass(cls):
        cls.dumbo.kill()

    def setUp(self):
        pass
