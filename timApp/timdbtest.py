import os
import unittest
from documentmodel.docparagraph import DocParagraph
from documentmodel.document import Document

import dumboclient
from filemodehelper import change_permission_and_retry
import initdb2
from timdb.timdb2 import TimDb
from utils import del_content


class TimDbTest(unittest.TestCase):
    test_files_path = '/tmp/doctest_files'
    db_path = os.path.join(test_files_path, 'tim.db')
    dumbo = None

    @classmethod
    def get_db(cls):
        return TimDb(db_path=cls.db_path, files_root_path=cls.test_files_path)

    @classmethod
    def setUpClass(cls):
        if os.path.exists(cls.test_files_path):
            # Safety mechanism
            assert cls.test_files_path != 'tim_files'
            del_content(cls.test_files_path, onerror=change_permission_and_retry)
        else:
            os.mkdir(cls.test_files_path)
        os.system('mount -t tmpfs -o size=2m tmpfs ' + os.path.abspath(cls.test_files_path))
        Document.default_files_root = cls.test_files_path
        DocParagraph.default_files_root = cls.test_files_path
        initdb2.initialize_database(cls.db_path, cls.test_files_path, create_docs=False, print_progress=False)
        cls.dumbo = dumboclient.launch_dumbo()

    @classmethod
    def tearDownClass(cls):
        cls.dumbo.kill()
        os.system('umount ' + os.path.abspath(cls.test_files_path))

    def setUp(self):
        pass
