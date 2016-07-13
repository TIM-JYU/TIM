import os
import unittest

import dumboclient
import initdb2
from filemodehelper import change_permission_and_retry
from tim_app import db, app
from timdb.timdb2 import TimDb
from utils import del_content


class TimDbTest(unittest.TestCase):
    test_files_path = '/tmp/doctest_files'
    db_path = app.config['DATABASE']
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
        initdb2.initialize_temp_database()
        # Safety mechanism
        assert app.config['SQLALCHEMY_BINDS']['tim_main'] == "postgresql://postgres@postgresql:5432/tempdb_" + 'timtest'
        db.session.commit()
        db.drop_all(bind='tim_main')
        initdb2.initialize_database(create_docs=False)
        cls.dumbo = dumboclient.launch_dumbo()

    @classmethod
    def tearDownClass(cls):
        cls.dumbo.kill()

    def setUp(self):
        pass
