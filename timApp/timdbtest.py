import os
import unittest

import sqlalchemy.exc

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
        # Safety mechanism to make sure we are not wiping some production database
        assert app.config['SQLALCHEMY_BINDS']['tim_main'] == "postgresql://postgres@postgresql-timtest:5432/timtest"
        db.session.commit()

        # The following throws if the testing database has not been created yet; we can safely ignore it
        try:
            db.drop_all(bind='tim_main')
        except sqlalchemy.exc.OperationalError:
            pass
        initdb2.initialize_database(create_docs=False)
        cls.dumbo = dumboclient.launch_dumbo()

    @classmethod
    def tearDownClass(cls):
        cls.dumbo.kill()

    def setUp(self):
        pass
