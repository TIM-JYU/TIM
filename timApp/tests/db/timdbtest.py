import os
import unittest

import sqlalchemy.exc

import dumboclient
import initdb2
from filemodehelper import change_permission_and_retry
from tim_app import app
from timdb.tim_models import db
from timdb.timdb2 import TimDb
from utils import del_content


class TimDbTest(unittest.TestCase):
    test_files_path = '/tmp/doctest_files'
    db_path = app.config['DATABASE']
    dumbo = None

    def get_db(self):
        return self.db

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
        db.get_engine(app, 'tim_main').dispose()
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
        self.db = TimDb(files_root_path=self.test_files_path)

    def tearDown(self):
        self.db.close()


TEST_USER_1_ID = 4
TEST_USER_2_ID = 5
TEST_USER_3_ID = 6

TEST_USER_1_NAME = 'Test user 1'
TEST_USER_2_NAME = 'Test user 2'
