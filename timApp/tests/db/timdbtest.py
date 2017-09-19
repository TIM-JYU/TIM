import glob
import os
import unittest
from typing import Union, List

import sqlalchemy.exc

import timApp.dumboclient
import timApp.initdb2
from timApp.documentmodel.document import Document
from timApp.filemodehelper import change_permission_and_retry
from timApp.tim_app import app
from timApp.timdb.models.docentry import DocEntry
from timApp.timdb.models.user import User
from timApp.timdb.tim_models import db
from timApp.timdb.timdb2 import TimDb
from timApp.utils import del_content


class TimDbTest(unittest.TestCase):
    test_files_path = '/tmp/doctest_files'
    db_path = app.config['DATABASE']
    dumbo = None
    i = 0

    def get_db(self):
        return self.db

    @classmethod
    def setUpClass(cls):
        if os.path.exists(cls.test_files_path):
            # Safety mechanism
            assert cls.test_files_path != '/tim_files'
            del_content(cls.test_files_path, onerror=change_permission_and_retry)
            for f in glob.glob('/tmp/heading_cache_*'):
                os.remove(f)
            for f in glob.glob('/tmp/tim_auto_macros_*'):
                os.remove(f)
        else:
            os.mkdir(cls.test_files_path)
        timApp.initdb2.initialize_temp_database()
        # Safety mechanism to make sure we are not wiping some production database
        assert app.config['SQLALCHEMY_BINDS']['tim_main'].endswith('-test')
        db.session.commit()
        db.get_engine(app, 'tim_main').dispose()
        # The following throws if the testing database has not been created yet; we can safely ignore it
        try:
            db.drop_all(bind='tim_main')
        except sqlalchemy.exc.OperationalError:
            pass
        timApp.initdb2.initialize_database(create_docs=False)
        cls.dumbo = timApp.dumboclient.launch_dumbo()

    @classmethod
    def tearDownClass(cls):
        cls.dumbo.kill()
        cls.dumbo.wait()

    def setUp(self):
        self.db = TimDb(files_root_path=self.test_files_path)

    def tearDown(self):
        """While testing, the Flask-SQLAlchemy session needs to be removed manually; see https://pythonhosted.org/Flask-
        Testing/#testing-with-sqlalchemy."""
        db.session.remove()
        self.db.close()

    def create_doc(self, from_file=None, initial_par: Union[str, List[str]]=None, settings=None):
        d = DocEntry.create(f'test{TimDbTest.i}', 0, 'test', from_file=from_file, initial_par=initial_par,
                            settings=settings)
        return d

    def init_doc(self, doc: Document, from_file, initial_par: Union[str, List[str]], settings):
        if from_file is not None:
            with open(from_file, encoding='utf-8') as f:
                doc.add_text(f.read())
        elif initial_par is not None:
            if isinstance(initial_par, str):
                doc.add_text(initial_par)
            elif isinstance(initial_par, list):
                for p in initial_par:
                    doc.add_text(p)
        if settings is not None:
            doc.set_settings(settings)

    @property
    def test_user_1(self):
        return User.get_by_name('testuser1')

    @property
    def test_user_2(self):
        return User.get_by_name('testuser2')

    @property
    def test_user_3(self):
        return User.get_by_name('testuser3')

    def get_test_user_1_group_id(self):
        return self.db.users.get_personal_usergroup_by_id(TEST_USER_1_ID)

    def get_test_user_2_group_id(self):
        return self.db.users.get_personal_usergroup_by_id(TEST_USER_2_ID)

    def assert_dict_subset(self, data, subset):
        for k, v in subset.items():
            self.assertEqual(data[k], v, msg=f'Key {k} was different')


TEST_USER_1_ID = 4
TEST_USER_2_ID = 5
TEST_USER_3_ID = 6

TEST_USER_1_NAME = 'Test user 1'
TEST_USER_2_NAME = 'Test user 2'
TEST_USER_3_NAME = 'Test user 3'
