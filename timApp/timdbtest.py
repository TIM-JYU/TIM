import os
import shutil
import unittest
import ephemeralclient
from timdb.gitclient import GitClient
from timdb.timdb2 import TimDb


def change_permission_and_retry(func, path, exc_info):
    import stat

    # Change permission of the path so that it is deletable
    os.chmod(path, stat.S_IWUSR)
    func(path)


class TimDbTest(unittest.TestCase):

    @classmethod
    def setUpClass(cls):
        global e
        global db
        TEST_FILES_PATH = 'test_files'
        if os.path.exists(TEST_FILES_PATH):
            shutil.rmtree(TEST_FILES_PATH, onerror=change_permission_and_retry)
        TEST_DB_NAME = ':memory:'

        GitClient.initRepo(TEST_FILES_PATH)
        db = TimDb(TEST_DB_NAME, TEST_FILES_PATH)
        e = ephemeralclient.launch_ephemeral()
        db.initializeTables("schema2.sql")
        db.users.createAnonymousAndLoggedInUserGroups()

    @classmethod
    def tearDownClass(cls):
        e.kill()

    def setUp(self):
        global db
        self.db = db
