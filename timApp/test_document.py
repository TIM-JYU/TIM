import os
import shutil
import unittest
from hypothesis.testdecorators import *
from timdb.timdb2 import TimDb
import ephemeralclient
from timdb.gitclient import initRepo
from timdb.timdbbase import TimDbException


def onerror(func, path, exc_info):
    import stat
    if not os.access(path, os.W_OK):
        # Is the error an access error ?
        os.chmod(path, stat.S_IWUSR)
        func(path)
    else:
        assert False


def debug_print(name, msg):
    print("{}: '{}', hex: {}".format(name, msg, ':'.join(hex(ord(x))[2:] for x in msg)))


class MyTestCase(unittest.TestCase):

    def setUp(self):
        global db
        self.db = db

    @given(str, verifier=Verifier(timeout=9999, max_size=10))
    def test_create_document(self, name):
        debug_print('test_create_document', name)
        if '\0' in name:
            with self.assertRaises(TimDbException):
                self.db.documents.createDocument(name, 0)
            return
        else:
            doc = self.db.documents.createDocument(name, 0)

        markdown = self.db.documents.getDocumentMarkdown(doc)
        self.assertEqual(markdown, 'Edit me!')

        meta = self.db.documents.getDocument(doc)
        self.assertTrue('id' in meta)
        self.assertTrue('name' in meta)
        self.assertEqual(len(meta), 2)

        htmlblocks = self.db.documents.getDocumentAsHtmlBlocks(doc)
        self.assertEqual(len(htmlblocks), 1)
        self.assertEqual(htmlblocks[0], '<p>Edit me!</p>')

        blocks = self.db.documents.getDocumentAsBlocks(doc)
        self.assertEqual(blocks[0], 'Edit me!\n')

    @given(str, verifier=Verifier(timeout=9999, max_size=10))
    def test_edit_document(self, newText):
        debug_print('test_edit_document', newText)
        doc = self.db.documents.createDocument('test', 0)
        new_doc = self.db.documents.updateDocument(doc, newText)
        actualText = self.db.documents.getDocumentMarkdown(new_doc)
        self.assertEqual(newText, actualText)
        versions = self.db.documents.getDocumentVersions(doc.id)
        self.assertEqual(len(versions), 2)
        self.assertEqual(versions[0]['hash'], new_doc.hash)
        self.assertEqual(versions[1]['hash'], doc.hash)


try:
    TEST_FILES_PATH = 'test_files'
    if os.path.exists(TEST_FILES_PATH):
        shutil.rmtree(TEST_FILES_PATH, onerror=onerror)
    TEST_DB_NAME = ':memory:'

    db = TimDb(TEST_DB_NAME, TEST_FILES_PATH)
    e = ephemeralclient.launch_ephemeral()
    db.initializeTables("schema2.sql")
    db.users.createAnonymousUser()
    initRepo(TEST_FILES_PATH)
    unittest.main(warnings='ignore')

finally:
    e.kill()
