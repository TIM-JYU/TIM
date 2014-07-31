"""Unit tests for TimDb."""

import shutil
import unittest
from hypothesis.statefultesting import *
from hypothesis.testdecorators import *

from timdb.timdb import TimDb

TEST_DB_NAME = ':memory:'
TEST_FILES_PATH = 'test_files'


class TestTimDb(unittest.TestCase):

    def setUp(self):
        self.db = TimDb(TEST_DB_NAME, TEST_FILES_PATH)
        self.db.initializeTables()
        #self.db.clear()
    
    @given(str)
    def test_user(self, TEST_USER_NAME):
        #TEST_USER_NAME = 'test_name'
        #print(TEST_USER_NAME)
        user_id = self.db.createUser(TEST_USER_NAME)
        self.assertIsNotNone(user_id, "user_id was None")
        user = self.db.getUser(user_id)
        self.assertEqual(TEST_USER_NAME, user['name'], 'User name was not saved properly')
        self.assertEqual(user_id, user['id'], 'User id was not saved properly')
    
    @given(str,str)
    def test_document(self, TEST_DOCUMENT_NAME, TEST_DOCUMENT_NAME2):
        #TEST_DOCUMENT_NAME = 'test_document'
        #TEST_DOCUMENT_NAME2 = 'some_other_document'
        document_id = self.db.createDocument(TEST_DOCUMENT_NAME)
        self.assertIsNotNone(document_id, "document_id was None")
        self.assertEqual(TEST_DOCUMENT_NAME, self.db.getDocument(document_id)['name'], 'Document name was not saved properly')
        
        document_id2 = self.db.createDocument(TEST_DOCUMENT_NAME2)
        self.assertIsNotNone(document_id2, "document_id2 was None")
        self.assertEqual(TEST_DOCUMENT_NAME2, self.db.getDocument(document_id2)['name'], 'Document name was not saved properly')
        
        docs = self.db.getDocumentsByIds([document_id, document_id2])
        self.assertEqual(2, len(docs), 'Length of docs was wrong')
        self.assertEqual(TEST_DOCUMENT_NAME, docs[0]['name'], 'Document name was not retrieved properly')
        self.assertEqual(TEST_DOCUMENT_NAME2, docs[1]['name'], 'Document name was not retrieved properly')
        
    @given(str,str,str)
    def test_blocks(self, TEST_CONTENT, TEST_CONTENT2, TEST_CONTENT3):
        #TEST_CONTENT = 'test content!'
        #TEST_CONTENT2 = 'second block'
        #TEST_CONTENT3 = 'something new'
        document_id = self.db.createDocument('test_document')
        
        self.db.addMarkDownBlock(document_id, TEST_CONTENT, None)
        block_ids = self.db.getDocumentBlockIds(document_id)
        self.assertEqual(1, len(block_ids), 'Block count was wrong')
        blocks = self.db.getDocumentBlocks(document_id)
        self.assertEqual(TEST_CONTENT, blocks[0]['text'], 'Block content was wrong')
        
        self.db.addMarkDownBlock(document_id, TEST_CONTENT2, block_ids[0])
        block_ids = self.db.getDocumentBlockIds(document_id)
        self.assertEqual(2, len(block_ids), 'Block count was wrong')
        blocks = self.db.getDocumentBlocks(document_id)
        self.assertEqual(TEST_CONTENT2, blocks[0]['text'], 'Block content was wrong')
        self.assertEqual(TEST_CONTENT, blocks[1]['text'], 'Block content was wrong')
        
        self.db.modifyMarkDownBlock(block_ids[0], TEST_CONTENT3)
        blocks = self.db.getDocumentBlocks(document_id)
        self.assertEqual(TEST_CONTENT3, blocks[0]['text'], 'Block content was wrong')
        
    def tearDown(self):
        self.db.close()
        return

# class TimDbTester(StatefulTest):
#     def __init__(self):
#         self.target = TimDb(TEST_DB_NAME, TEST_FILES_PATH)
#         self.target.clear()
#     
#     @step
#     @requires(str)
#     def addUser(self, name):
#         print(name)
#         user_id = self.target.createUser(name)
#         assert user_id is not None
        
if __name__ == '__main__':
    shutil.rmtree(TEST_FILES_PATH)
    unittest.main()
    #TimDbTester.breaking_example()
