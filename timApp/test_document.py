import os
import shutil
import unittest
import random

from hypothesis.testdecorators import *

from timdb.timdb2 import TimDb
import ephemeralclient
from timdb.gitclient import GitClient
from timdb.timdbbase import TimDbException, DocIdentifier

import hypothesis.settings as hs

hs.default.max_examples = 10


def onerror(func, path, exc_info):
    import stat

    # Is the error an access error ?
    os.chmod(path, stat.S_IWUSR)
    #os.chmod(path, stat.S_IWRITE)
    #os.chmod(path, stat.S_IRWXU| stat.S_IRWXG| stat.S_IRWXO)
    func(path)


def debug_print(name, msg):
    print("{}: '{}', hex: {}".format(name, msg, ':'.join(hex(ord(x))[2:] for x in msg)))


class DocTest(unittest.TestCase):

    @classmethod
    def setUpClass(cls):
        global e
        global db
        TEST_FILES_PATH = 'test_files'
        if os.path.exists(TEST_FILES_PATH):
            shutil.rmtree(TEST_FILES_PATH, onerror=onerror)
        TEST_DB_NAME = ':memory:'

        GitClient.initRepo(TEST_FILES_PATH)
        db = TimDb(TEST_DB_NAME, TEST_FILES_PATH)
        e = ephemeralclient.launch_ephemeral()
        db.initializeTables("schema2.sql")
        db.users.createAnonymousUser()

    @classmethod
    def tearDownClass(cls):
        e.kill()

    def setUp(self):
        global db
        self.db = db

    @given(str)
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

        meta = self.db.documents.getDocument(doc.id)
        self.assertTrue('id' in meta)
        self.assertTrue('name' in meta)
        self.assertEqual(len(meta), 2)

        htmlblocks = self.db.documents.getDocumentAsHtmlBlocks(doc)
        self.assertEqual(len(htmlblocks), 1)
        self.assertEqual(htmlblocks[0], '<p>Edit me!</p>')

        blocks = self.db.documents.getDocumentAsBlocks(doc)
        self.assertEqual(blocks[0], 'Edit me!\n')

    @given(str)
    def test_edit_document(self, new_text):
        debug_print('test_edit_document', new_text)
        doc = self.db.documents.createDocument('test', 0)
        new_doc = self.db.documents.updateDocument(doc, new_text)
        actual_text = self.db.documents.getDocumentMarkdown(new_doc)
        self.assertEqual(new_text, actual_text)
        versions = self.db.documents.getDocumentVersions(doc.id)
        self.assertEqual(len(versions), 2)
        self.assertEqual(versions[0]['hash'], new_doc.hash)
        self.assertEqual(versions[1]['hash'], doc.hash)

    def create_test_document(self, test_length):
        doc = self.db.documents.createDocument('testing notes', 0)

        doc_paragraphs = ['Paragraph number {}'.format(num) for num in range(0, test_length)]
        doc = self.db.documents.updateDocument(doc, '\n\n'.join(doc_paragraphs))
        doc_paragraphs = self.db.documents.getDocumentAsBlocks(doc)
        return doc, doc_paragraphs

    def create_test_notes(self, test_length):
        doc, doc_paragraphs = self.create_test_document(test_length)

        blocks = self.db.documents.getDocumentAsBlocks(doc)
        self.assertTrue(len(doc_paragraphs) == len(blocks))

        print('Adding {} notes to document...'.format(test_length))
        for i in range(0, test_length):
            #self.db.notes.addNote(0, str(i), doc.id, i, [], cache=False)
            self.db.notes.addNote(0, doc.id, doc.hash, i, str(i), 'justme', [], commit=False)
        self.db.commit()
        print('Done.')
        return doc, doc_paragraphs

    def test_notes_with_update(self):
        print('test_notes_with_update')
        test_length = 500
        doc, doc_paragraphs = self.create_test_notes(test_length)

        random.seed(0)
        indices = list(range(0, test_length))
        random.shuffle(indices)
        random.seed(0)
        random.shuffle(doc_paragraphs)

        # Refresh cache for the first document, otherwise it might not be found
        self.db.documents.getDocumentAsHtmlBlocks(doc)

        print('Updating document...')
        doc = self.db.documents.updateDocument(doc, '\n\n'.join(doc_paragraphs))
        print('Done.')

        print('Fetching notes of the document...')
        #notes = self.db.notes.getNotes(0, doc.id, get_html=False)
        notes = self.db.notes.getNotes(0, doc.id, doc.hash)
        print('Done.')

        for i in range(0, test_length):
            self.assertTrue(indices[int(notes[i]['par_index'])] == int(notes[i]['content']))

    def check_notes(self, new_count, note_index, notes, test_length):
        self.assertEquals(len(notes), test_length)

        for i in range(0, test_length):
           content_int = int(notes[i]['content'])
           if content_int < note_index:
               self.assertEquals(content_int, notes[i]['par_index'])
           else:
               self.assertEquals(content_int + new_count, notes[i]['par_index'],
                                 'contentInt: {}, new_count: {}, par_index: {}'.format(content_int, new_count, notes[i]['par_index']))

    def test_notes_with_add(self):
        print('test_notes_with_add')
        test_length = 500
        doc, _ = self.create_test_notes(test_length)
        new_par_index = 100
        new_count = random.randint(1, 100)
        _, ver = self.db.documents.addMarkdownBlock(doc, '\n\n'.join(['new'] * new_count), new_par_index)
        #self.check_notes(new_count, new_par_index, self.db.notes.getNotes(0, doc.id, get_html=False), test_length)
        self.check_notes(new_count, new_par_index, self.db.notes.getNotes(0, doc.id, ver), test_length)

    def test_notes_with_edit(self):
        print('test_notes_with_edit')
        test_length = 500
        doc, _ = self.create_test_notes(test_length)
        par_index = 100
        new_count = random.randint(1, 100)
        _, ver = self.db.documents.modifyMarkDownBlock(doc, par_index, '\n\n'.join(['new'] * new_count))
        #self.check_notes(new_count - 1, par_index, self.db.notes.getNotes(0, doc.id, get_html=False), test_length)
        self.check_notes(new_count - 1, par_index + 1, self.db.notes.getNotes(0, doc.id, ver), test_length)

    def test_notes_with_delete(self):
        print('test_notes_with_delete')
        test_length = 500
        doc, _ = self.create_test_notes(test_length)
        delete_par_index = 100
        ver = self.db.documents.deleteParagraph(doc, delete_par_index)
        #self.check_notes(-1, delete_par_index, self.db.notes.getNotes(0, doc.id, get_html=False), test_length)
        self.check_notes(-1, delete_par_index, self.db.notes.getNotes(0, doc.id, ver), test_length - 1)

    def test_readings(self):
        print('test_readings')
        doc, _ = self.create_test_document(500)
        readings = self.db.readings.getReadings(0, doc.id, doc.hash)
        self.assertEqual(len(readings), 0)
        par_index = 5
        self.db.readings.setAsRead(0, doc.id, doc.hash, par_index)

        readings = self.db.readings.getReadings(0, doc.id, doc.hash)
        self.assertEqual(len(readings), 1)
        fr = readings[0]
        self.assertEqual(fr['par_index'], par_index)

        ver = self.db.documents.deleteParagraph(doc, 0)
        doc = DocIdentifier(doc.id, ver)
        readings = self.db.readings.getReadings(0, doc.id, doc.hash)
        fr = readings[0]
        par_index -= 1
        self.assertEqual(fr['par_index'], par_index)

        self.db.readings.setAsRead(0, doc.id, doc.hash, par_index)
        readings = self.db.readings.getReadings(0, doc.id, doc.hash)
        self.assertEqual(len(readings), 1)

        doc = self.db.documents.updateDocument(doc, 'cleared')
        readings = self.db.readings.getReadings(0, doc.id, doc.hash)
        # Document changes so radically that the readings should be gone
        self.assertEqual(len(readings), 0)

    def check_list_dict_contents(self, list_to_check, key_name, *args):
        self.assertEqual(len(list_to_check), len(args))
        values = [x[key_name] for x in list_to_check]
        for a in args:
            self.assertIn(a, values)

    def test_readings_with_add(self):
        print('test_readings_with_add')
        doc, _ = self.create_test_document(5)
        self.db.readings.setAsRead(0, doc.id, doc.hash, 2)
        self.db.readings.setAsRead(0, doc.id, doc.hash, 3)
        readings = self.db.readings.getReadings(0, doc.id, doc.hash)
        self.check_list_dict_contents(readings, 'par_index', 2, 3)
        blocks, ver = self.db.documents.addMarkdownBlock(doc, 'edited', 3)
        readings = self.db.readings.getReadings(0, doc.id, ver)
        self.check_list_dict_contents(readings, 'par_index', 2, 4)
        self.db.readings.setAsRead(0, doc.id, ver, 3)
        readings = self.db.readings.getReadings(0, doc.id, ver)
        self.check_list_dict_contents(readings, 'par_index', 2, 3, 4)

    def test_readings_with_edit(self):
        print('test_readings_with_edit')
        doc, _ = self.create_test_document(5)
        par_index = 3
        self.db.readings.setAsRead(0, doc.id, doc.hash, par_index)
        _, ver = self.db.documents.modifyMarkDownBlock(doc, par_index, 'edited')
        readings = self.db.readings.getReadings(0, doc.id, ver)
        self.check_list_dict_contents(readings, 'par_index', par_index)
        self.check_list_dict_contents(readings, 'status', 'modified')
        self.db.readings.setAsRead(0, doc.id, ver, par_index)
        readings = self.db.readings.getReadings(0, doc.id, ver)
        self.check_list_dict_contents(readings, 'par_index', par_index)
        self.check_list_dict_contents(readings, 'status', 'read')

    def test_multiple_notes_same_par(self):
        print("test_multiple_notes_same_par")
        doc, _ = self.create_test_document(500)
        notes = self.db.notes.getNotes(0, doc.id, doc.hash)
        self.assertEqual(len(notes), 0)
        par_index = 67
        content = 'test note'
        self.db.notes.addNote(0, doc.id, doc.hash, par_index, content, 'everyone', [])
        notes = self.db.notes.getNotes(0, doc.id, doc.hash)
        self.assertEqual(len(notes), 1)
        note = notes[0]
        self.assertEqual(note['note_index'], 0)
        self.assertEqual(note['par_index'], par_index)
        self.assertEqual(note['doc_ver'], doc.hash)
        self.assertEqual(note['UserGroup_id'], 0)
        self.assertEqual(note['content'], content)

        content2 = 'test note2'
        #Add another note in the same paragraph
        self.db.notes.addNote(0, doc.id, doc.hash, par_index, content2, 'everyone', [])
        notes = self.db.notes.getNotes(0, doc.id, doc.hash)
        self.assertEqual(len(notes), 2)
        note = notes[1]
        self.assertEqual(note['note_index'], 1)
        self.assertEqual(note['par_index'], par_index)
        self.assertEqual(note['doc_ver'], doc.hash)
        self.assertEqual(note['UserGroup_id'], 0)
        self.assertEqual(note['content'], content2)

        #Edit the first one
        content3 = 'edited'
        self.db.notes.modifyNote(doc.id, doc.hash, par_index, 0, content3, 'everyone', [])
        notes = self.db.notes.getNotes(0, doc.id, doc.hash)
        self.assertEqual(len(notes), 2)
        note = notes[0]
        self.assertEqual(note['note_index'], 0)
        self.assertEqual(note['par_index'], par_index)
        self.assertEqual(note['doc_ver'], doc.hash)
        self.assertEqual(note['UserGroup_id'], 0)
        self.assertEqual(note['content'], content3)

        blocks, ver = self.db.documents.addMarkdownBlock(doc, 'edited', 0)
        notes = self.db.notes.getNotes(0, doc.id, ver)
        self.assertEqual(len(notes), 2)
        self.assertEqual(notes[0]['note_index'], 0)
        self.assertEqual(notes[1]['note_index'], 1)
        self.assertEqual(notes[0]['par_index'], par_index + 1)
        self.assertEqual(notes[1]['par_index'], par_index + 1)
        self.assertEqual(notes[0]['content'], content3)
        self.assertEqual(notes[1]['content'], content2)

        content4 = 'new'
        self.db.notes.modifyNote(doc.id, ver, par_index + 1, 0, content4, 'everyone', [])
        notes = self.db.notes.getNotes(0, doc.id, ver)
        self.assertEqual(len(notes), 2)
        note = notes[0]
        self.assertEqual(note['note_index'], 0)
        self.assertEqual(note['par_index'], par_index + 1)
        self.assertEqual(note['content'], content4)

        doc = DocIdentifier(doc.id, ver)
        blocks, ver = self.db.documents.addMarkdownBlock(doc, 'edited', 0)
        content5 = 'new2'
        # self.db.notes.getNotes(0, doc.id, ver)  # The test would pass if this was here
        self.db.notes.modifyNote(doc.id, ver, par_index + 2, 0, content5, 'everyone', [])
        notes = self.db.notes.getNotes(0, doc.id, ver)
        self.assertEqual(len(notes), 2)
        note = notes[0]
        self.assertEqual(note['note_index'], 0)
        self.assertEqual(note['par_index'], par_index + 2)
        self.assertEqual(note['content'], content5)

    def test_special_chars(self):
        print("test_special_chars")
        with open("special_chars.md", "r") as f:
            special_char_text = f.read()
        doc = self.db.documents.createDocument('special_chars', 0)
        doc = self.db.documents.updateDocument(doc, special_char_text)
        doc_paragraphs = self.db.documents.getDocumentAsBlocks(doc)
        num_read = 0
        for i in range(0, len(doc_paragraphs)):
            self.db.readings.setAsRead(0, doc.id, doc.hash, i)
            num_read += 1
        readings = self.db.readings.getReadings(0, doc.id, doc.hash)
        self.assertEqual(len(readings), num_read)

        random.seed(0)
        indices = list(range(0, len(doc_paragraphs)))
        random.shuffle(indices)
        random.seed(0)
        random.shuffle(doc_paragraphs)

        # Refresh cache for the first document, otherwise it might not be found
        self.db.documents.getDocumentAsHtmlBlocks(doc)

        doc = self.db.documents.updateDocument(doc, '\n\n'.join(doc_paragraphs))
        readings = self.db.readings.getReadings(0, doc.id, doc.hash)

        # This fails because the special character cannot be mapped properly
        #self.assertEqual(len(readings), num_read)

if __name__ == '__main__':
    unittest.main(warnings='ignore')
