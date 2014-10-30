import os
import shutil
import string
import random

from timdb.timdb2 import TimDb
import ephemeralclient
from timdb.gitclient import initRepo
from timdb.timdbbase import TimDbException, DocIdentifier


def init():
    global e
    global db
    global new_version
    TEST_FILES_PATH = 'test_files'
    if os.path.exists(TEST_FILES_PATH):
        shutil.rmtree(TEST_FILES_PATH, onerror=onerror)
    TEST_DB_NAME = 'test.db'
    if os.path.exists(TEST_DB_NAME):
        os.remove(TEST_DB_NAME)

    db = TimDb(TEST_DB_NAME, TEST_FILES_PATH)
    e = ephemeralclient.launch_ephemeral()
    db.initializeTables("schema2.sql")
    db.users.createAnonymousUser()
    initRepo(TEST_FILES_PATH)

    random.seed(0)
    new_version = True

def onerror(func, path, exc_info):
    import stat
    if not os.access(path, os.W_OK):
        # Is the error an access error ?
        os.chmod(path, stat.S_IWUSR)
        func(path)
    else:
        assert False

def random_str(min_length, max_length):
    chars = string.ascii_lowercase
    length = random.randint(min_length, max_length)
    return ''.join(random.choice(chars) for _ in range(length))

def random_word():
    return random_str(2, 8)

def random_clause():
    words = random.randint(3, 20)
    return ' '.join(random_word() for _ in range(words))

def random_sentence():
    parts = random.randint(1, 4)
    return ', '.join(random_clause() for _ in range(parts))

def random_paragraph():
    sentences = random.randint(3, 6)
    return '. '.join(random_sentence() for _ in range(sentences)) + '.'

def mkusers(count):
    print("Creating {0} users...".format(count))
    for i in range(0, count):
        name = random_word()
        realname = random_word() + ' ' + random_word()
        email = '{0}@{1}.{2}'.format(random_word(), random_word(), random_str(2, 3))
        user_id = db.users.createUser(name, realname, email, commit = False)
        group_id = db.users.createUserGroup(name, commit = False)
        db.users.addUserToGroup(group_id, user_id, commit = False)
    db.commit()

def mkdocs(user_count, min_docs, max_docs, min_pars, max_pars):
    print("Creating {0} to {1} documents for each user...".format(min_docs, max_docs))

    for user_id in range(0, user_count):
        for doc_index in range(0, random.randint(min_docs, max_docs)):
            doc = db.documents.createDocument(random_word(), user_id)
            pars = random.randint(min_pars, max_pars)
            for par_index in range(pars):
                _, ver = db.documents.addMarkdownBlock(doc, random_paragraph(), par_index)
                doc = DocIdentifier(doc.id, ver)

def modify_docs(user_count, min_mods, max_mods):
    for user_id in range(0, user_count):
        docs = db.documents.getDocumentsForGroup(user_id)
        for docd in docs:
            doc = DocIdentifier(docd['id'], docd['versions'][0]['hash'])
            pars = len(db.documents.getDocumentAsBlocks(doc))
            for mod_index in range(0, random.randint(min_mods, max_mods)):
                par_index = random.randint(0, pars)
                mod_type = random.randint(0, 3)
                if mod_type == 0:
                    # Insert
                    _, ver = db.documents.addMarkdownBlock(doc, random_paragraph(), par_index)
                    pars += 1
                elif mod_type == 1:
                    # Delete
                    try:
                        ver = db.documents.deleteParagraph(doc, par_index)
                    except TimDbException:
                        print("! Nothing to commit exception, par_index = " + str(par_index))
                else:
                    # Modify
                    _, ver = db.documents.modifyMarkDownBlock(doc, par_index, random_paragraph())
                doc = DocIdentifier(doc.id, ver)

def mkmeta(user_count, readcv, notecv):
    print("Adding notes with {0}% coverage and read markings with {1}% coverage...".format(100 * notecv, 100 * readcv))

    for user_id in range(0, user_count):
        docs = db.documents.getDocumentsForGroup(user_id)
        for doc in docs:
            doc_id = doc['id']
            doc_ver = random.choice(doc['versions'])['hash']
            pars = len(db.documents.getDocumentAsBlocks(DocIdentifier(doc_id, doc_ver)))
            for par_index in range(0, pars):
                if random.random() < readcv:
                    if new_version:
                        db.readings.setAsRead(user_id, doc_id, doc_ver, par_index, commit = False)
                    else:
                        pass # todo
                if random.random() < notecv:
                    if new_version:
                        access = random.choice(['everyone', 'justme'])
                        tags = random.choice([[], ['difficult'], ['unclear'], ['difficult', 'unclear']])
                        db.notes.addNote(user_id, user_id, doc_id, doc_ver, par_index, random_sentence(), access, tags, commit=False)
                    else:
                        pass # todo
    db.commit()

if __name__ == '__main__':
    init()
    #user_count = 200
    user_count = 2
    min_docs = 1
    #max_docs = 10
    max_docs = 3
    min_pars = 2
    max_pars = 5
    readcv = 0.3
    notecv = 0.1

    mkusers(user_count)
    mkdocs(user_count, min_docs, max_docs, min_pars, max_pars)

    for i in range(1,3):
        mkmeta(user_count, readcv, notecv)
        modify_docs(user_count, 0, 4)
