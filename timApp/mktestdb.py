import os
import shutil
import random

from timdb.timdb2 import TimDb
import ephemeralclient
from timdb.gitclient import initRepo
from timdb.timdbbase import TimDbException, DocIdentifier
from testhelper import print_times, random_str, random_word, random_sentence, random_paragraph
from datetime import datetime

class mktestdb:
    def __init__(self, new_version):
        TEST_FILES_PATH = 'test_files'
        if os.path.exists(TEST_FILES_PATH):
            shutil.rmtree(TEST_FILES_PATH, onerror=mktestdb.onerror)
        TEST_DB_NAME = 'test.db'
        if os.path.exists(TEST_DB_NAME):
            os.remove(TEST_DB_NAME)

        self.db = TimDb(TEST_DB_NAME, TEST_FILES_PATH)
        self.e = ephemeralclient.launch_ephemeral()
        self.db.initializeTables("schema2.sql")
        self.db.users.createAnonymousAndLoggedInUserGroups()
        initRepo(TEST_FILES_PATH)

        random.seed(0)
        self.new_version = new_version
        self.docs = None

    @classmethod
    def onerror(cls, func, path, exc_info):
        import stat
        if not os.access(path, os.W_OK):
            # Is the error an access error ?
            os.chmod(path, stat.S_IWUSR)
            func(path)
        else:
            assert False

    def mkusers(self, count):
        for i in range(0, count):
            name = random_word()
            realname = random_word() + ' ' + random_word()
            email = '{0}@{1}.{2}'.format(random_word(), random_word(), random_str(2, 3))
            user_id = self.db.users.createUser(name, realname, email, commit = False)
            group_id = self.db.users.createUserGroup(name, commit = False)
            self.db.users.addUserToGroup(group_id, user_id, commit = False)
        self.db.commit()

    def mkdocs(self, user_count, min_docs, max_docs, min_pars, max_pars):
        self.docs = None
        for user_id in range(0, user_count):
            for doc_index in range(0, random.randint(min_docs, max_docs)):
                doc = self.db.documents.createDocument(random_word(), user_id)
                pars = random.randint(min_pars, max_pars)
                for par_index in range(pars):
                    _, doc = self.db.documents.addMarkdownBlock(doc, random_paragraph(), par_index)

    def modify_docs(self, user_count, min_mods, max_mods):
        self.docs = None
        for user_id in range(0, user_count):
            docs = self.db.documents.getDocumentsForGroup(user_id)
            for docd in docs:
                doc = DocIdentifier(docd['id'], docd['versions'][0]['hash'])
                pars = len(self.db.documents.getDocumentAsBlocks(doc))
                for mod_index in range(0, random.randint(min_mods, max_mods)):
                    par_index = random.randint(0, pars)
                    mod_type = random.randint(0, 3)
                    if mod_type == 0:
                        # Insert
                        _, doc = self.db.documents.addMarkdownBlock(doc, random_paragraph(), par_index)
                        pars += 1
                    elif mod_type == 1:
                        # Delete
                        try:
                            doc = self.db.documents.deleteParagraph(doc, par_index)
                        except TimDbException:
                            #print("! Nothing to commit exception, par_index = " + str(par_index))
                            pass
                    else:
                        # Modify
                        _, doc = self.db.documents.modifyMarkDownBlock(doc, par_index, random_paragraph())

    def mkmeta(self, user_count, readcv, notecv):
        #print("Adding notes with {0}% coverage and read markings with {1}% coverage...".format(100 * notecv, 100 * readcv))

        for user_id in range(0, user_count):
            docs = self.db.documents.getDocumentsForGroup(user_id)
            for doc in docs:
                doc_id = doc['id']
                doc_ver = random.choice(doc['versions'])['hash']
                pars = len(self.db.documents.getDocumentAsBlocks(DocIdentifier(doc_id, doc_ver)))
                for par_index in range(0, pars):
                    if random.random() < readcv:
                        if self.new_version:
                            self.db.readings.setAsRead(user_id, doc_id, doc_ver, par_index, commit = False)
                        else:
                            pass # todo
                    if random.random() < notecv:
                        if self.new_version:
                            access = random.choice(['everyone', 'justme'])
                            tags = random.choice([[], ['difficult'], ['unclear'], ['difficult', 'unclear']])
                            self.db.notes.addNote(user_id, user_id, doc_id, doc_ver, par_index, random_sentence(), access, tags, commit=False)
                        else:
                            pass # todo
        self.db.commit()

    def modify_loop(self):
        readcv = 0.3
        notecv = 0.1
        min_mods = 0
        max_mods = 4
        self.mkmeta(user_count, readcv, notecv)
        self.modify_docs(user_count, min_mods, max_mods)

    def get_random_docid(self):
        if self.docs is None:
            self.docs = self.db.documents.getDocuments()
        doc_index = random.randint(0, len(self.docs))
        return self.docs[doc_index]['id']

    def get_doc_html(self, doc_id):
        timdb = self.db
        if not timdb.documents.documentExists(doc_id):
            print("! Document {0} does not exist!".format(doc_id))

        versions = timdb.documents.getDocumentVersions(doc_id)
        xs = timdb.documents.getDocumentAsHtmlBlocks(DocIdentifier(doc_id, versions[0]['hash']))
        doc = timdb.documents.getDocument(doc_id)
        #texts, jsPaths, cssPaths, modules = pluginControl.pluginify(xs, getCurrentUserName(), timdb.answers, doc_id, getCurrentUserId())
        #modules.append("ngSanitize")
        #modules.append("angularFileUpload")


if __name__ == '__main__':
    testdb = mktestdb(new_version=True)
    user_count = 50
    min_docs = 1
    max_docs = 10
    min_pars = 5
    max_pars = 20
    mod_rounds = 20
    doc_loads = 100

    print("Creating {0} users...".format(user_count))
    t0 = datetime.now()
    testdb.mkusers(user_count)
    time = datetime.now() - t0
    print("Completed in {0:.0f} milliseconds.\n".format(time.microseconds / 1000))

    print("Creating {0} to {1} documents for each user...".format(min_docs, max_docs))
    t0 = datetime.now()
    testdb.mkdocs(user_count, min_docs, max_docs, min_pars, max_pars)
    time = datetime.now() - t0
    print("Completed in {0:.0f} milliseconds.\n".format(time.microseconds / 1000))

    print("Simulating modifications in {0} iterations...".format(mod_rounds))
    times = []
    for i in range(0, mod_rounds):
        t0 = datetime.now()
        testdb.modify_loop()
        t = datetime.now() - t0
        times.append(t.microseconds / 1000)
    print_times(times)

    print("Simulating {0} document loads...".format(doc_loads))
    times = []
    for i in range(0, doc_loads):
        doc_id = testdb.get_random_docid()
        t0 = datetime.now()
        testdb.get_doc_html(doc_id)
        t = datetime.now() - t0
        times.append(t.microseconds / 1000)
    print_times(times)

