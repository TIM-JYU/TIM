import os
from shutil import copyfile
from datetime import datetime

from contracts import contract
from htmlSanitize import sanitize_html

from timdb.timdbbase import TimDbBase, TimDbException, blocktypes
from timdb.docidentifier import DocIdentifier
from ephemeralclient import EphemeralClient, EphemeralException, EPHEMERAL_URL
from timdb.gitclient import NothingToCommitException, GitClient
from utils import date_to_relative
from ansi2html import Ansi2HTMLConverter


class Documents(TimDbBase):
    def __repr__(self):
        """For caching - we consider two Documents collections to be the same if their
        files_root_paths are equal."""
        return self.files_root_path

    @contract
    def __init__(self, db_path: 'Connection', files_root_path: 'str', type_name: 'str', current_user_name: 'str'):
        """Initializes TimDB with the specified database and root path.
        
        :param type_name: The type name.
        :param current_user_name: The name of the current user.
        :param db_path: The path of the database file.
        :param files_root_path: The root path where all the files will be stored.
        """
        TimDbBase.__init__(self, db_path, files_root_path, type_name, current_user_name)
        self.ec = EphemeralClient(EPHEMERAL_URL)
        self.git = GitClient.connect(files_root_path)

    @contract
    def addMarkdownBlock(self, document_id: 'DocIdentifier', content: 'str',
                         new_block_index: 'int') -> 'tuple(list(str),DocIdentifier)':
        """Adds a new markdown block to the specified document.
        
        :param document_id: The id of the document.
        :param content: The content of the block.
        :param new_block_index: The index of the new block.
        :returns: A list of the added blocks.
        """

        assert self.documentExists(document_id.id), 'document does not exist: %r' % document_id
        self.ensureCached(document_id)
        content = self.trimDoc(content)
        response = self.ec.addBlock(document_id, new_block_index, content)
        new_doc = self.__handleModifyResponse(document_id,
                                              response,
                                              'Added a paragraph at index %d' % (new_block_index),
                                              new_block_index,
                                              len(response['paragraphs']))
        return response['paragraphs'], new_doc

    @contract
    def createDocument(self, name: 'str', owner_group_id: 'int') -> 'DocIdentifier':
        """Creates a new document with the specified name.
        
        :param name: The name of the document to be created.
        :param owner_group_id: The id of the owner group.
        :returns: The id of the newly created document.
        """

        if '\0' in name:
            raise TimDbException('Document name cannot contain null characters.')

        document_id = self.insertBlockToDb(name, owner_group_id, blocktypes.DOCUMENT)
        document_path = os.path.join(self.blocks_path, str(document_id))

        try:
            self.writeUtf8('Edit me!', document_path)
        except OSError:
            print('Couldn\'t open file for writing:' + document_path)
            self.db.rollback()
            raise

        rel_block_path = os.path.relpath(self.blocks_path, self.files_root_path)
        rel_document_path = os.path.join(rel_block_path, str(document_id))
        self.git.add(rel_document_path)
        doc_hash = self.git.commit('Created a new document: {} (id = {})'.format(name, document_id))

        docId = DocIdentifier(document_id, doc_hash)

        self.ec.loadDocument(docId, b'Click right side to edit. You can get help with editing from editors Help tab.')

        cursor = self.db.cursor()
        cursor.execute("""UPDATE Block SET created = CURRENT_TIMESTAMP, modified = CURRENT_TIMESTAMP
                          WHERE type_id = ? and id = ?""",
                       [blocktypes.DOCUMENT, docId.id])
        self.db.commit()
        self.updateLatestRevision(docId)
        return docId

    @contract
    def createDocumentFromBlocks(self, block_directory: 'str', document_name: 'str'):
        """
        Creates a document from existing blocks in the specified directory.
        The blocks should be ordered alphabetically.
        
        :param block_directory: The path to the directory containing the blocks.
        :param document_name: The name of the document to be created.
        """
        assert os.path.isdir(block_directory)
        blockfiles = [int(f) for f in os.listdir(block_directory) if os.path.isfile(os.path.join(block_directory, f))]

        blockfiles.sort()
        with open("tmp.temp", "w", encoding='utf-8') as tmpfile:
            for file in blockfiles:
                print(file)
                with open(os.path.join(block_directory, str(file)), 'r', encoding='utf-8') as f:
                    tmpfile.write(f.read())
                    tmpfile.write('\n\n')
        self.importDocumentFromFile('tmp.temp', document_name, 0)

    @contract
    def deleteDocument(self, document_id: 'int'):
        """Deletes the specified document.
        
        :param document_id: The id of the document to be deleted.
        """

        assert self.documentExists(document_id), 'document does not exist: %d' % document_id

        cursor = self.db.cursor()
        cursor.execute('DELETE FROM Block WHERE type_id = ? AND id = ?', [blocktypes.DOCUMENT, document_id])
        cursor.execute('DELETE FROM ParMappings where doc_id = ?', [document_id])
        cursor.execute('DELETE FROM ReadParagraphs where doc_id = ?', [document_id])
        cursor.execute('DELETE FROM UserNotes where doc_id = ?', [document_id])
        self.db.commit()

        os.remove(self.getDocumentPath(document_id))

        self.git.rm(self.getDocumentPathAsRelative(document_id))
        self.git.commit('Deleted document {}.'.format(document_id))

    @contract
    def deleteParagraph(self, document_id: 'DocIdentifier', par_id: 'int') -> 'DocIdentifier':
        """Deletes a paragraph from a document.
        
        :param document_id: The id of the document from which to delete the paragraph.
        :param par_id: The index of the paragraph in the document that should be deleted.
        """

        self.ensureCached(document_id)
        response = self.ec.deleteBlock(document_id, par_id)
        new_doc = self.__handleModifyResponse(document_id,
                                              response,
                                              'Deleted a paragraph at index %d' % (par_id),
                                              par_id,
                                              -1)
        return new_doc

    @contract
    def documentExists(self, document_id: 'int') -> 'bool':
        """Checks whether a document with the specified id exists.
        
        :param document_id: The id of the document.
        :returns: True if the documents exists, false otherwise.
        """

        return self.blockExists(document_id, blocktypes.DOCUMENT)

    @contract
    def getDocumentId(self, document_name: 'str') -> 'int|None':
        """Gets the document's identifier by its name or None if not found.
        
        :param document_name: The name of the document.
        :returns: A row representing the document.
        """
        cursor = self.db.cursor()
        cursor.execute('SELECT id FROM Block WHERE description = ? AND type_id = ?',
                       [document_name, blocktypes.DOCUMENT])

        row = cursor.fetchone()
        return row[0] if row is not None else None

    @contract
    def getDocument(self, document_id: 'int') -> 'dict':
        """Gets the metadata information of the specified document.
        
        :param document_id: The id of the document to be retrieved.
        :returns: A row representing the document.
        """
        cursor = self.db.cursor()
        cursor.execute('SELECT id, description AS name FROM Block WHERE id = ? AND type_id = ?',
                       [document_id, blocktypes.DOCUMENT])

        return self.resultAsDictionary(cursor)[0]

    @contract
    def getDocuments(self, historylimit: 'int'=100) -> 'list(dict)':
        """Gets all the documents in the database.
        
        :returns: A list of dictionaries of the form {'id': <doc_id>, 'name': 'document_name'}
        """
        cursor = self.db.cursor()
        cursor.execute('SELECT id,description AS name,modified,latest_revision_id FROM Block WHERE type_id = ?', [blocktypes.DOCUMENT])
        results = self.resultAsDictionary(cursor)
        zombies = []
        for result in results:
            if not self.blockExists(result['id'], blocktypes.DOCUMENT):
                print('getDocuments: document {} does not exist on the disk!'.format(result['id']))
                zombies.append(result)
                continue
            else:
                result['versions'] = self.getDocumentVersions(result['id'], limit=historylimit)
            if result['modified'] is None or result['latest_revision_id'] is None:
                latest_version = self.getDocumentVersions(result['id'], limit=1, date_format='iso')
                time_str = latest_version[0]['timestamp'].rsplit(' ', 1)[0]
                cursor.execute("""UPDATE Block SET modified = strftime("%Y-%m-%d %H:%M:%S", ?)
                                  WHERE type_id = ? and id = ?""", [time_str, blocktypes.DOCUMENT, result['id']])
                result['modified'] = time_str
                if result['latest_revision_id'] is None:
                    cursor.execute("INSERT INTO ReadRevision (Block_id, Hash) VALUES (?, ?)",
                        [result['id'], latest_version[0]['hash']])
                    cursor.execute("UPDATE Block SET latest_revision_id = ? WHERE type_id = ? and id = ?",
                        [cursor.lastrowid, blocktypes.DOCUMENT, result['id']])
            result['modified'] = date_to_relative(datetime.strptime(result['modified'], "%Y-%m-%d %H:%M:%S"))
        self.db.commit()
        for zombie in zombies:
            results.remove(zombie)

        return results

    @contract
    def updateLatestRevision(self, doc_id: 'DocIdentifier', commit: 'bool'=True):
        cursor = self.db.cursor()
        cursor.execute('SELECT latest_revision_id FROM Block WHERE id = ?', [doc_id.id])
        revid = cursor.fetchone()[0]
        if revid is None:
            cursor.execute("INSERT INTO ReadRevision (Block_id, Hash) VALUES (?, ?)",
                [doc_id.id, doc_id.hash])
            cursor.execute("UPDATE Block SET latest_revision_id = ? WHERE type_id = ? and id = ?",
                [cursor.lastrowid, blocktypes.DOCUMENT, doc_id.id])
        else:
            cursor.execute("UPDATE ReadRevision SET Hash = ? WHERE revision_id = ?",
                [doc_id.hash, revid])

        if commit:
            self.db.commit()

    @contract
    def getDocumentsForGroup(self, group_id : 'int', historylimit: 'int'=100) -> 'list(dict)':
        """Gets all the documents owned by a group.

        :returns: A list of dictionaries of the form {'id': <doc_id>, 'name': 'document_name'}
        """
        cursor = self.db.cursor()
        cursor.execute('SELECT id,description AS name FROM Block WHERE type_id = ? AND UserGroup_id = ?', [blocktypes.DOCUMENT, group_id])
        results = self.resultAsDictionary(cursor)
        for result in results:
            result['versions'] = self.getDocumentVersions(result['id'], limit=historylimit)
        return results

    @contract
    def getDocumentsByIds(self, document_ids: 'list(int)') -> 'seq(row)':
        """Gets all the documents in the database."""
        cursor = self.db.cursor()
        cursor.execute('select id,description as name from Block where id in (%s)' %
                       ','.join('?' * len(document_ids)), document_ids)
        return cursor.fetchall()

    @contract
    def getDocumentBlockIds(self, document_id: 'DocIdentifier') -> 'list(int)':
        """Gets the block ids of the specified document.
        
        :param document_id: The id of the document.
        :returns: A list of the block ids of the document.
        """
        document_path = self.getDocumentPath(document_id.id)

        assert os.path.exists(document_path), 'document does not exist: %d' % document_id

        # TODO: Get ids of the document from Ephemeral. If the ids are indexes, maybe only count is needed?

    @contract
    def getBlock(self, document_id: 'DocIdentifier', block_id: 'int') -> 'str':
        """Gets a block of a document.
        
        :param document_id: The id of the document.
        :param block_id: The id (index) of the block in the document.
        """

        content = self.ephemeralCall(document_id, self.ec.getBlock, block_id)
        return self.trimDoc(content)

    def getBlockAsHtml(self, document_id: 'DocIdentifier', block_id: 'int') -> 'str':
        """Gets a block of a document in HTML.
        
        :param document_id: The id of the document.
        :param block_id: The id (index) of the block in the document.
        """

        return self.ephemeralCall(document_id, self.ec.getBlockAsHtml, block_id)

    @contract
    def getDocumentAsBlocks(self, document_id: 'DocIdentifier') -> 'list(str)':
        """Gets all the blocks of the specified document in markdown format.
        
        :param document_id: The id of the document.
        :returns: The blocks of the document in markdown format.
        """

        return [self.trimDoc(block) for block in self.ephemeralCall(document_id, self.ec.getDocumentAsBlocks)]

    @contract
    def getDocumentAsHtmlBlocks(self, document_id: 'DocIdentifier') -> 'list(str)':
        """Gets the specified document in HTML form.

        :param document_id: The id of the document.
        :returns: The document contents as a list of HTML blocks.
        """

        return self.ephemeralCall(document_id, self.ec.getDocumentAsHtmlBlocks)

    def getDocumentAsHtmlBlocksSanitized(self, document_id: 'DocIdentifier') -> 'list(str)':
        """Gets the specified document in HTML form sanitized.

        :param document_id: The id of the document.
        :returns: The document contents as a list of sanitized HTML blocks.
        """
        return [sanitize_html(block) for block in self.getDocumentAsHtmlBlocks(document_id)]

    @contract
    def getIndex(self, document_id: 'DocIdentifier') -> 'list(str)':
        return [block for block in self.getDocumentAsBlocks(document_id) if len(block) > 0 and block[0] == '#']

    @contract
    def ephemeralCall(self, document_id: 'DocIdentifier', ephemeral_function, *args):
        """Calls a function of EphemeralClient, ensuring that the document is in cache.

        :param args: Required arguments for the function.
        :param ephemeral_function: The function to call.
        :param document_id: The id of the document.
        """

        try:
            result = ephemeral_function(document_id, *args)
        except EphemeralException:
            if self.documentExists(document_id.id):
                with open(self.getBlockPath(document_id.id), 'rb') as f:
                    self.ec.loadDocument(document_id, f.read())
                result = ephemeral_function(document_id, *args)
            else:
                raise TimDbException('The requested document was not found.')
        return result

    @contract
    def getDocumentPath(self, document_id: 'int') -> 'str':
        """Gets the path of the specified document.
        
        :param document_id: The id of the document.
        :returns: The path of the document.
        """
        return self.getBlockPath(document_id)

    @contract
    def getDocumentPathAsRelative(self, document_id: 'int'):
        return os.path.relpath(self.getDocumentPath(document_id), self.files_root_path).replace('\\', '/')

    @contract
    def getDocumentMarkdown(self, document_id: 'DocIdentifier') -> 'str':
        content = self.git.get_contents(document_id.hash, self.getDocumentPathAsRelative(document_id.id))
        return self.trimDoc(content)

    @contract
    def getDifferenceToPrevious(self, document_id: 'DocIdentifier') -> 'str':
        try:
            out, _ = self.git.command('diff --word-diff=color --unified=5 {}^! {}'.format(document_id.hash,
                                                                                self.getDocumentPathAsRelative(
                                                                                    document_id.id)))
        except TimDbException as e:
            e.message = 'The requested revision was not found.'
            raise
        conv = Ansi2HTMLConverter(inline=True, dark_bg=False)
        html = conv.convert(out, full=False)
        return html

    @contract
    def getDocumentVersions(self, document_id: 'int', limit: 'int'=100, date_format: 'str'='relative') -> 'list(dict(str:str))':
        """Gets the versions of a document.
        
        :param date_format: The date format.
        :param limit: Maximum number of versions to get.
        :param document_id: The id of the document whose versions will be fetched.
        :returns: A list of the versions of the document.
        """

        if limit <= 0:
            return []

        if not self.documentExists(document_id):
            raise TimDbException('The specified document does not exist.')

        file_path = self.getDocumentPathAsRelative(document_id)
        output, _ = self.git.command('log --format=%H|%ad|%an|%s --date={} -n {} {}'.format(date_format, limit, file_path))
        lines = output.splitlines()
        versions = []
        for line in lines:
            pieces = line.split('|')
            versions.append({'hash': pieces[0], 'timestamp': pieces[1], 'user': pieces[2], 'message': pieces[3]})
        return versions

    @contract
    def getNewestVersion(self, document_id: 'int') -> 'dict(str:str)|None':
        """Gets the hash of the newest version for a document.

        :param document_id: The id of the document.
        :returns: A version dictionary, or none if not found.
        """
        return self.git.getLatestVersionDetails(self.getDocumentPathAsRelative(document_id))

    @contract
    def getNewestVersionHash(self, doc_id: 'int') -> 'str|None':
        """Gets the hash of the newest version for a document.
        
        :param document_id: The id of the document.
        :returns: The hash string, or None if not found.
        """
        cursor = self.db.cursor()
        cursor.execute("SELECT latest_revision_id FROM Block WHERE id = ?", [doc_id])
        revid = cursor.fetchone()
        if revid is None:
            doc_ver = self.git.getLatestVersion(self.getDocumentPathAsRelative(doc_id))
            self.updateLatestRevision(DocIdentifier(doc_id, doc_ver))
            return doc_ver
        else:
            cursor.execute("SELECT Hash FROM ReadRevision WHERE revision_id = ?", [revid[0]])
            return cursor.fetchone()[0]

    @contract
    def importDocumentFromFile(self, document_file: 'str', document_name: 'str',
                               owner_group_id: 'int') -> 'DocIdentifier':
        """Imports the specified document in the database.

        :param document_file: The file path of the document to import.
        :param document_name: The name for the document.
        :param owner_group_id: The owner group of the document.
        :returns: The id of the imported document.
        """

        # Assuming the document file is markdown-formatted, importing a document is very straightforward.
        doc_id = DocIdentifier(self.insertBlockToDb(document_name, owner_group_id, blocktypes.DOCUMENT), '')
        copyfile(document_file, self.getDocumentPath(doc_id.id))

        self.git.add(self.getDocumentPathAsRelative(doc_id.id))
        doc_hash = self.git.commit('Imported document: {} (id = {})'.format(document_name, doc_id.id))
        docId = DocIdentifier(doc_id.id, doc_hash)

        with open(document_file, 'rb') as f:
            self.ec.loadDocument(docId, f.read())

        return docId

    @contract
    def importDocument(self, content: 'str', document_name: 'str', owner_group_id: 'int') -> 'DocIdentifier':
        doc_id = DocIdentifier(self.insertBlockToDb(document_name, owner_group_id, blocktypes.DOCUMENT), '')
        doc_hash = self.__commitDocumentChanges(doc_id, content,
                                                'Imported document: %s (id = %d)' % (document_name, doc_id.id))
        doc_id = DocIdentifier(doc_id.id, doc_hash)
        self.ec.loadDocument(doc_id, content.encode('utf-8'))
        return doc_id

    def __commitDocumentChanges(self, document_id: 'DocIdentifier', doc_content: 'str', msg: 'str') -> 'str':
        """Commits the changes of the specified document to Git.
        
        :param document_id: The document identifier.
        :param msg: The commit message.
        :returns: The hash of the commit.
        """

        self.writeUtf8(doc_content, self.getDocumentPath(document_id.id))
        self.git.add(self.getDocumentPathAsRelative(document_id.id))
        return self.git.commit('Document {}: {}'.format(document_id.id, msg), author=self.current_user_name)

    @contract
    def ensureCached(self, document_id: 'DocIdentifier'):
        self.getDocumentAsBlocks(document_id)

    def getParMappings(self, doc_id : 'DocIdentifier', start_index = 0, end_index = -1) -> 'list(tuple)':
        cursor = self.db.cursor()
        endclause = " and par_index < {}".format(end_index) if end_index >= 0 else ""
        cursor.execute(
            "select par_index, new_index from ParMappings where doc_id = ? and doc_ver = ? and par_index >= ?" + endclause,
            [doc_id.id, doc_id.hash, start_index])
        return cursor.fetchall()

    def addParMapping(self, old_document_id : 'DocIdentifier', new_document_id : 'DocIdentifier', par_index : 'int', new_index : 'int', modified=False, commit=True):
        cursor = self.db.cursor()
        cursor.execute(
            """
            update ParMappings set new_ver = ?, new_index = ?, modified = ?
            where doc_id = ? and doc_ver = ? and par_index = ?
            """,
            [new_document_id.hash, new_index, modified, old_document_id.id, old_document_id.hash, par_index])
        cursor.execute(
            """
            insert into ParMappings (doc_id, doc_ver, par_index, new_ver, new_index, modified)
            values (?, ?, ?, NULL, NULL, NULL)
            """,
            [old_document_id.id, new_document_id.hash, new_index])

        if commit:
            self.db.commit()

    def __copyParMappings(self, old_document_id : 'DocIdentifier', new_document_id : 'DocIdentifier', start_index : 'int' = 0, end_index : 'int' = -1, offset : 'int' = 0, commit: 'bool'=True):
        for m in self.getParMappings(old_document_id, start_index, end_index):
            par_index = int(m[0])
            new_index = par_index + offset
            #print("{0} par {1} -> {2} par {3}".format(old_document_id.hash[:6], new_index, new_document_id.hash[:6], new_index))
            self.addParMapping(old_document_id, new_document_id, par_index, new_index, commit=False)
        if commit:
            self.db.commit()

    @contract
    def updateParMappings(self, old_document_id : 'DocIdentifier', new_document_id : 'DocIdentifier', start_index : 'int' = 0, commit: 'bool'=True):
        cursor = self.db.cursor()
        cursor.execute(
            """
            select par_index from ParMappings
            where doc_id = ? and doc_ver = ? and par_index >= ?
            order by par_index
            """,
            [old_document_id.id, old_document_id.hash, start_index])
        old_pars = self.resultAsDictionary(cursor)

        self.ensureCached(old_document_id)
        self.ensureCached(new_document_id)

        mappings = []
        invmap = {}
        removemaps = []
        for p in old_pars:
            old_index = int(p['par_index'])
            affinities = self.ec.getSingleBlockMapping(old_document_id, new_document_id, old_index)
            [affinity, new_index] = max(affinities, key=lambda x: x[0] if x[0] is not None else 0)

            if affinity < 0.5:
                # This is most likely a deleted paragraph
                continue

            if new_index in invmap and invmap[new_index] in mappings:
                # There is an existing mapping for the same index in the new document
                prevmap = mappings[invmap[new_index]]
                if affinity > prevmap[2]:
                    # This one is a better match
                    print('Taking back {} to {}'.format(invmap[new_index], new_index))
                    removemaps.append(invmap[new_index])
                else:
                    # This one is a bad match, do not add
                    continue

            mappings.append([old_index, new_index, affinity])
            invmap[new_index] = old_index

        # Remove mappings for bad matches
        for i in range(len(removemaps) - 1, -1, -1):
            del mappings[i]

        for m in mappings:
            [old_index, new_index, affinity] = m
            #print("{0} par {1} -> {2} par {3} (affinity {4})".format(old_document_id.hash[:6], old_index, new_document_id.hash[:6], new_index, affinity))

            cursor.execute(
                """
                update ParMappings set new_ver = ?, new_index = ?, modified = ?
                where doc_id = ? and doc_ver = ? and par_index = ?""",
                [new_document_id.hash, new_index, str(affinity < 1), old_document_id.id, old_document_id.hash, old_index])

            # We use "or ignore" in case two old paragraphs have been mapped to the same new one
            cursor.execute(
                """
                insert or ignore into ParMappings (doc_id, doc_ver, par_index, new_ver, new_index, modified)
                values (?, ?, ?, NULL, NULL, NULL)
                """,
                [old_document_id.id, new_document_id.hash, new_index])

        if commit:
            self.db.commit()

    @contract
    def __updateParMapping(self, old_document_id : 'DocIdentifier', new_document_id : 'DocIdentifier', par_index : 'int', new_index : 'int' = -1, commit: 'bool'=True):
        if new_index == -1:
            new_index = par_index

        #print("updateParMapping(doc {0}) : ver {1} par {2} -> ver {3} par {4}".format(old_document_id.id, old_document_id.hash[:6], par_index, new_document_id.hash[:6], new_index))

        cursor = self.db.cursor()
        cursor.execute(
            """
            select par_index from ParMappings
            where doc_id = ? and doc_ver = ? and par_index = ?
            """,
            [old_document_id.id, old_document_id.hash, par_index])

        if cursor.fetchone() is None:
            # Nothing to update
            return

        # TODO: this could be optimized by getting only a 1:1 affinity instead of 1:n
        affinities = self.ec.getSingleBlockMapping(old_document_id, new_document_id, par_index)
        affinity = affinities[new_index][0]

        cursor.execute(
            """
            update ParMappings set new_ver = ?, new_index = ?, modified = ?
            where doc_id = ? and doc_ver = ? and par_index = ?
            """,
            [new_document_id.hash, new_index, str(affinity < 1),
             old_document_id.id, old_document_id.hash, par_index])

        cursor.execute(
            """
            insert into ParMappings (doc_id, doc_ver, par_index, new_ver, new_index, modified)
            values (?, ?, ?, NULL, NULL, NULL)
            """,
            [old_document_id.id, new_document_id.hash, new_index])

        if commit:
            self.db.commit()

    @contract
    def __deleteParMapping(self, document_id : 'DocIdentifier', par_index : 'int'):
        #print("deleteParMapping(doc {0}:{1}, par {2})".format(document_id.id, document_id.hash[:6], par_index))

        cursor = self.db.cursor()
        cursor.execute(
            """
            select par_index from ParMappings
            where doc_id = ? and doc_ver = ? and par_index = ?
            """,
            [document_id.id, document_id.hash, par_index])

        if cursor.fetchone() is None:
            #print("Mapping does not exist.")
            return

        cursor.execute(
            """delete from ParMappings
               where doc_id = ? and doc_ver = ? and par_index = ?""",
            [document_id.id, document_id.hash, par_index])
        self.db.commit()

    @contract
    def __handleModifyResponse(self, document_id: 'DocIdentifier',
                               response: 'dict',
                               message: 'str',
                               mod_index: 'int',
                               mod_count: 'int') -> 'DocIdentifier':
        """Handles the response that comes from Ephemeral when modifying a document in some way.

        :param mod_index: The index at which the document was modified.
        :param mod_count: The number of paragraphs that was added (positive) or removed (negative).
        :param document_id: The id of the document that was modified.
        :param response: The response object from Ephemeral.
        :param message: The commit message describing the change.
        :returns: The id of the new document.
        """

        new_id = DocIdentifier(document_id.id, response['new_id'])
        self.ec.renameDocumentStr(response['new_id'], str(new_id))
        new_content = self.ec.getDocumentFullText(new_id)

        try:
            version = self.__commitDocumentChanges(new_id, new_content, message)
        except NothingToCommitException:
            return document_id
        self.ec.renameDocument(new_id, DocIdentifier(new_id.id, version))

        new_id = DocIdentifier(new_id.id, version)
        self.updateLatestRevision(new_id)
        self.__copyParMappings(document_id, new_id, start_index = 0, end_index = mod_index, commit=False)

        if mod_count >= 0:
            # Add or modify
            #print("__handleModifyResponse({0}): adding {1} paragraph(s) to index {2}".format(document_id.id, mod_count, mod_index))
            if message[:3] == 'Add':
                # No modifications to the paragraph at mod_index
                self.__updateParMapping(document_id, new_id, mod_index, mod_index + mod_count, commit=False)
            else:
                # This is a modify request, but there may still be more paragraphs...
                self.__updateParMapping(document_id, new_id, mod_index, commit=False)

            self.__copyParMappings(
                document_id, new_id,
                start_index = mod_index + 1,
                offset = mod_count,
                commit=False
            )
        else:
            # Remove
            #print("__handleModifyResponse({0}): removing {1} paragraph(s) from index {2}".format(document_id.id, -mod_count, mod_index))
            #print("copyParMappings({}, {}, start_index = {}, offset = {})".format(document_id.hash[:6], new_id.hash[:6], mod_index - mod_count, mod_count))
            self.__copyParMappings(
                document_id, new_id,
                start_index = mod_index - mod_count,
                offset = mod_count,
                commit=False
            )

        self.updateDocModified(new_id, commit=False)
        self.db.commit()
        return new_id

    @contract
    def modifyMarkDownBlock(self, document_id: 'DocIdentifier', block_id: 'int',
                            new_content: 'str') -> 'tuple(list(str), DocIdentifier)':
        """Modifies the specified block.
        
        :param document_id: The id of the document.
        :param block_id: The id (relative to document) of the paragraph to be modified.
        :param new_content: The new content of the paragraph.
        :returns: The modified blocks and the new document id as a tuple.
        """

        assert self.documentExists(document_id.id), 'document does not exist: ' + str(document_id)
        self.ensureCached(document_id)
        new_content = self.trimDoc(new_content)
        response = self.ec.modifyBlock(document_id, block_id, new_content)

        new_doc = self.__handleModifyResponse(document_id,
                                              response,
                                              'Modified a paragraph at index %d' % (block_id),
                                              block_id,
                                              len(response['paragraphs']) - 1)
        blocks = response['paragraphs']
        return blocks, new_doc

    @contract
    def renameDocument(self, document_id: 'DocIdentifier', new_name: 'str') -> 'None':
        """Renames a document.
        
        :param document_id: The id of the document to be renamed.
        :param new_name: The new name for the document.
        """

        assert self.documentExists(document_id.id), 'document does not exist: ' + str(document_id)

        cursor = self.db.cursor()
        cursor.execute('UPDATE Block SET description = ? WHERE type_id = ? AND id = ?',
                       [new_name, blocktypes.DOCUMENT, document_id.id])
        self.db.commit()

    @contract
    def updateDocument(self, document_id: 'DocIdentifier', new_content: 'str') -> 'DocIdentifier':
        """Updates a document.
        
        :param document_id: The id of the document to be updated.
        :param new_content: The new content of the document.
        :returns: The id of the new document.
        """

        assert self.documentExists(document_id.id), 'document does not exist: ' + document_id

        new_content = self.trimDoc(new_content)
        try:
            version = self.__commitDocumentChanges(document_id, new_content, "Modified as whole")
        except NothingToCommitException:
            return document_id
        new_id = DocIdentifier(document_id.id, version)
        self.ec.loadDocument(new_id, new_content.encode('utf-8'))
        self.updateParMappings(document_id, new_id, commit=False)
        self.updateDocModified(new_id, commit=False)
        self.updateLatestRevision(new_id, commit=False)
        self.db.commit()
        return new_id

    def trimDoc(self, text: 'str'):
        """Trims the specified text. Don't trim spaces from left side because they may indicate a code block

        :param text: The text to be trimmed.
        :return: The trimmed text.
        """
        return text.rstrip().strip('\r\n')

    @contract
    def updateDocModified(self, doc_id: 'DocIdentifier', commit: 'bool'=True):
        cursor = self.db.cursor()
        cursor.execute('UPDATE Block SET modified = CURRENT_TIMESTAMP WHERE type_id = ? and id = ?',
                       [blocktypes.DOCUMENT, doc_id.id])
        if commit:
            self.db.commit()

    @contract
    def previewBlock(self, content: 'str') -> 'list(str)':
        """Previews the specified markdown in HTML

        :param content: The markdown to preview.
        :returns: A list of HTML blocks.
        """
        doc_id = DocIdentifier('temp', '')
        self.ec.loadDocument(doc_id, content.encode('utf-8'))
        return self.getDocumentAsHtmlBlocks(doc_id)
