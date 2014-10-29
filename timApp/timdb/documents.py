from contracts import contract
from timdb.timdbbase import TimDbBase, TimDbException, blocktypes, DocIdentifier
import os
from ephemeralclient import EphemeralClient, EphemeralException, EPHEMERAL_URL
from shutil import copyfile
from .gitclient import gitCommit, gitCommand
from timdb.gitclient import NothingToCommitException
import ansiconv


class Documents(TimDbBase):
    @contract
    def __init__(self, db_path: 'Connection', files_root_path: 'str', type_name: 'str', current_user_name: 'str'):
        """Initializes TimDB with the specified database and root path.
        
        :param db_path: The path of the database file.
        :param files_root_path: The root path where all the files will be stored.
        """
        TimDbBase.__init__(self, db_path, files_root_path, type_name, current_user_name)
        self.ec = EphemeralClient(EPHEMERAL_URL)

    @contract
    def addMarkdownBlock(self, document_id: 'DocIdentifier', content: 'str',
                         new_block_index: 'int') -> 'tuple(list(str),str)':
        """Adds a new markdown block to the specified document.
        
        :param document_id: The id of the document.
        :param content: The content of the block.
        :param new_block_index: The index of the new block.
        :returns: A list of the added blocks.
        """

        assert self.documentExists(document_id), 'document does not exist: %r' % document_id
        self.ensureCached(document_id)
        response = self.ec.addBlock(document_id, new_block_index, content)
        version = self.__handleModifyResponse(document_id,
                                              response,
                                              'Added a paragraph at index %d' % (new_block_index),
                                              new_block_index,
                                              len(response['paragraphs']))
        return response['paragraphs'], version

    @contract
    def __insertBlockToDb(self, name: 'str', owner_group_id: 'int', block_type: 'int') -> 'int':
        """Inserts a block to database.
        
        :param name: The name (description) of the block.
        :param owner_group_id: The owner group of the block.
        :param block_type: The type of the block.
        :returns: The id of the block.
        """

        cursor = self.db.cursor()
        cursor.execute('INSERT INTO Block (description, UserGroup_id, type_id) VALUES (?,?,?)',
                       [name, owner_group_id, block_type])
        block_id = cursor.lastrowid
        self.db.commit()
        return block_id

    @contract
    def createDocument(self, name: 'str', owner_group_id: 'int') -> 'DocIdentifier':
        """Creates a new document with the specified name.
        
        :param name: The name of the document to be created.
        :param owner_group_id: The id of the owner group.
        :returns: The id of the newly created document.
        """

        if '\0' in name:
            raise TimDbException('Document name cannot contain null characters.')

        document_id = self.__insertBlockToDb(name, owner_group_id, blocktypes.DOCUMENT)

        document_path = os.path.join(self.blocks_path, str(document_id))

        try:
            self.writeUtf8('Edit me!', document_path)
        except OSError:
            print('Couldn\'t open file for writing:' + document_path)
            self.db.rollback()
            raise

        doc_hash = gitCommit(self.files_root_path, document_path,
                             'Created a new document: %s (id = %d)' % (name, document_id), 'docker')

        docId = DocIdentifier(document_id, doc_hash)

        self.ec.loadDocument(docId, b'Edit me!')

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
    def deleteDocument(self, document_id: 'DocIdentifier'):
        """Deletes the specified document.
        
        :param document_id: The id of the document to be deleted.
        """

        assert self.documentExists(document_id), 'document does not exist: %d' % document_id.id

        cursor = self.db.cursor()
        cursor.execute('DELETE FROM Block WHERE type_id = ? AND id = ?', [blocktypes.DOCUMENT, document_id.id])
        cursor.execute('DELETE FROM ParMappings where doc_id = ?', [document_id.id])
        cursor.execute('DELETE FROM ReadParagraphs where doc_id = ?', [document_id.id])
        cursor.execute('DELETE FROM UserNotes where doc_id = ?', [document_id.id])
        self.db.commit()

        os.remove(self.getDocumentPath(document_id))

        gitCommit(self.files_root_path, self.getDocumentPath(document_id), 'Deleted document %d.' % document_id.id,
                  'docker')

    @contract
    def deleteParagraph(self, document_id: 'DocIdentifier', par_id: 'int'):
        """Deletes a paragraph from a document.
        
        :param document_id: The id of the document from which to delete the paragraph.
        :param par_id: The index of the paragraph in the document that should be deleted.
        """

        self.ensureCached(document_id)
        response = self.ec.deleteBlock(document_id, par_id)
        version = self.__handleModifyResponse(document_id,
                                              response,
                                              'Deleted a paragraph at index %d' % (par_id),
                                              par_id,
                                              -1)
        return version

    @contract
    def documentExists(self, document_id: 'DocIdentifier') -> 'bool':
        """Checks whether a document with the specified id exists.
        
        :param document_id: The id of the document.
        :returns: True if the documents exists, false otherwise.
        """

        return self.blockExists(document_id.id, blocktypes.DOCUMENT)

    @contract
    def getDocument(self, document_id: 'DocIdentifier') -> 'dict':
        """Gets the metadata information of the specified document.
        
        :param document_id: The id of the document to be retrieved.
        :returns: A row representing the document.
        """
        cursor = self.db.cursor()
        cursor.execute('SELECT id, description AS name FROM Block WHERE id = ? AND type_id = ?',
                       [document_id.id, blocktypes.DOCUMENT])

        return self.resultAsDictionary(cursor)[0]

    @contract
    def getDocuments(self, historylimit: 'int'=100) -> 'list(dict)':
        """Gets all the documents in the database.
        
        :returns: A list of dictionaries of the form {'id': <doc_id>, 'name': 'document_name'}
        """
        cursor = self.db.cursor()
        cursor.execute('SELECT id,description AS name FROM Block WHERE type_id = ?', [blocktypes.DOCUMENT])
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
        document_path = self.getDocumentPath(document_id)

        assert os.path.exists(document_path), 'document does not exist: %d' % document_id

        # TODO: Get ids of the document from Ephemeral. If the ids are indexes, maybe only count is needed?

    @contract
    def getBlock(self, document_id: 'DocIdentifier', block_id: 'int') -> 'str':
        """Gets a block of a document.
        
        :param document_id: The id of the document.
        :param block_id: The id (index) of the block in the document.
        """

        return self.ephemeralCall(document_id, self.ec.getBlock, block_id)

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

        return self.ephemeralCall(document_id, self.ec.getDocumentAsBlocks)

    @contract
    def getDocumentAsHtmlBlocks(self, document_id: 'DocIdentifier') -> 'list(str)':
        """Gets the specified document in HTML form."""

        return self.ephemeralCall(document_id, self.ec.getDocumentAsHtmlBlocks)

    def ephemeralCall(self, document_id: 'DocIdentifier', ephemeral_function, *args):
        """Calls a function of EphemeralClient, ensuring that the document is in cache.

        :param args: Required arguments for the function.
        :param ephemeral_function: The function to call.
        :param document_id: The id of the document.
        """

        try:
            result = ephemeral_function(document_id, *args)
        except EphemeralException:
            if self.documentExists(document_id):
                with open(self.getBlockPath(document_id.id), 'rb') as f:
                    self.ec.loadDocument(document_id, f.read())
                result = ephemeral_function(document_id, *args)
            else:
                raise TimDbException('The requested document was not found.')
        return result

    @contract
    def getDocumentPath(self, document_id: 'DocIdentifier') -> 'str':
        """Gets the path of the specified document.
        
        :param document_id: The id of the document.
        :returns: The path of the document.
        """
        return self.getBlockPath(document_id.id)

    @contract
    def getDocumentPathAsRelative(self, document_id: 'DocIdentifier'):
        return os.path.relpath(self.getDocumentPath(document_id), self.files_root_path).replace('\\', '/')

    @contract
    def getDocumentMarkdown(self, document_id: 'DocIdentifier') -> 'str':
        try:
            out, _ = gitCommand(self.files_root_path,
                                'show %s:%s' % (document_id.hash, self.getDocumentPathAsRelative(document_id)))
        except TimDbException as e:
            e.message = 'The requested revision was not found.'
            raise
        return out

    def getDifferenceToPrevious(self, document_id: 'DocIdentifier') -> 'str':
        try:
            out, _ = gitCommand(self.files_root_path, 'diff --color --unified=5 {}^! {}'.format(document_id.hash,
                                                                                                self.getDocumentPathAsRelative(
                                                                                                    document_id)))
        except TimDbException as e:
            e.message = 'The requested revision was not found.'
            raise
        css = ansiconv.base_css()
        html = ansiconv.to_html(out)
        return """
<html>
  <head><style>{0}</style></head>
  <body>
    <pre class="ansi_fore ansi_back">{1}</pre>
  </body>
</html>
""".format(css, html)

    @contract
    def getDocumentVersions(self, document_id: 'int', limit: 'int'=100) -> 'list(dict(str:str))':
        """Gets the versions of a document.
        
        :param document_id: The id of the document whose versions will be fetched.
        :returns: A list of the versions of the document.
        """

        docId = DocIdentifier(document_id, '')

        if not self.documentExists(docId):
            raise TimDbException('The specified document does not exist.')

        output, _ = gitCommand(self.files_root_path, 'log --format=%H|%ad|%an|%s --date=relative  -n {} '.format(limit)
                               + self.getDocumentPathAsRelative(docId))
        lines = output.splitlines()
        versions = []
        for line in lines:
            pieces = line.split('|')
            versions.append({'hash': pieces[0], 'timestamp': pieces[1], 'user': pieces[2], 'message': pieces[3]})
        return versions

    @contract
    def getNewestVersion(self, document_id: 'int') -> 'dict(str:str)':
        """Gets the hash of the newest version for a document.
        
        :param document_id: The id of the document.
        :returns: A dictionary describing the latest version of the document: {'timestamp': 'xxx', 'hash': 'xxx'}
        """
        return self.getDocumentVersions(document_id)[0]

    @contract
    def importDocumentFromFile(self, document_file: 'str', document_name: 'str',
                               owner_group_id: 'int') -> 'DocIdentifier':
        """Imports the specified document in the database."""

        # Assuming the document file is markdown-formatted, importing a document is very straightforward.
        doc_id = DocIdentifier(self.__insertBlockToDb(document_name, owner_group_id, blocktypes.DOCUMENT), '')
        copyfile(document_file, self.getDocumentPath(doc_id))

        doc_hash = gitCommit(self.files_root_path, self.getDocumentPath(doc_id),
                             'Imported document: %s (id = %d)' % (document_name, doc_id.id), 'docker')
        docId = DocIdentifier(doc_id.id, doc_hash)

        with open(document_file, 'rb') as f:
            self.ec.loadDocument(docId, f.read())

        return docId

    @contract
    def importDocument(self, content: 'str', document_name: 'str', owner_group_id: 'int'):
        doc_id = DocIdentifier(self.__insertBlockToDb(document_name, owner_group_id, blocktypes.DOCUMENT), '')
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

        self.writeUtf8(doc_content, self.getDocumentPath(document_id))
        return gitCommit(self.files_root_path, self.getDocumentPath(document_id),
                         'Document %d: %s' % (document_id.id, msg), self.current_user_name)

    @contract
    def ensureCached(self, document_id: 'DocIdentifier'):
        self.getDocumentAsBlocks(document_id)

    @contract
    def addEmptyParMapping(self, document_id : 'DocIdentifier', paragraph_index : 'int'):
        cursor = self.db.cursor()

        cursor.execute(
            """
            select par_index from ParMappings
            where doc_id = ? and doc_ver = ? and par_index = ?
            """,
            [document_id.id, document_id.hash, paragraph_index])

        if cursor.fetchone() is not None:
            print("Mapping already exists - document {0} version {1} paragraph {2}".format(
            document_id.id, document_id.hash[:6], paragraph_index))
            return

        print("addEmptyParMapping(doc {0}, version {1}, paragraph {2})".format(
            document_id.id, document_id.hash[:6], paragraph_index))

        cursor.execute(
            """
            insert into ParMappings (doc_id, doc_ver, par_index) values (?, ?, ?)
            """,
            [document_id.id, document_id.hash, paragraph_index])

        self.db.commit()

    def __copyParMappings(self, old_document_id : 'DocIdentifier', new_document_id : 'DocIdentifier', start_index : 'int' = 0, end_index : 'int' = -1, offset : 'int' = 0):
        end_str = str(end_index) if end_index >= 0 else 'end';
        end2_str = str(end_index + offset) if end_index >= 0 else 'end + {0}'.format(offset);

        print("copyParMapping(doc {0}) : ver {1} : [{2}, {3}[ -> ver {4} : [{5}, {6}[".format(
            old_document_id.id,
            old_document_id.hash[:6], start_index, end_str,
            new_document_id.hash[:6], start_index + offset, end2_str
        ))

        cursor = self.db.cursor()

        if end_index < 0:
            cursor.execute(
                """
                select par_index from ParMappings
                where doc_id = ? and doc_ver = ? and par_index >= ?
                """,
                [old_document_id.id, old_document_id.hash, start_index])
        else:
            cursor.execute(
                """
                select par_index from ParMappings
                where doc_id = ? and doc_ver = ? and par_index >= ? and par_index < ?
                """,
                [old_document_id.id, old_document_id.hash, start_index, end_index])

        old_pars = self.resultAsDictionary(cursor)

        for p in old_pars:
            index = int(p['par_index'])
            new_index = index + offset
            print("{0} par {1} -> {2} par {3}".format(old_document_id.hash[:6], index, new_document_id.hash[:6], new_index))
            cursor.execute(
                """
                update ParMappings set new_ver = ?, new_index = ?, modified = 'False'
                where doc_id = ? and doc_ver = ? and par_index = ?
                """,
                [new_document_id.hash, new_index, old_document_id.id, old_document_id.hash, index])
            cursor.execute(
                """
                insert into ParMappings (doc_id, doc_ver, par_index, new_ver, new_index, modified)
                values (?, ?, ?, NULL, NULL, NULL)
                """,
                [old_document_id.id, new_document_id.hash, new_index])

        self.db.commit()

    @contract
    def __updateParMappings(self, old_document_id : 'DocIdentifier', new_document_id : 'DocIdentifier', start_index : 'int' = 0):
        print("updateParMapping(doc {0}) : ver {1} -> ver {2}".format(old_document_id.id, old_document_id.hash[:6], new_document_id.hash[:6]))

        cursor = self.db.cursor()
        cursor.execute(
            """
            select par_index from ParMappings
            where doc_id = ? and doc_ver = ? and par_index >= ?
            order by par_index
            """,
            [old_document_id.id, old_document_id.hash, start_index])
        old_pars = self.resultAsDictionary(cursor)

        #par_count = len(self.getDocumentAsBlocks(new_document_id))
        #print("par_count = " + str(par_count))

        mappings = []
        invmap = {}
        removemaps = []
        for p in old_pars:
            affinities = self.ec.getSingleBlockMapping(old_document_id, new_document_id, p['par_index'])
            [affinity, newindex] = max(affinities, key=lambda x: x[0])

            if newindex in invmap:
                # There is an existing mapping for the same index in the new document
                prevmap = mappings[invmap[newindex]]
                if affinity > prevmap[2]:
                    # This one is a better match
                    removemaps.append(invmap[newindex])
                else:
                    # This one is a bad match, do not add
                    continue

            mappings.append([p['par_index'], newindex, affinity])
            invmap[newindex] = len(mappings) - 1

        # Remove mappings for bad matches
        for i in range(len(removemaps) - 1, -1, -1):
            del mappings[i]

        #print(mappings)

        for m in mappings:
            print("{0} par {1} -> {2} par {3} (affinity {4})".format(old_document_id.hash[:6], m[0], new_document_id.hash[:6], m[1], m[2]))

            cursor.execute(
                """
                update ParMappings set new_ver = ?, new_index = ?, modified = ?
                where doc_id = ? and doc_ver = ? and par_index = ?""",
                [new_document_id.hash, m[1], str(m[2] < 1), old_document_id.id, old_document_id.hash, m[0]])
            cursor.execute(
                """
                insert into ParMappings (doc_id, doc_ver, par_index, new_ver, new_index, modified)
                values (?, ?, ?, NULL, NULL, NULL)
                """,
                [old_document_id.id, new_document_id.hash, m[1]])


        # mapindex = 0
        # for i in range(0, par_count):
        #     cursor.execute(
        #         'insert into ParMappings (doc_id, doc_ver, par_index, new_ver, new_index, modified) values (?, ?, ?, NULL, NULL, NULL)',
        #         [old_document_id.id, new_document_id.hash, i])
        #     print("Paragraph %d, mapIndex = %d" % (i, mapindex))
        #
        #     if mapindex < len(mappings) and i >= mappings[mapindex][0]:
        #         m = mappings[mapindex]
        #         if i == m[0]:
        #             # Existing mapping
        #             print("Paragraph %s: maxAffinity = %s, to old paragraph %s" % (str(m[1]), str(m[2]), str(m[0])))
        #             cursor.execute(
        #                 """update ParMappings set new_ver = ?, new_index = ?, modified = ?
        #                    where doc_id = ? and doc_ver = ? and par_index = ?""",
        #             [new_document_id.hash, m[1], str(m[2] < 1), old_document_id.id, old_document_id.hash, m[0]])
        #         mapindex += 1

        self.db.commit()

    @contract
    def __updateParMapping(self, old_document_id : 'DocIdentifier', new_document_id : 'DocIdentifier', par_index : 'int', new_index : 'int' = -1):
        if new_index == -1:
            new_index = par_index

        print("updateParMapping(doc {0}) : ver {1} par {2} -> ver {3} par {4}".format(old_document_id.id, old_document_id.hash[:6], par_index, new_document_id.hash[:6]), new_index)

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
        affinity = affinities[new_index]

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
            [old_document_id.id, new_document_id.hash, par_index])

        self.db.commit()

    @contract
    def __deleteParMapping(self, document_id : 'DocIdentifier', par_index : 'int'):
        print("deleteParMapping(doc {0}:{1}, par {2})".format(document_id.id, document_id.hash[:6], par_index))

        cursor = self.db.cursor()
        cursor.execute(
            """
            select par_index from ParMappings
            where doc_id = ? and doc_ver = ? and par_index = ?
            """,
            [document_id.id, document_id.hash, par_index])

        if cursor.fetchone() is None:
            print("Mapping does not exist.")
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
                               mod_count: 'int'):
        """Handles the response that comes from Ephemeral when modifying a document in some way.
        
        :param document_id: The id of the document that was modified.
        :param response: The response object from Ephemeral.
        :param message: The commit message.
        :returns: The version of the new document.
        """

        new_id = DocIdentifier(document_id.id, response['new_id'])
        self.ec.renameDocumentStr(response['new_id'], str(new_id))
        new_content = self.ec.getDocumentFullText(new_id)

        try:
            version = self.__commitDocumentChanges(new_id, new_content, message)
        except NothingToCommitException:
            return document_id.hash
        self.ec.renameDocument(new_id, DocIdentifier(new_id.id, version))
        #self.__updateParMappings(document_id, DocIdentifier(new_id.id, version))

        new_id = DocIdentifier(new_id.id, version)
        self.__copyParMappings(document_id, new_id, start_index = 0, end_index = mod_index)

        if message[:3] == 'Add':
            # Added new paragraphs
            print("__handleModifyResponse({0}): adding {1} paragraph(s) to index {2}".format(document_id.id, mod_count, mod_index))
            self.__copyParMappings(
                document_id, new_id,
                start_index = mod_index,
                offset = mod_count
            )
        elif mod_count > 0:
            # Modified existing paragraphs
            print("__handleModifyResponse({0}): modifying {1} paragraph at index {2}".format(document_id.id, mod_count, mod_index))
            for i in range(mod_index, mod_index + mod_count):
                self.__updateParMapping(document_id, new_id, i)
            self.__copyParMappings(document_id, new_id, start_index = mod_index + mod_count)
        elif mod_count < 0:
            # Removed paragraphs
            print("__handleModifyResponse({0}): removing {1} paragraph(s) from index {2}".format(document_id.id, mod_count, mod_index))
            for i in range(mod_index, mod_index - mod_count):
                self.__deleteParMapping(document_id, i)
            self.__updateParMappings(document_id, new_id, start_index = mod_index)

        return version

    @contract
    def modifyMarkDownBlock(self, document_id: 'DocIdentifier', block_id: 'int',
                            new_content: 'str') -> 'tuple(list(str), str|None)':
        """Modifies the specified block.
        
        :param document_id: The id of the document.
        :param block_id: The id (relative to document) of the paragraph to be modified.
        :param new_content: The new content of the paragraph.
        :returns: The modified blocks and the version hash as a tuple.
        """

        assert self.documentExists(document_id), 'document does not exist: ' + document_id
        self.ensureCached(document_id)
        response = self.ec.modifyBlock(document_id, block_id, new_content)

        version = self.__handleModifyResponse(document_id,
                                              response,
                                              'Modified a paragraph at index %d' % (block_id),
                                              block_id,
                                              len(response['paragraphs']) - 1)
        blocks = response['paragraphs']
        return blocks, version

    @contract
    def renameDocument(self, document_id: 'DocIdentifier', new_name: 'str'):
        """Renames a document.
        
        :param document_id: The id of the document to be renamed.
        :param new_name: The new name for the document.
        """

        assert self.documentExists(document_id), 'document does not exist: ' + document_id

        cursor = self.db.cursor()
        cursor.execute('UPDATE Block SET description = ? WHERE type_id = ? AND id = ?',
                       [new_name, blocktypes.DOCUMENT, document_id.id])
        self.db.commit()

    @contract
    def updateDocument(self, document_id: 'DocIdentifier', new_content: 'str') -> 'DocIdentifier':
        """Updates a document.
        
        :param document_id: The id of the document to be updated.
        :param new_content: The new content of the document.
        :returns: The version of the new document.
        """

        assert self.documentExists(document_id), 'document does not exist: ' + document_id

        try:
            version = self.__commitDocumentChanges(document_id, new_content, "Modified as whole")
        except NothingToCommitException:
            return document_id
        new_id = DocIdentifier(document_id.id, version)
        self.ec.loadDocument(new_id, new_content.encode('utf-8'))
        self.__updateParMappings(document_id, new_id)
        return new_id
