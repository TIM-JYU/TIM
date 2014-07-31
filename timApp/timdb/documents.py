
from contracts import contract
from timdb.timdbbase import TimDbBase, TimDbException, blocktypes, DocIdentifier
import os
from ephemeralclient import EphemeralClient, EphemeralException, NotInCacheException
from shutil import copyfile
from .gitclient import gitCommit, gitCommand

EPHEMERAL_URL = 'http://localhost:8001'

class Documents(TimDbBase):

    @contract
    def __init__(self, db_path : 'Connection', files_root_path : 'str'):
        """Initializes TimDB with the specified database and root path.
        
        :param db_path: The path of the database file.
        :param files_root_path: The root path where all the files will be stored.
        """
        TimDbBase.__init__(self, db_path, files_root_path)
        self.ec = EphemeralClient(EPHEMERAL_URL)
    
    @contract
    def addMarkdownBlock(self, document_id : 'DocIdentifier', content : 'str', new_block_index : 'int') -> 'tuple(list(str),str)':
        """Adds a new markdown block to the specified document.
        
        :param document_id: The id of the document.
        :param content: The content of the block.
        :param new_block_index: The index of the new block.
        :returns: A list of the added blocks.
        """

        assert self.documentExists(document_id), 'document does not exist: %r' % document_id
        
        response = self.ec.addBlock(document_id, new_block_index, content)
        version = self.__handleModifyResponse(document_id, response, 'Added a paragraph at index %d' % (new_block_index))
        return response['paragraphs'], version

    @contract
    def __insertBlockToDb(self, name : 'str', owner_group_id : 'int', block_type : 'int') -> 'int':
        """Inserts a block to database.
        
        :param name: The name (description) of the block.
        :param owner_group_id: The owner group of the block.
        :param block_type: The type of the block.
        :returns: The id of the block.
        """
        
        cursor = self.db.cursor()
        cursor.execute('insert into Block (description, UserGroup_id, type_id) values (?,?,?)', [name, owner_group_id, block_type])
        block_id = cursor.lastrowid
        self.db.commit()
        return block_id
    
    @contract
    def createDocument(self, name : 'str', owner_group_id : 'int') -> 'DocIdentifier':
        """Creates a new document with the specified name.
        
        :param name: The name of the document to be created.
        :param owner_group_id: The id of the owner group.
        :returns: The id of the newly created document.
        """

        document_id = self.__insertBlockToDb(name, owner_group_id, blocktypes.DOCUMENT)
        
        document_path = os.path.join(self.blocks_path, str(document_id))
        
        try:
            with open(document_path, 'w', encoding='utf-8', newline='\n') as f:
                f.write('Edit me!')
        except OSError:
            print('Couldn\'t open file for writing:' + document_path)
            self.db.rollback()
            raise
        
        doc_hash = gitCommit(self.files_root_path, document_path, 'Created a new document: %s (id = %d)' % (name, document_id), 'docker')
        
        docId = DocIdentifier(document_id, doc_hash)
        
        self.ec.loadDocument(docId, b'Edit me!')
        
        return docId

    @contract
    def createDocumentFromBlocks(self, block_directory : 'str', document_name : 'str'):
        """
        Creates a document from existing blocks in the specified directory.
        The blocks should be ordered alphabetically.
        
        :param block_directory: The path to the directory containing the blocks.
        :param document_name: The name of the document to be created.
        """
        assert os.path.isdir(block_directory)
        blockfiles = [ int(f) for f in os.listdir(block_directory) if os.path.isfile(os.path.join(block_directory, f)) ]
        
        blockfiles.sort()
        with open("tmp.temp", "w", encoding='utf-8') as tmpfile:
            for file in blockfiles:
                print(file)
                with open(os.path.join(block_directory, str(file)), 'r', encoding='utf-8') as f:
                    tmpfile.write(f.read())
                    tmpfile.write('\n\n')
        self.importDocumentFromFile('tmp.temp', document_name, 0)
        
    @contract
    def deleteParagraph(self, document_id : 'DocIdentifier', par_id : 'int'):
        """Deletes a paragraph from a document.
        
        :param document_id: The id of the document from which to delete the paragraph.
        :param par_id: The index of the paragraph in the document that should be deleted.
        """
        
        response = self.ec.deleteBlock(document_id, par_id)
        version = self.__handleModifyResponse(document_id, response, 'Deleted a paragraph at index %d' % (par_id))
        return version
        
    @contract
    def documentExists(self, document_id : 'DocIdentifier') -> 'bool':
        """Checks whether a document with the specified id exists.
        
        :param document_id: The id of the document.
        :returns: True if the documents exists, false otherwise.
        """
        
        return self.blockExists(document_id.id, blocktypes.DOCUMENT)
    
    @contract
    def getDocument(self, document_id : 'DocIdentifier') -> 'row':
        """Gets the metadata information of the specified document.
        
        :param document_id: The id of the document to be retrieved.
        :returns: A row representing the document.
        """
        cursor = self.db.cursor()
        cursor.execute('select id, description as name from Block where id = ? and type_id = ?', [document_id.id, blocktypes.DOCUMENT])
        
        return cursor.fetchone()
    
    @contract
    def getDocuments(self) -> 'list(dict)':
        """Gets all the documents in the database.
        
        :returns: A list of dictionaries of the form {'id': <doc_id>, 'name': 'document_name'}
        """
        cursor = self.db.cursor()
        cursor.execute('select id,description as name from Block where type_id = ?', [blocktypes.DOCUMENT])
        results = self.resultAsDictionary(cursor)
        for result in results:
            result['versions'] = self.getDocumentVersions(result['id'])
        return results
    
    @contract
    def getDocumentsByIds(self, document_ids : 'list(int)') -> 'seq(row)':
        """Gets all the documents in the database."""
        cursor = self.db.cursor()
        cursor.execute('select id,description as name from Block where id in (%s)' % 
                           ','.join('?' * len(document_ids)), document_ids)
        return cursor.fetchall()
    
    @contract
    def getDocumentBlockIds(self, document_id : 'DocIdentifier') -> 'list(int)':
        """Gets the block ids of the specified document.
        
        :param document_id: The id of the document.
        :returns: A list of the block ids of the document.
        """
        document_path = self.getDocumentPath(document_id)
        
        assert os.path.exists(document_path), 'document does not exist: %d' % document_id
        
        #TODO: Get ids of the document from Ephemeral. If the ids are indexes, maybe only count is needed?
    
    @contract
    def getBlock(self, document_id : 'DocIdentifier', block_id : 'int') -> 'str':
        """Gets a block of a document.
        
        :param document_id: The id of the document.
        :param block_id: The id (index) of the block in the document.
        """
        
        try:
            block = self.ec.getBlock(document_id, block_id)
        except EphemeralException as e:
            raise TimDbException(str(e))
        return block
    
    def getBlockAsHtml(self, document_id : 'DocIdentifier', block_id : 'int') -> 'str':
        """Gets a block of a document in HTML.
        
        :param document_id: The id of the document.
        :param block_id: The id (index) of the block in the document.
        """
        
        try:
            block = self.ec.getBlockAsHtml(document_id, block_id)
        except EphemeralException as e:
            raise TimDbException(str(e))
        return block
    
    @contract
    def getDocumentBlocks(self, document_id : 'DocIdentifier') -> 'list(dict[2](str: str))':
        """Gets all the blocks of the specified document.
        
        :param document_id: The id of the document.
        :returns: The blocks of the document.
        """
        #TODO: Ephemeral doesn't support this (at least not as well as it could). Cannot know how many blocks there are!
        
        # So let's make a quick hack to fetch the blocks. This is VERY slow (on Windows at least); it fetches about one block per second when running locally on Windows machine.
        responseStr = None
        blocks = []
        notEnd = True
        blockIndex = 0
        
        while notEnd:
            responseStr = self.ec.getBlock(document_id, blockIndex)
            notEnd = responseStr != '{"Error":"No block found"}'
            if notEnd:
                blocks.append({"par": str(blockIndex), "text" : responseStr})
            blockIndex += 1
        return blocks
        
    @contract
    def getDocumentAsHtmlBlocks(self, document_id : 'DocIdentifier') -> 'list(str)':
        """Gets the specified document in HTML form."""
        
        try:
            blocks = self.ec.getDocumentAsHtmlBlocks(document_id)
        except NotInCacheException:
            if self.documentExists(document_id):
                with open(self.getBlockPath(document_id.id), 'rb') as f:
                    self.ec.loadDocument(document_id, f.read())
                blocks = self.ec.getDocumentAsHtmlBlocks(document_id)
            else:
                raise TimDbException('The requested document was not found.')
        return blocks
    
    @contract
    def getDocumentPath(self, document_id : 'DocIdentifier') -> 'str':
        """Gets the path of the specified document.
        
        :param document_id: The id of the document.
        :returns: The path of the document.
        """
        return os.path.join(self.blocks_path, str(document_id.id))
    
    @contract
    def getDocumentPathAsRelative(self, document_id : 'DocIdentifier'):
        return os.path.relpath(self.getDocumentPath(document_id), self.files_root_path).replace('\\', '/')
    
    @contract
    def getDocumentMarkdown(self, document_id : 'DocIdentifier') -> 'str':
        out, _ = gitCommand(self.files_root_path, 'show %s:%s' % (document_id.hash, self.getDocumentPathAsRelative(document_id)))
        return out
        
    @contract
    def getDocumentVersions(self, document_id : 'int') -> 'list(dict(str:str))':
        """Gets the versions of a document.
        
        :param document_id: The id of the document whose versions will be fetched.
        :returns: A list of the versions of the document.
        """
        
        docId = DocIdentifier(document_id, '')
        
        if not self.documentExists(docId):
            raise TimDbException('The specified document does not exist.')
        
        output, _ = gitCommand(self.files_root_path, 'log --format=%H|%ad --date=relative '
                                 + self.getDocumentPathAsRelative(docId))
        lines = output.splitlines()
        versions = []
        for line in lines:
            pieces = line.split('|')
            versions.append({'hash' : pieces[0], 'timestamp' : pieces[1]})
        return versions
    
    @contract
    def getNewestVersion(self, document_id : 'int') -> 'dict(str:str)':
        """Gets the hash of the newest version for a document.
        
        :param document_id: The id of the document.
        :returns: A dictionary describing the latest version of the document: {'timestamp': 'xxx', 'hash': 'xxx'}
        """
        return self.getDocumentVersions(document_id)[0]
        
    @contract
    def importDocumentFromFile(self, document_file : 'str', document_name : 'str', owner_group_id : 'int') -> 'DocIdentifier':
        """Imports the specified document in the database."""
        
        # Assuming the document file is markdown-formatted, importing a document is very straightforward.
        doc_id = DocIdentifier(self.__insertBlockToDb(document_name, owner_group_id, blocktypes.DOCUMENT), '')
        copyfile(document_file, self.getDocumentPath(doc_id))
        
        doc_hash = gitCommit(self.files_root_path, self.getDocumentPath(doc_id), 'Imported document: %s (id = %d)' % (document_name, doc_id.id), 'docker')
        docId = DocIdentifier(doc_id.id, doc_hash)
        
        with open(document_file, 'rb') as f:
            self.ec.loadDocument(docId, f.read())
        
        return docId
    
    def __commitDocumentChanges(self, document_id : 'DocIdentifier', doc_content : 'str', msg : 'str') -> 'str':
        """Commits the changes of the specified document to Git.
        
        :param document_id: The document identifier.
        :param msg: The commit message.
        :returns: The hash of the commit.
        """
        
        #TODO: Is there a better way to commit changes to Git? Is it necessary to commit the full document?
        #doc_content = self.ec.getDocumentFullText(document_id)
        
        with open(self.getDocumentPath(document_id), 'w', encoding='utf-8', newline='\n') as f:
            f.write(doc_content)
        
        return gitCommit(self.files_root_path, self.getDocumentPath(document_id), 'Document %d: %s' % (document_id.id, msg), 'docker')
    
    @contract
    def __updateNoteIndexes(self, old_document_id : 'DocIdentifier', new_document_id : 'DocIdentifier', map_all : 'bool' = False):
        """Updates the indexes for notes. This should be called after the document has been modified on Ephemeral
        but before the change is committed to Git.
        
        :param old_document_id: The id of the old document.
        :param new_document_id: The id of the new document.
        
        """
        
        cursor = self.db.cursor()
        cursor.execute('select parent_block_specifier,parent_block_id,Block_id,parent_block_revision_id from BlockRelation where parent_block_id = ?',
                       [new_document_id.id])
        notes = self.resultAsDictionary(cursor)
        
        if map_all:
            mapping = self.ec.getBlockMapping(new_document_id, old_document_id)
        else:
            mapping = []
            for note in notes:
                val = max(self.ec.getSingleBlockMapping(old_document_id, new_document_id, note['parent_block_specifier']), key=lambda x: x[0])
                mapping.append([note['parent_block_specifier'], val[1], val[0]])

        for note in notes:
            note['updated'] = False
        cursor.execute('delete from BlockRelation where parent_block_id = ?', [new_document_id.id])
        for par_map in mapping:
            for note in notes:
                if not note['updated'] and note['parent_block_specifier'] == par_map[0]:
                    note['parent_block_specifier'] = par_map[1]
                    note['updated'] = True
        
        for note in notes:
            cursor.execute('insert into BlockRelation (parent_block_specifier,parent_block_id,Block_id,parent_block_revision_id) values (?,?,?,?)',
                           [note['parent_block_specifier'], note['parent_block_id'], note['Block_id'], note['parent_block_revision_id']])
        self.db.commit()
    
    @contract
    def __handleModifyResponse(self, document_id : 'DocIdentifier', response : 'dict', message : 'str'):
        """Handles the response that comes from Ephemeral when modifying a document in some way.
        
        :param document_id: The id of the document that was modified.
        :param response: The response object from Ephemeral.
        :param message: The commit message.
        :returns: The version of the new document.
        """
        
        new_id = DocIdentifier(document_id.id, response['new_id'])
        self.ec.renameDocumentStr(response['new_id'], str(new_id))
        self.__updateNoteIndexes(document_id, new_id)
        new_content = self.ec.getDocumentFullText(new_id)
        version = self.__commitDocumentChanges(new_id, new_content, message)
        self.ec.renameDocument(new_id, DocIdentifier(new_id.id, version))
        return version
        
    @contract
    def modifyMarkDownBlock(self, document_id : 'DocIdentifier', block_id : 'int', new_content : 'str') -> 'tuple(list(str), str|None)':
        """Modifies the specified block.
        
        :param document_id: The id of the document.
        :param block_id: The id (relative to document) of the paragraph to be modified.
        :param new_content: The new content of the paragraph.
        :returns: The modified blocks and the version hash as a tuple.
        """
        
        assert self.documentExists(document_id), 'document does not exist: ' + document_id
        
        response = self.ec.modifyBlock(document_id, block_id, new_content)
        version = self.__handleModifyResponse(document_id, response, 'Modified a paragraph at index %d' % (block_id))
        blocks = response['paragraphs']
        return blocks, version
    
    @contract
    def updateDocument(self, document_id : 'DocIdentifier', new_content : 'bytes') -> 'DocIdentifier':
        """Updates a document.
        
        :param document_id: The id of the document to be updated.
        :param new_content: The new content of the document.
        :returns: The version of the new document.
        """
        
        assert self.documentExists(document_id), 'document does not exist: ' + document_id
        
        version = self.__commitDocumentChanges(document_id, str(new_content, encoding='utf-8'), "Modified as whole")
        new_id = DocIdentifier(document_id.id, version)
        self.ec.loadDocument(new_id, new_content)
        self.__updateNoteIndexes(document_id, new_id)
        return new_id
