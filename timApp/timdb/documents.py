
from contracts import contract
from timdb.timdbbase import TimDbBase, TimDbException, blocktypes
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
    
    @contract
    def addMarkdownBlock(self, document_id : 'int', content : 'str', next_block_id : 'int|None') -> 'int':
        """Adds a new markdown block to the specified document.
        
        :param document_id: The id of the document.
        :param content: The content of the block.
        :param next_block_id: The id of the succeeding block.
           The value should be None if the block should be the last block of the document.
        :returns: The id of the newly added block.
        """

        document_path = self.getBlockPath(document_id)        
        assert os.path.exists(document_path), 'document does not exist: %r' % document_id       
        ec = EphemeralClient(EPHEMERAL_URL)
        success = ec.addBlock(document_id, next_block_id, content)
        
        #3. Check return value (success/fail).
        #4. Does Ephemeral save it to FS?
        return 0

    @contract
    def createDocument(self, name : 'str') -> 'int':
        """Creates a new document with the specified name.
        
        :param name: The name of the document to be created.
        :returns: The id of the newly created document.
        """
        # Usergroup id is 0 for now.
        cursor = self.db.cursor()
        cursor.execute('insert into Block (description, UserGroup_id, type_id) values (?,?,?)', [name, 0, blocktypes.DOCUMENT])
        document_id = cursor.lastrowid
        self.db.commit()
        
        document_path = os.path.join(self.blocks_path, str(document_id))
        
        try:
            with open(document_path, 'w', encoding='utf-8', newline='\n') as f:
                f.write('Edit me!')
        except OSError:
            print('Couldn\'t open file for writing:' + document_path)
            self.db.rollback()
            raise
        
        #TODO: Commit to Git
        #sha_hash = self.gitCommit(document_path, 'Created document: %s' % name, 'docker')
        #print(sha_hash)
        
        ec = EphemeralClient(EPHEMERAL_URL)
        ec.loadDocument(document_id, b'Edit me!')
        
        return document_id

    @contract
    def createDocumentFromBlocks(self, block_directory : 'str', document_name : 'str'):
        """
        Creates a document from existing blocks in the specified directory.
        The blocks should be ordered alphabetically.
        
        :param block_directory: The path to the directory containing the blocks.
        :param document_name: The name of the document to be created.
        """
        assert os.path.isdir(block_directory)
        blockfiles = [ f for f in os.listdir(block_directory) if os.path.isfile(os.path.join(block_directory, f)) ]
        
        blockfiles.sort()
        tmpfile = open("tmp.temp", "w", encoding='utf-8')
        for file in blockfiles:
            with open(os.path.join(block_directory, file), 'r', encoding='utf-8') as f:
                tmpfile.write(f.read())
                tmpfile.write('\n\n')
        self.importDocument('tmp.temp', document_name)  
        
    @contract
    def deleteParagraph(self, par_id : 'int', document_id : 'int'):
        """Deletes a paragraph from a document.
        
        :param document_id: The id of the document from which to delete the paragraph.
        :param par_id: The index of the paragraph in the document that should be deleted.
        """
        
        ec = EphemeralClient(EPHEMERAL_URL)
        success = ec.deleteBlock(document_id, par_id)
        
        #TODO: Check for errors.
        #TODO: Get the new document from Ephemeral and commit the change in VCS.
    
    @contract
    def documentExists(self, document_id : 'int') -> 'bool':
        """Checks whether a document with the specified id exists.
        
        :param document_id: The id of the document.
        :returns: True if the documents exists, false otherwise.
        """
        
        return self.blockExists(document_id, blocktypes.DOCUMENT)
    
    @contract
    def getDocument(self, document_id : 'int') -> 'row':
        """Gets the metadata information of the specified document.
        
        :param document_id: The id of the document to be retrieved.
        :returns: A row representing the document.
        """
        cursor = self.db.cursor()
        cursor.execute('select id, description as name from Block where id = ? and type_id = ?', [document_id, blocktypes.DOCUMENT])
        
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
    def getDocumentBlockIds(self, document_id : 'int') -> 'list(int)':
        """Gets the block ids of the specified document.
        
        :param document_id: The id of the document.
        :returns: A list of the block ids of the document.
        """
        document_path = self.getDocumentPath(document_id)
        
        assert os.path.exists(document_path), 'document does not exist: %d' % document_id
        
        #TODO: Get ids of the document from Ephemeral. If the ids are indexes, maybe only count is needed?
    
    @contract
    def getBlock(self, document_id : 'int', block_id : 'int') -> 'str':
        """Gets a block of a document.
        
        :param document_id: The id of the document.
        :param block_id: The id (index) of the block in the document.
        """
        
        ec = EphemeralClient(EPHEMERAL_URL)
        try:
            block = ec.getBlock(document_id, block_id)
        except EphemeralException as e:
            raise TimDbException(str(e))
        return block
    
    def getBlockAsHtml(self, document_id : 'int', block_id : 'int') -> 'str':
        """Gets a block of a document in HTML.
        
        :param document_id: The id of the document.
        :param block_id: The id (index) of the block in the document.
        """
        
        ec = EphemeralClient(EPHEMERAL_URL)
        try:
            block = ec.getBlockAsHtml(document_id, block_id)
        except EphemeralException as e:
            raise TimDbException(str(e))
        return block
    
    @contract
    def getDocumentBlocks(self, document_id : 'int') -> 'list(dict[2](str: str))':
        """Gets all the blocks of the specified document.
        
        :param document_id: The id of the document.
        :returns: The blocks of the document.
        """
        #TODO: Get blocks from Ephemeral.
        #TODO: Ephemeral doesn't support this (at least not as well as it could). Cannot know how many blocks there are!
        
        # So let's make a quick hack to fetch the blocks. This is VERY slow (on Windows at least); it fetches about one block per second when running locally on Windows machine.
        responseStr = None
        blocks = []
        notEnd = True
        blockIndex = 0
        ec = EphemeralClient(EPHEMERAL_URL)
        while notEnd:
            responseStr = ec.getBlock(document_id, blockIndex)
            notEnd = responseStr != '{"Error":"No block found"}'
            if notEnd:
                blocks.append({"par": str(blockIndex), "text" : responseStr})
            blockIndex += 1
        return blocks
        
    @contract
    def getDocumentAsHtmlBlocks(self, document_id : 'int') -> 'list(str)':
        """Gets the specified document in HTML form."""
        
        ec = EphemeralClient(EPHEMERAL_URL)
        
        try:
            blocks = ec.getDocumentAsHtmlBlocks(document_id)
        except NotInCacheException:
            #print('not in cache, checking if it exists')
            if self.documentExists(document_id):
                #print('it exists, loading to Ephemeral')
                with open(self.getBlockPath(document_id), 'rb') as f:
                    ec.loadDocument(document_id, f.read())
                #print('calling again Ephemeral')
                blocks = ec.getDocumentAsHtmlBlocks(document_id)
            else:
                raise TimDbException('The requested document was not found.')
        return blocks
    
    @contract
    def getDocumentPath(self, document_id : 'int') -> 'str':
        """Gets the path of the specified document.
        
        :param document_id: The id of the document.
        :returns: The path of the document.
        """
        return os.path.join(self.blocks_path, str(document_id))

    @contract
    def getDocumentVersions(self, document_id : 'int') -> 'list(dict(str:str))':
        """Gets the versions of a document.
        
        :param document_id: The id of the document whose versions will be fetched.
        :returns: A list of the versions of the document.
        """
        #TODO: Check that a document with this id exists.
        output, err = gitCommand(self.files_root_path, 'log --format=%H|%ad --date=relative '
                                 + os.path.relpath(self.getDocumentPath(document_id), self.files_root_path).replace('\\', '/'))
        lines = output.splitlines()
        versions = []
        for line in lines:
            pieces = line.split('|')
            versions.append({'hash' : pieces[0], 'timestamp' : pieces[1]})
        return versions
        
    @contract
    def importDocument(self, document_file : 'str', document_name : 'str') -> 'int':
        """Imports the specified document in the database."""
        
        # Assuming the document file is markdown-formatted, importing a document is very straightforward.
        doc_id = self.createDocument(document_name)
        copyfile(document_file, self.getDocumentPath(doc_id))
        
        with open(document_file, 'rb') as f:
            ec = EphemeralClient(EPHEMERAL_URL)
            ec.loadDocument(doc_id, f.read())
        
        gitCommit(self.files_root_path, self.getDocumentPath(doc_id), 'Imported document: %s' % document_name, 'docker')
        return doc_id
    
    @contract
    def modifyMarkDownBlock(self, document_id : 'int', block_id : 'int', new_content : 'str'):
        """Modifies the specified block.
        
        :param document_id: The id of the document.
        :param block_id: The id (relative to document) of the paragraph to be modified.
        :param new_content: The new content of the paragraph.
        """
        document_path = self.getDocumentPath(document_id)
        #print(document_path) 
        #TODO: This method needs the version id (hash) of the client's document to see if there's been another edit before this.
        
        assert os.path.exists(document_path), 'document does not exist: %r' % document_id
        
        ec = EphemeralClient(EPHEMERAL_URL)
        ec.modifyBlock(document_id, block_id, new_content)
        doc_content = ec.getDocumentFullText(document_id)
        
        with open(self.getDocumentPath(document_id), 'w', encoding='utf-8', newline='\n') as f:
            f.write(doc_content)
        
        gitCommit(self.files_root_path, document_path, 'Modified document with id: %d' % document_id, 'docker')
        
        #TODO: Check return value (success/fail). Currently Ephemeral doesn't return anything.
    
