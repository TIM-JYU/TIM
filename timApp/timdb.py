from enum import Enum
import fileinput
import os
from shutil import copyfile
import sqlite3
import sys

from contracts import contract, new_contract


#import timeit
BLOCKTYPE = Enum('BLOCKTYPE', 'DocumentBlock Comment Note Answer')
TABLE_NAMES = ['User',
               'UserGroup',
               'UserGroupMember',
               'BlockAccess',
               'DocumentAccess',
               'Document',
               'Block',
               'ReadRevision',
               'DocumentBlock']

new_contract('row', sqlite3.Row)

class TimDb(object):
    """Handles saving and retrieving information from TIM database."""

    @contract
    def __init__(self, db_path : 'str', files_root_path : 'str'):
        self.db = sqlite3.connect(db_path)
        self.db.row_factory = sqlite3.Row
        self.files_root_path = files_root_path
        
        #TODO: Make sure that db_path and files_root_path are valid!
        
        self.documents_path = os.path.join(files_root_path, 'documents')
        self.blocks_path = os.path.join(files_root_path, 'blocks')
        for path in [self.documents_path, self.blocks_path]:
            if not os.path.exists(path):
                os.makedirs(path)

    @contract
    def addBlockToDb(self, document_id : 'int'):
        """Adds a new block to the database with the specified document_id.
        Does NOT commit the transaction!
        
        :param document_id: The id of the document with which this block is
           associated.
        :returns: The id of the newly added block.
        """
        cursor = self.db.cursor()
        cursor.execute('insert into Block (type_id) values (?)', [BLOCKTYPE.DocumentBlock.value])
        block_id = cursor.lastrowid
        assert block_id is not None
        cursor.execute('insert into DocumentBlock (Document_id, Block_id) values (?,?)', [document_id, block_id])
        
        return block_id
    
    @contract
    def addMarkDownBlock(self, document_id : 'int', content : 'str', next_block_id : 'int|None') -> 'int':
        """Adds a new markdown block to the specified document.
        
        :param document_id: The id of the document.
        :param content: The content of the block.
        :param next_block_id: The id of the succeeding block.
           The value should be None if the block should be the last block of the document.
        :returns: The id of the newly added block.
        """
        block_id = self.addBlockToDb(document_id)
        self.db.commit()
        # TODO: Use path.join always instead of string concatenation.
        # Save all blocks in the same directory for now.
        # block_path = os.path.join(self.blocks_path, document_id + '/' + block_id)
        block_path = os.path.join(self.blocks_path, str(block_id))
        
        try:
            # The file shouldn't already exist, so we use the 'x' flag.
            with open(block_path, 'xt', encoding="utf-8") as blockfile:
                blockfile.write(content)
        except OSError:
            print('Couldn\'t create the file or file already exists:' + block_path)
            self.db.rollback()
            raise
        # TODO: Add the block under version control (Git module?).
        
        # Modify the document file appropriately.
        next_block_id_str = str(next_block_id)
        
        document_path = self.getDocumentPath(document_id)
        
        if next_block_id is None:
            with open(document_path, 'a') as docfile:
                docfile.write(str(block_id) + "\n")
            return block_id
        
        found = False
        for line in fileinput.input(document_path, inplace=1):
            if line == next_block_id_str + "\n":
                print(str(block_id))
                found = True
            sys.stdout.write(line)
        
        assert found
        
        return block_id
    
    def clear(self):
        """Clears the contents of all database tables."""
        for table in TABLE_NAMES:
            self.db.execute('delete from ' + table) #TABLE_NAMES is constant so no SQL injection possible

    def close(self):
        """Closes the database connection."""
        self.db.close()
    
    def create(self):
        """Initializes the database from the schema.sql file.
        NOTE: The database is emptied if it exists."""
        with open('schema.sql', 'r') as schema_file:
            self.db.cursor().executescript(schema_file.read())
        self.db.commit()
        

            
    @contract
    def createDocument(self, name : 'str') -> 'int':
        """Creates a new document with the specified name.
        
        :param name: The name of the document to be created.
        :returns: The id of the newly created document.
        """
        # Usergroup id is 0 for now.
        cursor = self.db.cursor()
        cursor.execute('insert into Document (name, UserGroup_id) values (?,?)', [name, 0])
        document_id = cursor.lastrowid
        self.db.commit()
        
        document_path = os.path.join(self.documents_path, str(document_id))
        
        try:
            # Create an empty file.
            open(document_path, 'a').close()
        except OSError:
            print('Couldn\'t open file for writing:' + document_path)
            self.db.rollback()
            raise
        
        # TODO: Put the document file under version control (using a Git module maybe?).
        return document_id

    @contract
    def createDocumentFromBlocks(self, block_directory : 'str', document_name : 'str'):
        """
        Creates a document from existing blocks in the specified directory.
        The blocks should be ordered alphabetically.
        
        :param block_directory: The path to the directory containing the blocks.
        :param document_name: The name of the document to be created.
        """
        document_id = self.createDocument(document_name)
        assert os.path.isdir(block_directory)
        blockfiles = [ f for f in os.listdir(block_directory) if os.path.isfile(os.path.join(block_directory, f)) ]
        
        # TODO: Is the blockfiles list automatically sorted or not?
        
        blocks = []
        for file in blockfiles:
            block_id = self.addBlockToDb(document_id)
            copyfile(os.path.join(block_directory, file), self.getBlockPath(block_id))
            blocks.append(block_id)
        self.db.commit()
        
        with open(self.getDocumentPath(document_id), 'wt') as document_file:
            for block in blocks:
                document_file.write("%s\n" % block)

    @contract
    def createUser(self, name : 'str') -> 'int':
        """Creates a new user with the specified name.
        
        :param name: The name of the user to be created.
        :returns: The id of the newly created user.
        """
        cursor = self.db.cursor()
        cursor.execute('insert into User (name) values (?)', [name])
        self.db.commit()
        user_id = cursor.lastrowid
        return user_id

    @contract
    def getBlockPath(self, block_id : 'int') -> 'str':
        """Gets the path of the specified block.
        
        :param block_id: The id of the block.
        :returns: The path of the block.
        """
        return os.path.join(self.blocks_path, str(block_id))
    
    @contract
    def getDocument(self, document_id : 'int') -> 'row':
        """Gets the metadata information of the specified document.
        
        :param document_id: The id of the document to be retrieved.
        :returns: A row representing the document.
        """
        cursor = self.db.cursor()
        cursor.execute('select * from Document where id = ?', [document_id])
        return cursor.fetchone()
    
    @contract
    def getDocuments(self) -> 'seq(row)':
        """Gets all the documents in the database."""
        cursor = self.db.cursor()
        cursor.execute('select * from Document')
        return cursor.fetchall()
    
    @contract
    def getDocumentsByIds(self, document_ids : 'list(int)') -> 'seq(row)':
        """Gets all the documents in the database."""
        cursor = self.db.cursor()
        cursor.execute('select * from Document where id in (?)', document_ids)
        return cursor.fetchall()
    
    @contract
    def getDocumentBlockIds(self, document_id : 'int') -> 'list(int)':
        """Gets the block ids of the specified document.
        
        :param document_id: The id of the document.
        :returns: A list of the block ids of the document.
        """
        document_path = self.getDocumentPath(document_id)
        
        with open(document_path) as f:
            return [int(line) for line in f.readlines()]
    
    @contract
    def getDocumentBlocks(self, document_id : 'int') -> 'list(dict[2](str: str))':
        """Gets all the blocks of the specified document.
        
        :param document_id: The id of the document.
        :returns: The blocks of the document.
        """
        block_ids = self.getDocumentBlockIds(document_id)
        
        blocks = []
        for block_id in block_ids:
            with open(self.getBlockPath(block_id), encoding="utf-8") as f:
                blocks.append({"par": str(block_id), "text": f.read()}) #TODO: par doesn't have to be str... but atm frontend expects it to be str
        return blocks
    
    @contract
    def getDocumentPath(self, document_id : 'int') -> 'str':
        """Gets the path of the specified document.
        
        :param document_id: The id of the document.
        :returns: The path of the document.
        """
        return os.path.join(self.documents_path, str(document_id))

    @contract
    def getUser(self, user_id : 'int') -> 'row':
        """Gets the user with the specified id."""
        
        cursor = self.db.cursor()
        cursor.execute('select * from User where id = ?', [user_id])
        return cursor.fetchone()
    
    @contract
    def modifyMarkDownBlock(self, block_id : 'int', new_content : 'str'):
        """Modifies the specified block.
        
        :param block_id: The id of the block to be modified.
        :param new_content: The new content of the block.
        """
        block_path = self.getBlockPath(block_id)
        
        try:
            with open(block_path, 'wt', encoding="utf-8") as blockfile:
                blockfile.write(new_content)
                blockfile.truncate()
        except:
            print('Couldn\'t modify block file:' + block_path)
            raise
        
        # TODO: Commit changes in version control and update fields in database.
        