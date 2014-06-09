import fileinput
import os
import sqlite3
import sys
import timeit
from enum import Enum
from shutil import copyfile

BlockType = Enum('BlockType', 'DocumentBlock Comment Note Answer')


class TimDb(object):
    def __init__(self, db_path, files_root_path):
        self.db = sqlite3.connect(db_path)
        self.db.row_factory = sqlite3.Row
        self.files_root_path = files_root_path
        
        self.documents_path = os.path.join(files_root_path, 'documents')
        self.blocks_path = os.path.join(files_root_path, 'blocks')
        for path in [self.documents_path, self.blocks_path]:
            if not os.path.exists(path):
                os.makedirs(path)
        
    def init(self, schema_file):
        with open(schema_file, 'r') as f:
            self.db.cursor().executescript(f.read())
        self.db.commit()
    
    def createUser(self, name):
        """Creates a new user with the specified name."""
        self.db.execute('insert into User (name) values (?)', [name])
        self.db.commit()
    
    def createDocument(self, name):
        """Creates a new document with the specified name."""
        #Usergroup id is 0 for now.
        c = self.db.cursor()
        c.execute('insert into Document (name, UserGroup_id) values (?,?)', [name, 0])
        document_id = c.lastrowid
        self.db.commit()
        
        document_path = os.path.join(self.documents_path, str(document_id))
        
        try:
            #Create an empty file.
            open(document_path, 'a').close()
        except OSError:
            print('Couldn\'t open file for writing:' + document_path)
            self.db.rollback()
            raise
        
        #TODO: Put the document file under version control (using a Git module maybe?).
        return document_id
    
    def addBlockToDb(self, document_id):
        """Adds a new block to the database with the specified document_id. Does NOT commit the transaction!"""
        c = self.db.cursor()
        c.execute('insert into Block (type_id) values (?)', [BlockType.DocumentBlock.value])
        block_id = c.lastrowid
        assert block_id is not None
        c.execute('insert into DocumentBlock (Document_id, Block_id) values (?,?)', [document_id, block_id])
        
        return block_id
    
    def addMarkDownBlock(self, document_id, content, next_block_id):
        """Adds a new markdown block to the specified document."""
        start = timeit.default_timer()
        block_id = self.addBlockToDb(document_id)
        self.db.commit()
        #TODO: Use path.join always instead of string concatenation.
        #Save all blocks in the same directory for now.
        #block_path = os.path.join(self.blocks_path, document_id + '/' + block_id)
        block_path = os.path.join(self.blocks_path, str(block_id))
        
        try:
            #The file shouldn't already exist, so we use the 'x' flag.
            with open(block_path, 'xt', encoding="utf-8") as blockfile:
                blockfile.write(content)
        except OSError:
            print('Couldn\'t create the file or file already exists:' + block_path)
            self.db.rollback()
            raise
        #TODO: Add the block under version control (Git module?).
        
        #Modify the document file appropriately.
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
        
        stop = timeit.default_timer()
        print(stop - start)
        
        return block_id
    
    def modifyMarkDownBlock(self, block_id, new_content):
        block_path = self.getBlockPath(block_id)
        
        try:
            with open(block_path, 'wt') as blockfile:
                blockfile.write(new_content)
                blockfile.truncate()
        except:
            print('Couldn\'t modify block file:' + block_path)
            raise
        
        #TODO: Commit changes in version control and update fields in database.
    
    def getDocument(self, document_id):
        """Gets the metadata information of the specified document."""
        c = self.db.cursor()
        c.execute('select * from Document where id = ?', document_id)
        return c.fetchone()
    
    def getDocumentBlockIds(self, document_id):
        """Gets the block ids of the specified document."""
        document_path = self.getDocumentPath(document_id)
        
        with open(document_path) as f:
            return [int(line) for line in f.readlines()]
    
    def createDocumentFromBlocks(self, block_directory, document_name):
        """
        Creates a document from existing blocks in the specified directory.
        The blocks should be ordered alphabetically.
        """
        document_id = self.createDocument(document_name)
        assert os.path.isdir(block_directory)
        blockfiles = [ f for f in os.listdir(block_directory) if os.path.isfile(os.path.join(block_directory, f)) ]
        
        #TODO: Is the blockfiles list automatically sorted or not?
        
        blocks = []
        for file in blockfiles:
            block_id = self.addBlockToDb(document_id)
            copyfile(os.path.join(block_directory, file), self.getBlockPath(block_id))
            blocks.append(block_id)
        self.db.commit()
        
        with open(self.getDocumentPath(document_id), 'wt') as document_file:
            for block in blocks:
                document_file.write("%s\n" % block)
    
    def getBlockPath(self, block_id):
        return os.path.join(self.blocks_path, str(block_id))
    
    def getDocumentPath(self, document_id):
        return os.path.join(self.documents_path, str(document_id))