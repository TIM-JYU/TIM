import sqlite3
from enum import Enum

BlockType = Enum('BlockType', 'DocumentBlock Comment Note Answer')


class TimDb(object):
    def __init__(self, db_path):
        self.db = sqlite3.connect(db_path)
        self.db.row_factory = sqlite3.Row
        
    def init(self, f):
        self.db.cursor().executescript(f.read())
        self.db.commit()
    
    def createUser(self, name):
        """Creates a new user with the specified name."""
        self.db.execute('insert into User (name) values (?)', [name])
        self.db.commit()
    
    def createDocument(self, name):
        """Creates a new document with the specified name."""
        self.db.execute('insert into Document (name) values (?)', [name])
        self.db.commit()
        #TODO: Create a file for the document in file system.
        #TODO: Put the document file under version control (using a Git module maybe?).
        return
    
    def addMarkDownBlock(self, document_id, content, previous_block_id):
        """Adds a new markdown block to the specified document."""
        self.db.execute('insert into Block (type_id) values (?)', [BlockType.DocumentBlock])
        block_id = self.db.cursor().rowcount
        print(block_id)
        self.db.commit()
        #TODO: Create a file for the block using its id as the file name.
        #TODO: Modify the document file appropriately.
        return
    
    def modifyMarkDownBlock(self, block_id, new_content):
        return
    
    def createDocumentFromBlocks(self, dir, document_name):
        """
        Creates a document from existing blocks in the specified directory.
        The blocks should be ordered alphabetically.
        """
        return