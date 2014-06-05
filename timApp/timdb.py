import sqlite3
from enum import Enum

BlockType = Enum('BlockType', 'DocumentBlock Comment Note Answer')

class TimDb(object):
    '''
    classdocs
    '''

    def __init__(self, app, g):
        '''
        Constructor
        '''
        self.app = app
        self.g = g
        
    def init(self):
        with self.app.app_context():
            db = self.getDb()
            with self.app.open_resource('schema.sql', mode='r') as f:
                db.cursor().executescript(f.read())
            db.commit()

    def connect(self):
        """Connects to the specific database."""
        rv = sqlite3.connect(self.app.config['DATABASE'])
        rv.row_factory = sqlite3.Row
        return rv
    
    def getDb(self):
        """Opens a new database connection if there is none yet for the
        current application context.
        """
        if not hasattr(self.g, 'sqlite_db'):
            self.g.sqlite_db = connect()
        return self.g.sqlite_db
    
    def createUser(self, name):
        """Creates a new user with the specified name."""
        db = getDb()
        db.execute('insert into User (name) values (?)', [name])
        db.commit()
    
    def createDocument(self, name):
        """Creates a new document with the specified name."""
        db = getDb()
        db.execute('insert into Document (name) values (?)', [name])
        db.commit()
        #TODO: Create a file for the document in file system.
        #TODO: Put the document file under version control (using a Git module maybe?).
        return
    
    def addMarkDownBlock(self, document_id, content, previous_block_id):
        """Adds a new markdown block to the specified document."""
        db = getDb()
        db.execute('insert into Block (type_id) values (?)', [BlockType.DocumentBlock])
        block_id = db.last_insert_rowid()
        print(block_id)
        db.commit()
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