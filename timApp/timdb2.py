'''
Another version of TimDb that stores documents as whole.
'''

from enum import Enum
import os
from shutil import copyfile
import sqlite3
import time
import urllib.request
from contracts import contract, new_contract


# import timeit
BLOCKTYPE = Enum('BLOCKTYPE', 'Document Comment Note Answer Image')
EPHEMERAL_URL = 'http://localhost:8001'
TABLE_NAMES = ['BlockEditAccess',
               'BlockViewAccess',
               'UserGroupMember',
               'ReadRevision',
               'BlockRelation',
               'Block',
               'User',
               'UserGroup']

new_contract('row', sqlite3.Row)

class TimDb(object):
    """Handles saving and retrieving information from TIM database."""

    @contract
    def __init__(self, db_path : 'str', files_root_path : 'str'):
        self.db = sqlite3.connect(db_path)
        self.db.row_factory = sqlite3.Row
        self.files_root_path = files_root_path
        
        # TODO: Make sure that db_path and files_root_path are valid!
        
        self.blocks_path = os.path.join(files_root_path, 'blocks')
        for path in [self.blocks_path]:
            if not os.path.exists(path):
                os.makedirs(path)
    
    #@contract
    def addMarkDownBlock(self, document_id : 'int', content : 'str', next_block_id : 'int|None') -> 'int':
        """Adds a new markdown block to the specified document.
        
        :param document_id: The id of the document.
        :param content: The content of the block.
        :param next_block_id: The id of the succeeding block.
           The value should be None if the block should be the last block of the document.
        :returns: The id of the newly added block.
        """

        document_path = self.getBlockPath(document_id)
        
        #TODO: Is this method needed? We could just call modify so that the content has 2 paragraphs.
        
        
        assert os.path.exists(document_path), 'document does not exist: %r' % document_id
        
        #Ephemeral does not yet support adding blocks.
        
        req = urllib.request.Request(url=EPHEMERAL_URL + '/add/' + str(document_id) + '/' + str(next_block_id), data=content, method='PUT')
        
        response = urllib.request.urlopen(req)
        
        print(response.read())
        #3. Check return value (success/fail).
        #4. Does Ephemeral save it to FS?
        
    
    def clear(self):
        """Clears the contents of all database tables."""
        for table in TABLE_NAMES:
            self.db.execute('delete from ' + table)  # TABLE_NAMES is constant so no SQL injection possible

    def close(self):
        """Closes the database connection."""
        self.db.close()
    
    def create(self):
        """Initializes the database from the schema.sql file.
        NOTE: The database is emptied if it exists."""
        with open('schema2.sql', 'r') as schema_file:
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
        cursor.execute('insert into Block (description, UserGroup_id, type_id) values (?,?,?)', [name, 0, BLOCKTYPE.Document.value])
        document_id = cursor.lastrowid
        self.db.commit()
        
        document_path = os.path.join(self.blocks_path, str(document_id))
        
        try:
            # Create an empty file.
            open(document_path, 'a').close()
        except OSError:
            print('Couldn\'t open file for writing:' + document_path)
            self.db.rollback()
            raise
        
        # TODO: Put the document file block under version control (using a Git module maybe?).
        # TODO: Should the empty doc be put in Ephemeral?
        return document_id

    @contract
    def importDocument(self, document_file : 'str', document_name : 'str'):
        """Imports the specified document in the database."""
        
        # Assuming the document file is markdown-formatted, importing a document is very straightforward.
        doc_id = self.createDocument(document_name)
        copyfile(document_file, self.getDocumentPath(doc_id))
        
        print('doc_id is: ' + str(doc_id))
        
        with open(document_file, 'rb') as f:
            req = urllib.request.Request(url=EPHEMERAL_URL + '/load/' + str(doc_id), data=f.read(), method='POST')
            response = urllib.request.urlopen(req)
            print(response.read())
        

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
        cursor.execute('select * from Block where id = ?', [document_id])
        
        #TODO: Assert that type_id == Document
        
        return cursor.fetchone()
    
   
    def getDocuments(self) -> 'list(dict)':
        """Gets all the documents in the database."""
        cursor = self.db.cursor()
        cursor.execute('select * from Block where type_id = ?', [BLOCKTYPE.Document.value])
        # return cursor.fetchall()
        rows = [x for x in cursor.fetchall()]
        cols = [x[0] for x in cursor.description]
        results = []
        for row in rows:
            result = {}
            for prop, val in zip(cols, row):
                result[prop] = val
            results.append(result)
        return results
    
    @contract
    def getDocumentsByIds(self, document_ids : 'list(int)') -> 'seq(row)':
        """Gets all the documents in the database."""
        cursor = self.db.cursor()
        cursor.execute('select * from Block where id in (%s)' % 
                           ','.join('?' * len(document_ids)), document_ids)
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
        #TODO: Get blocks from Ephemeral.
        #TODO: Ephemeral doesn't support this (at least not as well as it could). Cannot know how many blocks there are!
        
    
    @contract
    def getDocumentPath(self, document_id : 'int') -> 'str':
        """Gets the path of the specified document.
        
        :param document_id: The id of the document.
        :returns: The path of the document.
        """
        return os.path.join(self.blocks_path, str(document_id))

    @contract
    def getImagePath(self, image_id : 'int', image_filename : 'str'):
        """Gets the path of an image.
        
        :param image_id: The id of the image.
        :param image_filename: The filename of the image.
        :returns: The path of the image file.
        """
        
        return os.path.join(self.files_root_path, 'img', str(image_id), image_filename)

    @contract
    def getUser(self, user_id : 'int') -> 'row':
        """Gets the user with the specified id."""
        
        cursor = self.db.cursor()
        cursor.execute('select * from User where id = ?', [user_id])
        return cursor.fetchone()
        
    @contract
    def modifyMarkDownBlock(self, document_id : 'int', block_id : 'int', new_content : 'str'):
        """Modifies the specified block.
        
        :param document_id: The id of the document.
        :param block_id: The id (relative to document) of the paragraph to be modified.
        :param new_content: The new content of the paragraph.
        """
        document_path = self.getBlockPath(block_id)
        
        #TODO: This method needs the version id (hash) of the client's document to see if there's been another edit before this.
        
        assert os.path.exists(document_path), 'document does not exist: %r' % document_id
        
        req = urllib.request.Request(url=EPHEMERAL_URL + '/' + str(document_id) + '/' + str(block_id), data=bytes(new_content, encoding='utf-8'), method='PUT')
        response = urllib.request.urlopen(req)
        responseStr = str(response.read())
        print(responseStr)
        
        #TODO: Check return value (success/fail). Currently Ephemeral doesn't return anything.
    
    def saveImage(self, image_data : 'bytes', image_filename : 'str'):
        """Saves an image to the database."""
        
        # TODO: Check that the file extension is allowed.
        # TODO: User group id should be a parameter.
        cursor = self.db.cursor()
        cursor.execute('insert into Block (description, UserGroup_id, type_id) values (?,?,?)', [image_filename, 0, BLOCKTYPE.Image.value])
        img_id = cursor.lastrowid
        
        with open(self.getImagePath(img_id, image_filename), 'xb') as f:
            f.write(image_data)
        
        self.db.commit()
        return

    def deleteImage(self, image_id : 'int'):
        """Deletes an image from the database."""
        
        #TODO: Check that the user has right to delete image.
        cursor = self.db.cursor()
        cursor.execute('select description from Block where type_id = ? and id = ?', [BLOCKTYPE.Image.value, image_id])
        image_filename = cursor.fetchone()[0]
        cursor.execute('delete from Block where type_id = ? and id = ?', [BLOCKTYPE.Image.value, image_id])
        if cursor.rowcount == 0:
            raise
            #TODO: Raise error if no image was deleted.
        
        os.remove(self.getImagePath(image_id, image_filename))
        
        self.db.commit()
        