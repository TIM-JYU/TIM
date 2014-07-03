'''
Another version of TimDb that stores documents as whole.
'''

# TODO: This file is getting rather large. It should probably be divided somehow.

#from enum import Enum
import os
from shutil import copyfile
import sqlite3
#import time
from contracts import contract, new_contract
#from vcstools import GitClient # vcstools doesn't seem to support creating repos...
import gitpylib.repo
import gitpylib.sync
import gitpylib.file
import gitpylib.common
from ephemeralclient import EphemeralClient, EphemeralException, NotInCacheException
import collections

class TimDbException(Exception):
    pass

BLOCKTYPES=collections.namedtuple('blocktypes', ('DOCUMENT', 'COMMENT', 'NOTE', 'ANSWER', 'IMAGE'))
blocktypes=BLOCKTYPES(0,1,2,3,4)
# import timeit
#BLOCKTYPE = Enum('BLOCKTYPE', 'Document Comment Note Answer Image')
#DOCUMENT = 1
#COMMENT = 2
#NOTE = 3
#ANSWER = 4
#IMAGE = 5

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
        """Initialized TimDB with the specified database and root path.
        
        :param db_path: The path of the database file.
        :param files_root_path: The root path where all the files will be stored.
        """
        self.files_root_path = files_root_path
        
        # TODO: Make sure that db_path and files_root_path are valid!
        
        self.blocks_path = os.path.join(os.path.abspath(files_root_path), 'blocks')
        for path in [self.blocks_path]:
            if not os.path.exists(path):
                os.makedirs(path)
        self.db = sqlite3.connect(db_path)
        self.db.row_factory = sqlite3.Row
        
    def initRepo(self):
        #Initialize repo to root path:
        
        cwd = os.getcwd()
        os.chdir(self.files_root_path)
        gitpylib.repo.init()
        
        #Restore old working directory (TODO: is this needed?)
        os.chdir(cwd)
    
    @contract
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
        
        ec = EphemeralClient(EPHEMERAL_URL)
        success = ec.addBlock(document_id, next_block_id, content)
        
        #3. Check return value (success/fail).
        #4. Does Ephemeral save it to FS?
        return 0
    
    @contract
    def addNote(self, usergroup_id: 'int', content : 'str', block_id : 'int', block_specifier : 'int'):
        """Adds a note to the document.
        
        :param usergroup_id: The usergroup who owns the note.
        :param content: The content of the note.
        :param block_id: The block to which the comment is added.
        :param block_specifier: A specifier that tells a more accurate position of the note.
               Should be the index of the paragraph within the document.
        """
        #TODO: Needs revision id.
        cursor = self.db.cursor()
        cursor.execute('insert into Block (description, UserGroup_id, type_id) values (?, ?, ?)', [None, usergroup_id, blocktypes.NOTE])
        note_id = cursor.lastrowid
        assert note_id is not None, 'note_id was None'
        cursor.execute('insert into BlockRelation (block_id, parent_block_specifier, parent_block_id, parent_block_revision_id) values (?,?,?,?)',
                       [note_id, block_specifier, block_id, 0])
        
        with open(self.getBlockPath(note_id), 'w', encoding='utf-8') as f:
            f.write(content)
        
        self.db.commit()
        
        #TODO: Do notes need to be versioned?
        
        return
    
    @contract
    def getNotes(self, user_id : 'int', document_id : 'int'):
        """Gets all the notes for a document for a user.
        
        :param user_id: The id of the user whose notes will be fetched.
        :param block_id: The id of the block whose notes will be fetched.
        """
        cursor = self.db.cursor()
        cursor.execute("""select id, parent_block_specifier from Block,BlockRelation where
                             Block.id = BlockRelation.Block_id
                          and id in
                             (select Block_id from BlockRelation where parent_block_id = ?)
                          and type_id = ?
                          and UserGroup_id in
                                 (select UserGroup_id from UserGroupMember where User_id = ?)""", [document_id, blocktypes.NOTE, user_id])
        rows = [x for x in cursor.fetchall()]
        
        notes = []
        for row in rows:
            note_id = row[0]
            note = {'id' : note_id, 'specifier' : row[1]}
            with open(self.getBlockPath(note_id)) as f:
                note['content'] = f.read()
            notes.append(note)
        return notes
        
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
        cursor.execute('insert into Block (description, UserGroup_id, type_id) values (?,?,?)', [name, 0, blocktypes.DOCUMENT])
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
        
        # Don't commit to Git at this point.
        #sha_hash = self.gitCommit(document_path, 'Created document: %s' % name, 'docker')
        #print(sha_hash)
        
        # TODO: Should the empty doc be put in Ephemeral?
        return document_id
    
    @contract
    def gitCommit(self, file_path : 'str', commit_message: 'str', author : 'str'):
        cwd = os.getcwd()
        os.chdir(self.files_root_path)
        gitpylib.file.stage(file_path)
        # TODO: Set author for the commit (need to call safe_git_call).
        gitpylib.sync.commit([file_path], commit_message, skip_checks=False, include_staged_files=False)
        latest_hash, err = gitpylib.common.safe_git_call('rev-parse HEAD') # Gets the latest version hash
        os.chdir(cwd)
        return latest_hash.rstrip()

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
    def importDocument(self, document_file : 'str', document_name : 'str') -> 'int':
        """Imports the specified document in the database."""
        
        # Assuming the document file is markdown-formatted, importing a document is very straightforward.
        doc_id = self.createDocument(document_name)
        copyfile(document_file, self.getDocumentPath(doc_id))
        
        with open(document_file, 'rb') as f:
            ec = EphemeralClient(EPHEMERAL_URL)
            ec.loadDocument(doc_id, f.read())
        
        sha_hash = self.gitCommit(self.getDocumentPath(doc_id), 'Imported document: %s' % document_name, 'docker')
        return doc_id
    
    @contract
    def grantViewAccess(self, group_id : 'int', block_id : 'int'):
        """Grants view access to a group for a block.
        
        :param group_id: The group id to which to grant view access.
        :param block_id: The id of the block for which to grant view access.
        """

        #TODO: Check that the group_id and block_id exist.
        cursor = self.db.cursor()
        cursor.execute('insert into BlockViewAccess (Block_id,UserGroup_id,visible_from) values (?,?,date(\'now\'))', [block_id, group_id])
        self.db.commit()

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
    def createUserGroup(self, name : 'str') -> 'int':
        """Creates a new user group.
        
        :returns: The id of the created user group.
        """
        cursor = self.db.cursor()
        cursor.execute('insert into UserGroup (name) values (?)', [name])
        group_id = cursor.lastrowid
        assert group_id is not None, 'group_id was None'
        self.db.commit()
        return group_id
    
    @contract
    def addUserToGroup(self, group_id : 'int', user_id : 'int'):
        cursor = self.db.cursor()
        cursor.execute('insert into UserGroupMember (UserGroup_id, User_id) values (?, ?)', [group_id, user_id])
        self.db.commit()

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
        cursor.execute('select id, description as name from Block where id = ? and type_id = ?', [document_id, blocktypes.DOCUMENT])
        
        return cursor.fetchone()
    
    @contract
    def getDocuments(self) -> 'list(dict)':
        """Gets all the documents in the database.
        
        :returns: A list of dictionaries of the form {'id': <doc_id>, 'name': 'document_name'}
        """
        cursor = self.db.cursor()
        cursor.execute('select id,description as name from Block where type_id = ?', [blocktypes.DOCUMENT])
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
    def documentExists(self, document_id : 'int') -> 'bool':
        """Checks whether a document with the specified id exists.
        
        :param document_id: The id of the document.
        :returns: True if the documents exists, false otherwise.
        """
        
        cursor = self.db.cursor()
        cursor.execute('select count(id) from Block where id = ? and type_id = ?', [document_id, blocktypes.DOCUMENT])
        result = cursor.fetchall()
        assert len(result) <= 1, 'len(result) was more than 1'
        return len(result) == 1
        
    @contract
    def getDocumentAsHtmlBlocks(self, document_id : 'int') -> 'list(str)':
        """Gets the specified document in HTML form."""
        
        ec = EphemeralClient(EPHEMERAL_URL)
        
        try:
            blocks = ec.getDocumentAsHtmlBlocks(document_id)
        except NotInCacheException:
            if self.documentExists(document_id):
                with open(self.getBlockPath(document_id), 'rb') as f:
                    ec.loadDocument(document_id, f.read())
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
        cwd = os.getcwd()
        os.chdir(self.files_root_path)
        output, err = gitpylib.common.safe_git_call('log --format=%H|%ad ' + os.path.relpath(self.getDocumentPath(document_id)).replace('\\', '/'))
        os.chdir(cwd)
        lines = output.splitlines()
        versions = []
        for line in lines:
            pieces = line.split('|')
            versions.append({'hash' : pieces[0], 'timestamp' : pieces[1]})
        return versions
        
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
        """Gets the user with the specified id.
        
        :returns: An sqlite3 row object representing the user. Columns: id, name.
        """
        
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
        document_path = self.getDocumentPath(document_id)
        
        #TODO: This method needs the version id (hash) of the client's document to see if there's been another edit before this.
        
        assert os.path.exists(document_path), 'document does not exist: %r' % document_id
        
        ec = EphemeralClient(EPHEMERAL_URL)
        ec.modifyBlock(document_id, block_id, new_content)
        doc_content = ec.getDocumentFullText(document_id)
        
        with open(self.getDocumentPath(document_id), 'w', encoding='utf-8') as f:
            f.write(doc_content)
            
        sha_hash = self.gitCommit(document_path, 'Modified document with id: %d' % document_id, 'docker')
        
        #TODO: Check return value (success/fail). Currently Ephemeral doesn't return anything.
    
    @contract
    def userHasViewAccess(self, user_id : 'int', block_id : 'int') -> 'bool':
        """Returns whether the user has access to the specified block.
        
        :returns: True if the user with id 'user_id' has view access to the block 'block_id', false otherwise.
        """
        
        cursor = self.db.cursor()
        cursor.execute("""select id from User where
                          id = ?
                          and (User.id in 
                              (select User_id from UserGroupMember where UserGroup_id in
                                  (select UserGroup_id from BlockViewAccess where Block_id = ?))
                              
                          or  (User.id in 
                              (select User_id from UserGroupMember where UserGroup_id in
                              (select UserGroup_id from Block where Block.id = ?))
                              ))""", [user_id, block_id, block_id])
        result = cursor.fetchall()
        assert len(result) <= 1, 'rowcount should be 1 at most'
        return len(result) == 1
    
    @contract
    def userHasEditAccess(self, user_id : 'int', block_id : 'int') -> 'bool':
        """Returns whether the user has access to the specified block.
        
        :returns: True if the user with id 'user_id' has view access to the block 'block_id', false otherwise.
        """
        
        #TODO: This method is pretty much copy-paste from userHasViewAccess. Should make some common method.
        cursor = self.db.cursor()
        cursor.execute("""select id from User where
                          id = ?
                          and (User.id in 
                              (select User_id from UserGroupMember where UserGroup_id in
                                  (select UserGroup_id from BlockEditAccess where Block_id = ?))
                              
                          or  (User.id in 
                              (select User_id from UserGroupMember where UserGroup_id in
                              (select UserGroup_id from Block where Block.id = ?))
                              ))""", [user_id, block_id, block_id])
        assert cursor.rowcount <= 1, 'rowcount should be 1 at most'
        return cursor.rowcount == 1
    
    @contract
    def userIsOwner(self, user_id : 'int', block_id : 'int') -> 'bool':
        """Returns whether the user belongs to the owners of the specified block.
        
        :returns: True if the user with 'user_id' belongs to the owner group of the block 'block_id'.
        """
        cursor = self.db.cursor()
        cursor.execute("""select id from User where
                          id = ?
                          and (id in
                              (select User_id from UserGroup where id in
                              (select UserGroup_id from Block where Block_id = ?))
                              )""", [user_id, block_id])
        assert cursor.rowcount <= 1, 'rowcount should be 1 at most'
        return cursor.rowcount == 1
    
    @contract
    def saveImage(self, image_data : 'bytes', image_filename : 'str'):
        """Saves an image to the database."""
        
        # TODO: Check that the file extension is allowed.
        # TODO: Should file name be unique among images?
        # TODO: User group id should be a parameter.
        cursor = self.db.cursor()
        cursor.execute('insert into Block (description, UserGroup_id, type_id) values (?,?,?)', [image_filename, 0, blocktypes.IMAGE])
        img_id = cursor.lastrowid
        
        with open(self.getImagePath(img_id, image_filename), 'xb') as f:
            f.write(image_data)
        
        self.db.commit()
        #TODO: Return image filename (and id if file names don't have to be unique).
        return

    @contract
    def deleteImage(self, image_id : 'int'):
        """Deletes an image from the database."""
        
        #TODO: Check that the user has right to delete image.
        cursor = self.db.cursor()
        cursor.execute('select description from Block where type_id = ? and id = ?', [blocktypes.IMAGE, image_id])
        image_filename = cursor.fetchone()[0]
        cursor.execute('delete from Block where type_id = ? and id = ?', [blocktypes.IMAGE, image_id])
        if cursor.rowcount == 0:
            raise
            #TODO: Raise error if no image was deleted.
        
        os.remove(self.getImagePath(image_id, image_filename))
        
        self.db.commit()
        
