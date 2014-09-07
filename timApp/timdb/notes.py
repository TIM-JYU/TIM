from contracts import contract
from timdb.timdbbase import TimDbBase, blocktypes, TimDbException, DocIdentifier
from ephemeralclient import EphemeralClient, EPHEMERAL_URL, NotInCacheException
import os

class Notes(TimDbBase):
    
    def getDocIdentifierForNote(self, note_id):
        return DocIdentifier('note', note_id)
    
    @contract
    def __init__(self, db_path : 'Connection', files_root_path : 'str', type_name : 'str', current_user_name : 'str'):
        """Initializes TimDB with the specified database and root path.
        
        :param db_path: The path of the database file.
        :param files_root_path: The root path where all the files will be stored.
        """
        TimDbBase.__init__(self, db_path, files_root_path, type_name, current_user_name)
        self.ec = EphemeralClient(EPHEMERAL_URL)

    @contract
    def addNote(self, usergroup_id: 'int', content : 'str', block_id : 'int', block_specifier : 'int', tags: 'list(str)') -> 'tuple(int,str)':
        """Adds a note to the document.
        
        :param usergroup_id: The usergroup who owns the note.
        :param content: The content of the note.
        :param block_id: The block to which the comment is added.
        :param block_specifier: A specifier that tells a more accurate position of the note.
               Should be the index of the paragraph within the document.
        """
        #TODO: Needs revision id.
        cursor = self.db.cursor()
        cursor.execute('insert into Block (description, UserGroup_id, type_id, created) values (?, ?, ?, CURRENT_TIMESTAMP)',
                       [",".join(tags), usergroup_id, blocktypes.NOTE])
        note_id = cursor.lastrowid
        assert note_id is not None, 'note_id was None'
        cursor.execute('insert into BlockRelation (block_id, parent_block_specifier, parent_block_id, parent_block_revision_id) values (?,?,?,?)',
                       [note_id, block_specifier, block_id, 0])
        
        self.writeUtf8(content, self.getBlockPath(note_id))
        
        self.db.commit()
        
        #TODO: Do notes need to be versioned?
        
        self.ec.loadDocument(self.getDocIdentifierForNote(note_id), content.encode('utf-8'))
        return note_id, self.ec.getDocumentFullHtml(self.getDocIdentifierForNote(note_id))
    
    @contract
    def deleteNote(self, note_id : 'int'):
        """Deletes a note.
        
        :param note_id: The id of the note to be deleted.
        """
        
        cursor = self.db.cursor()
        cursor.execute('delete from Block where id = ? and type_id = ?', [note_id, blocktypes.NOTE])
        if cursor.rowcount == 0:
            raise TimDbException('The block %d was not found.' % note_id)
        
        cursor.execute('delete from BlockRelation where Block_id = ?', [note_id])
        
        assert os.path.exists(self.getBlockPath(note_id)), "Block did not exist on file system."
        
        os.remove(self.getBlockPath(note_id))
        
        self.db.commit()
        
    @contract
    def modifyNote(self, note_id : 'int', new_content : 'str', tags : 'list(str)') -> 'str':
        """Modifies an existing note.
        
        :param note_id: The id of the note to be modified.
        :param new_content: The new content of the note.
        """
        
        if not self.blockExists(note_id, blocktypes.NOTE):
            raise TimDbException('The requested note was not found.')

        cursor = self.db.cursor()
        cursor.execute('update Block set description = ?, modified = CURRENT_TIMESTAMP where id = ? and type_id = ?',
                       [",".join(tags), note_id, blocktypes.NOTE])

        self.writeUtf8(new_content, self.getBlockPath(note_id))
        self.db.commit()
        self.ec.loadDocument(self.getDocIdentifierForNote(note_id), new_content.encode('utf-8'))
        return self.ec.getDocumentFullHtml(self.getDocIdentifierForNote(note_id))

    def processRows(self, rows):
        notes = []
        for row in rows:
            note_id = row['id']
            note = {'id': note_id,
                    'specifier': row['parent_block_specifier'],
                    'tags': (row['description'] or "").split(','),
                    'created': row['created'],
                    'modified': row['modified']}
            with open(self.getBlockPath(note_id), 'r', encoding='utf-8') as f:
                note['content'] = f.read()
            notes.append(note)
        for note in notes:
            try:
                note['htmlContent'] = self.ec.getDocumentFullHtml(self.getDocIdentifierForNote(note['id']))
            except NotInCacheException:
                self.ec.loadDocument(self.getDocIdentifierForNote(note['id']), note['content'].encode('utf-8'))
                note['htmlContent'] = self.ec.getDocumentFullHtml(self.getDocIdentifierForNote(note['id']))
        return notes

    @contract
    def getNotes(self, user_id : 'int', document_id : 'int') -> 'list(dict)':
        """Gets all the notes for a document for a user.
        
        :param user_id: The id of the user whose notes will be fetched.
        :param document_id: The id of the block whose notes will be fetched.
        """
        rows = self.getOwnedBlockRelations(document_id, user_id, blocktypes.NOTE)

        return self.processRows(rows)

    @contract
    def getAllNotes(self, document_id : 'int') -> 'list(dict)':
        """Gets all the notes for a document.

        :param document_id: The id of the block whose notes will be fetched.
        """
        rows = self.getBlockRelations(document_id, blocktypes.NOTE)

        return self.processRows(rows)
