from contracts import contract
from timdb.timdbbase import TimDbBase, blocktypes, TimDbException
import os

class Notes(TimDbBase):

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
        
        with open(self.getBlockPath(note_id), 'w', encoding='utf-8', newline='\n') as f:
            f.write(content)
        
        self.db.commit()
        
        #TODO: Do notes need to be versioned?
        
        return
    
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
    def modifyNote(self, note_id : 'int', new_content : 'str'):
        """Modifies an existing note.
        
        :param note_id: The id of the note to be modified.
        :param new_content: The new content of the note.
        """
        
        if not self.blockExists(note_id, blocktypes.NOTE):
            raise TimDbException('The requested note was not found.')
        
        with open(self.getBlockPath(note_id), 'w', encoding='utf-8', newline='\n') as f:
            f.write(new_content)
        
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
            with open(self.getBlockPath(note_id), 'r', encoding='utf-8') as f:
                note['content'] = f.read()
            notes.append(note)
        return notes
