from contracts import contract
from timdb.timdbbase import TimDbBase, blocktypes, TimDbException

class Readings(TimDbBase):
        
    @contract
    def getReadings(self, user_id : 'int', document_id : 'int') -> 'list(dict)':
        """Gets the reading info for a document for a user.
        
        :param user_id: The id of the user whose readings will be fetched.
        :param block_id: The id of the block whose readings will be fetched.
        """
        rows = self.getOwnedBlockRelations(document_id, user_id, blocktypes.READING)
        return [{'id' : row['id'],
                 'specifier' : row['parent_block_specifier'],
                 'text' : row['description'],
                 'created' : row['created']} for row in rows]

    @contract
    def setAsRead(self, usergroup_id : 'int', document_id : 'int', paragraph_specifier : 'int', current_text : 'str'):
        cursor = self.db.cursor()

        # Check if the reading already exists. If so, delete it first.
        cursor.execute("""select id from Block
                          join BlockRelation on id = block_id
                          where UserGroup_id = ?
                            and type_id = ?
                            and parent_block_id = ?
                            and parent_block_specifier = ?""", [usergroup_id,
                                                                blocktypes.READING,
                                                                document_id,
                                                                paragraph_specifier])
        result = self.resultAsDictionary(cursor)

        #assert len(result) <= 1, "There was two or more readings"

        if len(result) > 0:
            for row in result:
                cursor.execute("""delete from Block
                              where id = ?""", [row['id']])
                cursor.execute("""delete from BlockRelation
                              where block_id = ?""", [row['id']])

        cursor.execute('insert into Block (description, UserGroup_id, type_id, created) values (?, ?, ?, CURRENT_TIMESTAMP)',
                       [current_text, usergroup_id, blocktypes.READING])
        reading_id = cursor.lastrowid
        assert reading_id is not None, 'reading_id was None'
        cursor.execute('insert into BlockRelation (block_id, parent_block_specifier, parent_block_id, parent_block_revision_id) values (?,?,?,?)',
                       [reading_id, paragraph_specifier, document_id, 0])
        self.db.commit()
