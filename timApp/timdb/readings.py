from contracts import contract
from timdb.timdbbase import TimDbBase, blocktypes, TimDbException


class Readings(TimDbBase):
    @contract
    def getReadings(self, user_id: 'int', document_id: 'int') -> 'list(dict)':
        """Gets the reading info for a document for a user.
        
        :param user_id: The id of the user whose readings will be fetched.
        :param document_id: The id of the block whose readings will be fetched.
        """
        rows = self.getOwnedBlockRelations(document_id, user_id, blocktypes.READING)
        return [{'id': row['id'],
                 'specifier': row['parent_block_specifier'],
                 'text': row['description'],
                 'created': row['created']} for row in rows]

    @contract
    def setAsRead(self, usergroup_id: 'int', document_id: 'int', paragraph_specifier: 'int', current_text: 'str'):
        cursor = self.db.cursor()

        # Check if the reading already exists. If so, delete it first.
        cursor.execute("""SELECT id FROM Block
                          JOIN BlockRelation ON id = block_id
                          WHERE UserGroup_id = ?
                            AND type_id = ?
                            AND parent_block_id = ?
                            AND parent_block_specifier = ?""", [usergroup_id,
                                                                blocktypes.READING,
                                                                document_id,
                                                                paragraph_specifier])
        result = self.resultAsDictionary(cursor)

        # assert len(result) <= 1, "There was two or more readings"

        if len(result) > 0:
            for row in result:
                cursor.execute("""DELETE FROM Block
                              WHERE id = ?""", [row['id']])
                cursor.execute("""DELETE FROM BlockRelation
                              WHERE block_id = ?""", [row['id']])

        cursor.execute(
            'INSERT INTO Block (description, UserGroup_id, type_id, created) VALUES (?, ?, ?, CURRENT_TIMESTAMP)',
            [current_text, usergroup_id, blocktypes.READING])
        reading_id = cursor.lastrowid
        assert reading_id is not None, 'reading_id was None'
        cursor.execute(
            'INSERT INTO BlockRelation (block_id, parent_block_specifier, parent_block_id, parent_block_revision_id) VALUES (?,?,?,?)',
            [reading_id, paragraph_specifier, document_id, 0])
        self.db.commit()
