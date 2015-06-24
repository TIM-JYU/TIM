""""""
import os
import collections
import sqlite3

from contracts import contract, new_contract

new_contract('Connection', sqlite3.Connection)

BLOCKTYPES = collections.namedtuple('blocktypes', ('DOCUMENT', 'COMMENT', 'NOTE', 'ANSWER', 'IMAGE', 'READING', 'FOLDER'))
blocktypes = BLOCKTYPES(0, 1, 2, 3, 4, 5, 6)


class TimDbException(Exception):
    """The exception that is thrown when an error occurs during a TimDb operation."""
    pass


class TimDbBase(object):
    """Base class for TimDb classes (e.g. Users, Notes).

    :type db: sqlite3.Connection
    :type files_root_path: str
    :type current_user_name: str
    :type blocks_path: str
    """

    @contract
    def __init__(self, db: 'Connection', files_root_path: 'str', type_name: 'str', current_user_name: 'str'):
        """Initializes TimDB with the specified database and root path.
        
        :param db: The database connection.
        :param files_root_path: The root path where all the files will be stored.
        :param type_name: The type name.
        :param current_user_name: The current user name.
        """
        self.files_root_path = os.path.abspath(files_root_path)
        self.current_user_name = current_user_name

        self.blocks_path = os.path.join(self.files_root_path, 'blocks', type_name)
        for path in [self.blocks_path]:
            if not os.path.exists(path):
                os.makedirs(path)
        self.db = db

    @contract
    def insertBlockToDb(self, name: 'str', owner_group_id: 'int', block_type: 'int') -> 'int':
        """Inserts a block to database.

        :param name: The name (description) of the block.
        :param owner_group_id: The owner group of the block.
        :param block_type: The type of the block.
        :returns: The id of the block.
        """

        cursor = self.db.cursor()
        cursor.execute('INSERT INTO Block (description, UserGroup_id, type_id) VALUES (?,?,?)',
                       [name, owner_group_id, block_type])
        block_id = cursor.lastrowid
        self.db.commit()
        return block_id

    @contract
    def getBlockPath(self, block_id: 'int') -> 'str':
        """Gets the path of the specified block.
        
        :param block_id: The id of the block.
        :returns: The path of the block.
        """
        return os.path.join(self.blocks_path, str(block_id))

    @contract
    def blockExists(self, block_id: 'int', block_type: 'int', check_file: 'bool' = True) -> 'bool':
        """Checks whether the specified block exists.
        
        :param block_id: The id of the block to check.
        :param block_type: The type of the block to check.
        :returns: True if the block exists, false otherwise.
        """

        cursor = self.db.cursor()
        cursor.execute('SELECT exists(SELECT id FROM Block WHERE id = ? AND type_id = ? LIMIT 1)',
                       [block_id, block_type])
        result = cursor.fetchone()
        return result[0] == 1

    @contract
    def setOwner(self, block_id: 'int', usergroup_id: 'int'):
        """Changes the owner group for a block.

        :param block_id: The id of the block.
        :param usergroup_id: The id of the new usergroup.
        """
        cursor = self.db.cursor()
        cursor.execute('UPDATE Block SET UserGroup_id = ? WHERE id = ?',
                       [usergroup_id, block_id])
        self.db.commit()

    # TODO: contract
    def resultAsDictionary(self, cursor):
        """Converts the result in database cursor object to JSON."""

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
    def writeUtf8(self, content: 'str', path: 'str'):
        with open(path, 'w', encoding='utf-8', newline='\n') as f:
            f.write(content)

    @contract
    def getOwnedBlockRelations(self, block_id: 'int', user_id: 'int', relation_type: 'int') -> 'list(dict)':
        cursor = self.db.cursor()
        cursor.execute("""SELECT id, parent_block_specifier, description, created, modified FROM Block,BlockRelation WHERE
                             Block.id = BlockRelation.Block_id
                          AND id IN
                             (SELECT Block_id FROM BlockRelation WHERE parent_block_id = ?)
                          AND type_id = ?
                          AND UserGroup_id IN
                                 (SELECT UserGroup_id FROM UserGroupMember WHERE User_id = ?)""",
                       [block_id, relation_type, user_id])
        return self.resultAsDictionary(cursor)

    @contract
    def getBlockRelations(self, block_id: 'int', relation_type: 'int') -> 'list(dict)':
        cursor = self.db.cursor()
        cursor.execute("""SELECT id, parent_block_specifier, description, created, modified FROM Block,BlockRelation WHERE
                             Block.id = BlockRelation.Block_id
                          AND id IN
                             (SELECT Block_id FROM BlockRelation WHERE parent_block_id = ?)
                          AND type_id = ?""", [block_id, relation_type])
        return self.resultAsDictionary(cursor)
