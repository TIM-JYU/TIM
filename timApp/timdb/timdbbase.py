""""""
import os
from contracts import contract, new_contract
import collections
import sqlite3
from collections import namedtuple


# A document identifier consists of the id of the document and the version hash.
class DocIdentifier(namedtuple("DocIdentifier", "id hash")):
    __slots__ = ()

    def __str__(self):
        return str(self.id) + ':' + str(self.hash)


new_contract('Connection', sqlite3.Connection)
new_contract('DocIdentifier', DocIdentifier)

BLOCKTYPES = collections.namedtuple('blocktypes', ('DOCUMENT', 'COMMENT', 'NOTE', 'ANSWER', 'IMAGE', 'READING'))
blocktypes = BLOCKTYPES(0, 1, 2, 3, 4, 5)


class TimDbException(Exception):
    """The exception that is thrown when an error occurs during a TimDb operation."""
    pass


class TimDbBase(object):
    """Base class for TimDb classes (e.g. Users, Notes)."""

    @contract
    def __init__(self, db: 'Connection', files_root_path: 'str', type_name: 'str', current_user_name: 'str'):
        """Initializes TimDB with the specified database and root path.
        
        :param db_path: The path of the database file.
        :param files_root_path: The root path where all the files will be stored.
        """
        self.files_root_path = os.path.abspath(files_root_path)
        self.current_user_name = current_user_name

        self.blocks_path = os.path.join(self.files_root_path, 'blocks', type_name)
        for path in [self.blocks_path]:
            if not os.path.exists(path):
                os.makedirs(path)
        self.db = db

    @contract
    def getBlockPath(self, block_id: 'int') -> 'str':
        """Gets the path of the specified block.
        
        :param block_id: The id of the block.
        :returns: The path of the block.
        """
        return os.path.join(self.blocks_path, str(block_id))

    @contract
    def blockExists(self, block_id: 'int', block_type: 'int') -> 'bool':
        """Checks whether the specified block exists.
        
        :param block_id: The id of the block to check.
        :param block_type: The type of the block to check.
        :returns: True if the block exists, false otherwise.
        """

        cursor = self.db.cursor()
        cursor.execute('SELECT exists(SELECT id FROM Block WHERE id = ? AND type_id = ? LIMIT 1)',
                       [block_id, block_type])
        result = cursor.fetchone()
        if result[0] == 1:
            assert os.path.exists(
                self.getBlockPath(block_id)), 'the block {} was in database but the file was not found'.format(block_id)
            return True
        return False

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
