""""""
import os
#import time
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

BLOCKTYPES = collections.namedtuple('blocktypes', ('DOCUMENT', 'COMMENT', 'NOTE', 'ANSWER', 'IMAGE'))
blocktypes = BLOCKTYPES(0,1,2,3,4)

class TimDbException(Exception):
    """The exception that is thrown when an error occurs during TimDb operation."""
    pass

class TimDbBase(object):
    """Base class for TimDb classes (e.g. Users, Notes)."""
    
    @contract
    def __init__(self, db : 'Connection', files_root_path : 'str'):
        """Initializes TimDB with the specified database and root path.
        
        :param db_path: The path of the database file.
        :param files_root_path: The root path where all the files will be stored.
        """
        self.files_root_path = os.path.abspath(files_root_path)
        
        # TODO: Make sure that files_root_path is valid!
        
        self.blocks_path = os.path.join(self.files_root_path, 'blocks')
        for path in [self.blocks_path]:
            if not os.path.exists(path):
                os.makedirs(path)
        self.db = db
        
    @contract
    def getBlockPath(self, block_id : 'int') -> 'str':
        """Gets the path of the specified block.
        
        :param block_id: The id of the block.
        :returns: The path of the block.
        """
        return os.path.join(self.blocks_path, str(block_id))
    
    @contract
    def blockExists(self, block_id : 'int', block_type : 'int') -> 'bool':
        """Checks whether the specified block exists.
        
        :param block_id: The id of the block to check.
        :param block_type: The type of the block to check.
        :returns: True if the block exists, false otherwise.
        """
        
        cursor = self.db.cursor()
        cursor.execute('select exists(select id from Block where id = ? and type_id = ? limit 1)', [block_id, block_type])
        result = cursor.fetchone()
        if result[0] == 1:
            assert os.path.exists(self.getBlockPath(block_id)), 'the block was in database but the file was not found'
            return True
        return False
    
    #TODO: contract
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
    def writeUtf8(self, content : 'str', path : 'str'):
        with open(path, 'w', encoding='utf-8', newline='\n') as f:
            f.write(content)