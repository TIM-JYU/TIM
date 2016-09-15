""""""
import collections
from typing import Iterable
import decimal
import os
from typing import Optional, Tuple

from psycopg2._psycopg import connection
from sqlalchemy.orm import scoped_session

from timdb.tim_models import Block

BLOCKTYPES = collections.namedtuple('blocktypes', ('DOCUMENT', 'COMMENT', 'NOTE', 'ANSWER', 'IMAGE', 'READING', 'FOLDER', 'FILE', 'UPLOAD', 'VELPGROUP', 'ANNOTATION'))
blocktypes = BLOCKTYPES(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10)


class TimDbBase(object):
    """Base class for TimDb classes (e.g. Users, Notes).

    :type db: connection
    :type files_root_path: str
    :type current_user_name: str
    :type blocks_path: str
    """

    def __init__(self, db: connection, files_root_path: str, type_name: str, current_user_name: str, session: scoped_session):
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
        self.session = session

    def get_sql_template(self, value_list: list):
        return ','.join(['%s'] * len(value_list))

    def insertBlockToDb(self, description: Optional[str], owner_group_id: int, block_type: int, commit: bool=True) -> int:
        """Inserts a block to database.

        :param commit: Whether to commit the change.
        :param description: The name (description) of the block.
        :param owner_group_id: The owner group of the block.
        :param block_type: The type of the block.
        :returns: The id of the block.
        """
        b = Block(description=description, type_id=block_type, usergroup_id=owner_group_id)
        self.session.add(b)
        self.session.flush()
        block_id = b.id
        assert block_id != 0
        if commit:
            self.session.commit()
        return block_id

    def getBlockPath(self, block_id: int) -> str:
        """Gets the path of the specified block.
        
        :param block_id: The id of the block.
        :returns: The path of the block.
        """
        return os.path.join(self.blocks_path, str(block_id))

    def blockExists(self, block_id: int, block_type: int, check_file: bool = True) -> bool:
        """Checks whether the specified block exists.
        
        :param block_id: The id of the block to check.
        :param block_type: The type of the block to check.
        :returns: True if the block exists, false otherwise.
        """

        cursor = self.db.cursor()
        try:
            cursor.execute('SELECT exists(SELECT id FROM Block WHERE id = %s AND type_id = %s LIMIT 1)',
                           [block_id, block_type])
        except OverflowError:
            return False
        result = cursor.fetchone()
        return result[0] == 1

    def get_owner(self, block_id: int) -> Optional[int]:
        """Returns the owner group for a block.

        :param block_id: The id of the block.
        """
        cursor = self.db.cursor()
        cursor.execute('SELECT UserGroup_id FROM Block WHERE id = %s', [block_id])
        result = cursor.fetchone()
        return result[0] if result is not None else None

    def setOwner(self, block_id: int, usergroup_id: int):
        """Changes the owner group for a block.

        :param block_id: The id of the block.
        :param usergroup_id: The id of the new usergroup.
        """
        cursor = self.db.cursor()
        cursor.execute('UPDATE Block SET UserGroup_id = %s WHERE id = %s',
                       [usergroup_id, block_id])
        self.db.commit()

    def resultAsDictionary(self, cursor):
        """Converts the result in database cursor object to JSON."""

        rows = [x for x in cursor.fetchall()]
        cols = [x[0] for x in cursor.description]
        results = []
        for row in rows:
            result = {}
            for prop, val in zip(cols, row):
                if isinstance(val, decimal.Decimal): val = float(val)
                result[prop] = val
            results.append(result)
        return results

    @classmethod
    def split_location(cls, path: str) -> Tuple[str, str]:
        rs = path.rfind('/')
        return ('', path) if rs < 0 else (path[:rs], path[rs+1:])

    @classmethod
    def join_location(cls, location: str, name: str) -> str:
        return name if location == '' else location + '/' + name

    def get_id_filter(self, filter_ids: Iterable[int]):
        return ' AND id IN ({})'.format(','.join(str(x) for x in filter_ids))
