""""""
import decimal
import os

from psycopg2._psycopg import connection
from sqlalchemy.orm import scoped_session


def result_as_dict_list(cursor):
    """Converts the result in database cursor object to JSON."""

    rows = [x for x in cursor.fetchall()]
    cols = [x[0] for x in cursor.description]
    results = []
    for row in rows:
        result = {}
        for prop, val in zip(cols, row):
            if isinstance(val, decimal.Decimal):
                val = float(val)
            result[prop] = val
        results.append(result)
    return results


class TimDbBase:
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
