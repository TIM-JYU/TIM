"""
Handles temp database that is needed by lecture and questions.
"""

import sqlite3
from timdb.runningquestion import RunningQuestions
from timdb.usersshown import UsersShown
from timdb.usersextended import UsersExtended
from timdb.usersanswered import UsersAnswered
from timdb.useractivity import UserActivity
from timdb.newanswers import NewAnswers
from timdb.showpoints import ShowPoints
from timdb.pointsshown import PointsShown
from contracts import contract
import os


class TempDb(object):
    @contract
    def __init__(self, db_path: 'str', files_root_path: 'str', current_user_name='Anonymous'):
        """Initializes TimDB with the specified database and root path.

        :param db_path: The path of the database file.
        :param files_root_path: The root path where all the files will be stored.
        """
        self.db = sqlite3.connect(db_path)
        self.db.row_factory = sqlite3.Row
        self.runningquestions = RunningQuestions(self.db, files_root_path, 'runningquestions', current_user_name)
        self.usersshown = UsersShown(self.db, files_root_path, 'usersshown', current_user_name)
        self.usersextended = UsersExtended(self.db, files_root_path, 'usersextended', current_user_name)
        self.usersanswered = UsersAnswered(self.db, files_root_path, 'usersanswered', current_user_name)
        self.useractivity = UserActivity(self.db, files_root_path, 'useractivity', current_user_name)
        self.newanswers = NewAnswers(self.db, files_root_path, 'newanswers', current_user_name)
        self.showpoints = ShowPoints(self.db, files_root_path, 'showpoints', current_user_name)
        self.pointsshown = PointsShown(self.db, files_root_path, 'pointsshown', current_user_name)

    """
    def clear(self):
        #Clears the contents of all database tables.
        for table in TABLE_NAMES:
            self.db.execute('delete from ' + table)  # TABLE_NAMES is constant so no SQL injection possible
    """

    def commit(self):
        """Commits any changes to the database"""
        self.db.commit()

    def close(self):
        """Closes the database connection."""
        self.db.commit()
        self.db.close()

    def initialize_tables(self):
        """Initializes the database from the schema2.sql file.
        NOTE: The database is emptied if it exists."""
        self.execute_script('temp_schema.sql')

    def execute_script(self, sql_file):
        """Executes an SQL file on the database.
        :param sql_file: The SQL script to be executed.
        """
        with open(sql_file, 'r') as schema_file:
            self.db.cursor().executescript(schema_file.read())
        self.db.commit()

    def execute_sql(self, sql):
        """Executes an SQL command on the database.
        :param sql_file: The SQL command to be executed.
        """
        self.db.cursor().executescript(sql)
        self.db.commit()

    """
    def get_version(self):
        try:
            return self.db.execute("SELECT MAX(id) FROM Version").fetchone()[0]
        except sqlite3.OperationalError:
            return 0

    def update_version(self):
        self.db.execute("INSERT INTO Version(updated_on) VALUES (CURRENT_TIMESTAMP)")
        self.db.commit()
    """