"""
Handles temp database that is needed by lecture and questions.
"""

# import sqlite3
import time
import psycopg2
import psycopg2.extras
from timdb.runningquestion import RunningQuestions
from timdb.useractivity import UserActivity
from timdb.newanswers import NewAnswers
from timdb.showpoints import ShowPoints
from timdb.temp_info_for_user import TempInfoUserQuestion
from contracts import contract
import os


class TempDb(object):
    @contract
    def __init__(self, db_path: 'str', files_root_path: 'str', current_user_name='Anonymous'):
        """Initializes TimDB with the specified database and root path.

        :param db_path: The path of the database file.
        :param files_root_path: The root path where all the files will be stored.
        """
        tries = 0
        while tries < 15:
            try:
                self.db = psycopg2.connect('dbname=docker user=docker password=docker host=/run/postgresql')
                break
            except psycopg2.OperationalError:
                print("PostGre is not ready yet")
                time.sleep(1)
                tries += 1

        if tries >= 14:
            exit()

        self.cursor = self.db.cursor(cursor_factory=psycopg2.extras.DictCursor)
        self.runningquestions = RunningQuestions(self.db, self.cursor)
        self.showpoints = ShowPoints(self.db, self.cursor)
        self.useractivity = UserActivity(self.db, self.cursor)
        self.newanswers = NewAnswers(self.db, self.cursor)
        self.usersshown = TempInfoUserQuestion(self.db, self.cursor, 'UserShown')
        self.usersextended = TempInfoUserQuestion(self.db, self.cursor, 'UserExtended')
        self.usersanswered = TempInfoUserQuestion(self.db, self.cursor, 'UserAnswered')
        self.pointsshown = TempInfoUserQuestion(self.db, self.cursor, 'PointsShown')
        self.pointsclosed = TempInfoUserQuestion(self.db, self.cursor, 'PointsClosed')

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
        self.db.cursor().execute(open("temp_schema.sql", "r").read())
        # self.execute_script('temp_schema.sql')

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