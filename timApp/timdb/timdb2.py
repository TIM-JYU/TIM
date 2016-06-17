"""
Defines the TimDb database class.
"""

import sqlite3
from sqlalchemy.orm import scoped_session

from tim_app import db
from timdb.notes import Notes
from timdb.uploads import Uploads
from timdb.users import Users
from timdb.images import Images
from timdb.files import Files
from timdb.documents import Documents
from timdb.answers import Answers
from timdb.readings import Readings
from timdb.questions import Questions
from timdb.messages import Messages
from timdb.lectures import Lectures
from timdb.folders import Folders
from timdb.lectureanswers import LectureAnswers
import os


class TimDb(object):
    """Handles saving and retrieving information from TIM database.
    """

    def __init__(self, db_path: str,
                 files_root_path: str,
                 session: scoped_session = None,
                 current_user_name: str = 'Anonymous'):
        """Initializes TimDB with the specified database, files root path, SQLAlchemy session and user name.
        
        :param session: The scoped_session to be used for SQLAlchemy operations. If None, a scoped_session will be
        created.
        :param current_user_name: The username of the current user.
        :param db_path: The path of the database file.
        :param files_root_path: The root path where all the files will be stored.
        """
        self.files_root_path = os.path.abspath(files_root_path)
        
        self.blocks_path = os.path.join(self.files_root_path, 'blocks')
        for path in [self.blocks_path]:
            if not os.path.exists(path):
                os.makedirs(path)
        
        self.db = sqlite3.connect(db_path)
        self.db.row_factory = sqlite3.Row

        if session is None:
            self.session = db.create_scoped_session()

        self.notes = Notes(self.db, files_root_path, 'notes', current_user_name)
        self.readings = Readings(self.db, files_root_path, 'notes', current_user_name)
        self.users = Users(self.db, files_root_path, 'users', current_user_name)
        self.images = Images(self.db, files_root_path, 'images', current_user_name)
        self.uploads = Uploads(self.db, files_root_path, 'uploads', current_user_name)
        self.files = Files(self.db, files_root_path, 'files', current_user_name)
        self.documents = Documents(self.db, files_root_path, 'documents', current_user_name)
        self.answers = Answers(self.db, files_root_path, 'answers', current_user_name)
        self.questions = Questions(self.db, files_root_path, 'questions', current_user_name)
        self.messages = Messages(self.db, files_root_path, 'messages', current_user_name)
        self.lectures = Lectures(self.db, files_root_path, 'lectures', current_user_name)
        self.folders = Folders(self.db, files_root_path, 'folders', current_user_name)
        self.lecture_answers = LectureAnswers(self.db, files_root_path, 'lecture_answers', current_user_name)

    def __del__(self):
        """Release the database connection when the object is deleted."""
        if self.db is not None:
            self.close()

    def commit(self):
        """Commits any changes to the database."""
        self.db.commit()

    def close(self):
        """Closes the database connection."""
        if self.db is not None:
            self.db.close()
            self.db = None

    def initialize_tables(self):
        """Initializes the database from the schema2.sql file.
        NOTE: The database is emptied if it exists."""
        self.execute_script('schema2.sql')

    def execute_script(self, sql_file):
        """Executes an SQL file on the database.
        :param sql_file: The SQL script to be executed.
        """
        with open(sql_file, 'r') as schema_file:
            self.db.cursor().executescript(schema_file.read())
        self.db.commit()

    def execute_sql(self, sql):
        """Executes an SQL command on the database.
        :param sql: The SQL command to be executed.
        """
        self.db.cursor().executescript(sql)
        self.db.commit()

    def get_version(self):
        """Gets the current database version.
        :return: The database version as an integer.
        """
        try:
            return self.db.execute("""SELECT MAX(id) FROM Version""").fetchone()[0]
        except sqlite3.OperationalError:
            return 0

    def update_version(self):
        """Updates the database version by inserting a new sequential entry in the Version table.
        """
        self.db.execute("""INSERT INTO Version(updated_on) VALUES (CURRENT_TIMESTAMP)""")
        self.db.commit()

    def table_exists(self, table_name):
        """Checks whether a table with the specified name exists in the database.
        """
        return bool(self.db.execute("SELECT EXISTS(SELECT name from sqlite_master WHERE type = 'table' AND name = ?)",
                               [table_name]).fetchone()[0])
