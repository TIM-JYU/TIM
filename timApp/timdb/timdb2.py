"""
Another version of TimDb that stores documents as whole.
"""

import sqlite3
from contracts import contract
from timdb.notes import Notes
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


TABLE_NAMES = ['BlockEditAccess',
               'BlockViewAccess',
               'UserGroupMember',
               'BlockRelation',
               'Block',
               'User',
               'UserGroup',
               'Question',
               'Messages',
               'Lectures',
               'LectureAnswers']


class TimDb(object):
    """Handles saving and retrieving information from TIM database.

    :type readings: Readings
    :type notes: Notes
    :type users: Users
    :type images: Images
    :type documents: Documents
    :type answers: Answers
    :type folders: Folders
    :type db: sqlite3.Connection
    """

    @contract
    def __init__(self, db_path: 'str', files_root_path: 'str', current_user_name='Anonymous'):
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
        
        self.db = sqlite3.connect(db_path)
        self.db.row_factory = sqlite3.Row
        self.notes = Notes(self.db, files_root_path, 'notes', current_user_name)
        self.readings = Readings(self.db, files_root_path, 'notes', current_user_name)
        self.users = Users(self.db, files_root_path, 'users', current_user_name)
        self.images = Images(self.db, files_root_path, 'images', current_user_name)
        self.files = Files(self.db, files_root_path, 'files', current_user_name)
        self.documents = Documents(self.db, files_root_path, 'documents', current_user_name)
        self.answers = Answers(self.db, files_root_path, 'answers', current_user_name)
        self.questions = Questions(self.db, files_root_path, 'questions', current_user_name)
        self.messages = Messages(self.db, files_root_path, 'messages', current_user_name)
        self.lectures = Lectures(self.db, files_root_path, 'lectures', current_user_name)
        self.folders = Folders(self.db, files_root_path, 'folders', current_user_name)
        self.lecture_answers = LectureAnswers(self.db, files_root_path, 'lecture_answers', current_user_name)

    def clear(self):
        """Clears the contents of all database tables."""
        for table in TABLE_NAMES:
            self.db.execute('delete from ' + table)  # TABLE_NAMES is constant so no SQL injection possible

    def commit(self):
        """Commits any changes to the database"""
        self.db.commit()

    def close(self):
        """Closes the database connection."""
        self.db.close()

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
        :param sql_file: The SQL command to be executed.
        """
        self.db.cursor().executescript(sql)
        self.db.commit()

    def get_version(self):
        try:
            return self.db.execute("""SELECT MAX(id) FROM Version""").fetchone()[0]
        except sqlite3.OperationalError:
            return 0

    def update_version(self):
        self.db.execute("""INSERT INTO Version(updated_on) VALUES (CURRENT_TIMESTAMP)""")
        self.db.commit()

    def table_exists(self, table_name):
        return bool(self.db.execute("SELECT EXISTS(SELECT name from sqlite_master WHERE type = 'table' AND name = ?)",
                               [table_name]).fetchone()[0])
