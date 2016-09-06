"""
Defines the TimDb database class.
"""

import sqlalchemy
from sqlalchemy.orm import scoped_session

from routes.logger import log_info
from tim_app import db, app

from timdb.notes import Notes
from timdb.tim_models import Version
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
from timdb.velps import Velps
from timdb.velpgroups import VelpGroups
from timdb.annotations import Annotations
import os


class TimDb(object):
    instances = 0
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
                log_info('Creating directory: {}'.format(path))
                os.makedirs(path)

        self.session = session
        while True:
            try:
                if session is None:
                    self.session = db.create_scoped_session()
                    self.owns_session = True
                    self.engine = sqlalchemy.create_engine(db_path)
                    self.db = self.engine.connect().connection  # psycopg2.connect(db_path)  # type$
                    break
                else:
                    self.db = db.get_engine(app, 'tim_main').connect().connection
                    self.owns_session = False
                    break
            except:
                sleep(0.5)
                log_info("Wait db")
        TimDb.instances += 1
        # num_connections = self.get_pg_connections()
        # log_info('TimDb instances/PG connections: {}/{} (constructor)'.format(TimDb.instances, num_connections))
        self.notes = Notes(self.db, files_root_path, 'notes', current_user_name, self.session)
        self.readings = Readings(self.db, files_root_path, 'notes', current_user_name, self.session)
        self.users = Users(self.db, files_root_path, 'users', current_user_name, self.session)
        self.images = Images(self.db, files_root_path, 'images', current_user_name, self.session)
        self.uploads = Uploads(self.db, files_root_path, 'uploads', current_user_name, self.session)
        self.files = Files(self.db, files_root_path, 'files', current_user_name, self.session)
        self.documents = Documents(self.db, files_root_path, 'documents', current_user_name, self.session)
        self.answers = Answers(self.db, files_root_path, 'answers', current_user_name, self.session)
        self.questions = Questions(self.db, files_root_path, 'questions', current_user_name, self.session)
        self.messages = Messages(self.db, files_root_path, 'messages', current_user_name, self.session)
        self.lectures = Lectures(self.db, files_root_path, 'lectures', current_user_name, self.session)
        self.folders = Folders(self.db, files_root_path, 'folders', current_user_name, self.session)
        self.lecture_answers = LectureAnswers(self.db, files_root_path, 'lecture_answers', current_user_name, self.session)
        self.velps = Velps(self.db, files_root_path, 'velps', current_user_name, self.session)
        self.velp_groups = VelpGroups(self.db, files_root_path, 'velp_groups', current_user_name, self.session)
        self.annotations = Annotations(self.db, files_root_path, 'annotations', current_user_name, self.session)

    def get_pg_connections(self):
        """Returns the number of clients currently connected to PostgreSQL."""
        cursor = self.db.cursor()
        cursor.execute('SELECT sum(numbackends) FROM pg_stat_database')
        num_connections = cursor.fetchone()[0]
        return num_connections

    def __del__(self):
        """Release the database connection when the object is deleted."""
        self.close()

    def commit(self):
        """Commits any changes to the database."""
        self.db.commit()

    def close(self):
        """Closes the database connection."""
        if hasattr(self, 'db') and self.db is not None:
            TimDb.instances -= 1
            self.db.close()
            if self.owns_session:
                self.session.remove()
                self.engine.dispose()
            self.db = None
            self.session = None
            # log_info('TimDb instances: {} (destructor)'.format(TimDb.instances))

    def execute_script(self, sql_file):
        """Executes an SQL file on the database.
        :param sql_file: The SQL script to be executed.
        """
        with open(sql_file, 'r', encoding='utf-8') as schema_file:
            self.db.cursor().executescript(schema_file.read())
        self.db.commit()

    def execute_sql(self, sql):
        """Executes an SQL command on the database.
        :param sql: The SQL command to be executed.
        """
        self.db.cursor().executescript(sql)
        self.db.commit()

    def get_version(self) -> int:
        """Gets the current database version.
        :return: The database version as an integer.
        """
        ver = self.session.query(db.func.max(Version.id)).scalar()
        assert isinstance(ver, int)
        return ver

    def update_version(self):
        """Updates the database version by inserting a new sequential entry in the Version table.
        """
        c = self.db.cursor()
        c.execute("""INSERT INTO Version(updated_on) VALUES (CURRENT_TIMESTAMP)""")
        self.db.commit()

    def table_exists(self, table_name):
        """Checks whether a table with the specified name exists in the database.
        """
        c = self.db.cursor()
        c.execute("SELECT EXISTS(SELECT * FROM information_schema.tables WHERE table_name = %s)", (table_name,))
        return c.fetchone()[0]
