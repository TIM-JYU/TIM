"""Defines the TimDb database class."""
import time
from pathlib import Path
from time import sleep

from timApp.timdb.sqa import db
from timApp.util.logger import log_info, log_debug, log_error, log_warning
from timApp.velp.annotations import Annotations
from timApp.velp.velpgroups import VelpGroups
from timApp.velp.velps import Velps

num = 0

# Always 0 for now.
worker_pid = 0

DB_PART_NAMES = {'notes', 'readings', 'users', 'images', 'uploads', 'files', 'documents', 'answers', 'questions',
                 'messages', 'lectures', 'folders', 'lecture_answers', 'velps', 'velp_groups', 'annotations', 'session'}


class TimDb:
    """DEPRECATED CLASS, DO NOT ADD NEW CODE!

    Handles saving and retrieving information from TIM database.
    """
    instances = 0

    def __init__(self, files_root_path: Path,
                 current_user_name: str = 'Anonymous',
                 route_path: str = ''):
        """Initializes TimDB with the specified files root path, SQLAlchemy session and user name.

        created.
        :param current_user_name: The username of the current user.
        :param files_root_path: The root path where all the files will be stored.
        :param route_path: Path for the route requesting the db

        """
        self.files_root_path = files_root_path
        self.route_path = route_path
        self.current_user_name = current_user_name

        self.blocks_path = self.files_root_path / 'blocks'
        for path in [self.blocks_path]:
            if not path.exists():
                log_info(f'Creating directory: {path}')
                path.mkdir(parents=True, exist_ok=False)
        self.reset_attrs()

    def reset_attrs(self):
        self.num = 0
        self.time = 0
        self.engine = None
        self.db = None
        self.velps = None
        self.velp_groups = None
        self.annotations = None

    def __getattribute__(self, item):
        """Used to open TimDb connection lazily."""
        if item in DB_PART_NAMES and self.db is None:
            self.open()
        return object.__getattribute__(self, item)

    def open(self):
        global num
        num += 1
        self.num = num
        self.time = time.time()
        log_debug(f"GetDb      {worker_pid:2d} {self.num:6d} {'':2s} {'':3s} {'':7s} {self.route_path:s}")
        # log_info('TimDb-dstr {:2d} {:6d} {:2d} {:3d} {:7.5f} {:s}'.format(worker_pid,self.num, TimDb.instances, bes, time.time() - self.time, self.route_path))
        waiting = False
        from timApp.tim_app import app
        while True:
            try:
                self.engine = db.get_engine(app)
                self.db = self.engine.connect().connection
                self.session = db.session
                break
            except Exception as err:
                if not waiting:
                    log_warning("WaitDb " + str(self.num) + " " + str(err))
                waiting = True
                sleep(0.1)

        if waiting:
            log_warning("ReadyDb " + str(self.num))

        TimDb.instances += 1
        # num_connections = self.get_pg_connections()
        # log_info('TimDb instances/PG connections: {}/{} (constructor)'.format(TimDb.instances, num_connections))
        self.velps = Velps(self.db, self.files_root_path, 'velps', self.current_user_name, self.session)
        self.velp_groups = VelpGroups(self.db, self.files_root_path, 'velp_groups',
                                      self.current_user_name, self.session)
        self.annotations = Annotations(self.db, self.files_root_path, 'annotations',
                                       self.current_user_name, self.session)

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
        db.session.commit()
        if self.db:
            self.db.commit()

    def close(self):
        """Closes the database connection."""
        if hasattr(self, 'db') and self.db is not None:
            bes = -1
            TimDb.instances -= 1
            try:
                # bes = self.get_pg_connections()
                self.db.close()
            except Exception as err:
                log_error('close error: ' + str(self.num) + ' ' + str(err))

            log_debug(
                f'TimDb-dstr {worker_pid:2d} {self.num:6d} {TimDb.instances:2d} {bes:3d} {time.time() - self.time:7.5f} {self.route_path:s}')
            self.reset_attrs()
