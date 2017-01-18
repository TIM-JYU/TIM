"""Initializes the TIM database."""

import os

import flask_migrate
import sqlalchemy
import sqlalchemy.exc

from documentmodel.docparagraph import DocParagraph
from documentmodel.document import Document
from routes.logger import log_info, enable_loggers
from sql.migrate_to_postgre import perform_migration
from tim_app import app
from timdb import tempdb_models
from timdb.tim_models import AccessType, db
from timdb.timdb2 import TimDb


def postgre_create_database(host, db_name):
    engine = sqlalchemy.create_engine("postgresql://postgres@postgresql-{}:5432/postgres".format(host))
    conn = engine.connect()
    conn.execute("commit")
    try:
        conn.execute('create database "{}"'.format(db_name))
        return True
    except sqlalchemy.exc.ProgrammingError as e:
        if 'already exists' not in str(e):
            raise e
        return False
    finally:
        conn.close()


def initialize_temp_database():
    postgre_create_database('tempdb-' + app.config['TIM_NAME'], 'tempdb_' + app.config['TIM_NAME'])
    tempdb_models.initialize_temp_database()


def initialize_database(create_docs=True):
    abspath = os.path.abspath(__file__)
    dname = os.path.dirname(abspath)
    os.chdir(dname)
    files_root_path = app.config['FILES_PATH']
    Document.default_files_root = files_root_path
    DocParagraph.default_files_root = files_root_path
    was_created = postgre_create_database(app.config['TIM_NAME'], app.config['TIM_NAME'])
    log_info('Database {} {}.'.format(app.config['TIM_NAME'], 'was created' if was_created else 'exists'))
    timdb = TimDb(files_root_path=files_root_path)
    db.create_all(bind='tim_main')
    sess = timdb.session
    if sess.query(AccessType).count() > 0:
        log_info('Initial data already exists, skipping DB initialization.')
    else:
        old_db = app.config['OLD_SQLITE_DATABASE']
        if old_db and os.path.exists(old_db):
            perform_migration(app.config['OLD_SQLITE_DATABASE'], app.config['DATABASE'])
            timdb.close()
            return
        with app.app_context():
            flask_migrate.stamp()
        # Alembic disables loggers for some reason
        enable_loggers()
        sess.add(AccessType(id=1, name='view'))
        sess.add(AccessType(id=2, name='edit'))
        sess.add(AccessType(id=3, name='teacher'))
        sess.add(AccessType(id=4, name='manage'))
        sess.add(AccessType(id=5, name='see answers'))
        sess.add(AccessType(id=6, name='owner'))
        sess.commit()

        timdb.users.create_special_usergroups()
        anon_group = timdb.users.get_anon_group_id()
        timdb.users.create_user_with_group('vesal', 'Vesa Lappalainen', 'vesa.t.lappalainen@jyu.fi', is_admin=True)
        timdb.users.create_user_with_group('tojukarp', 'Tomi Karppinen', 'tomi.j.karppinen@jyu.fi', is_admin=True)
        for i in range(1, 4):
            timdb.users.create_user_with_group('testuser{}'.format(i),
                                               'Test user {}'.format(i),
                                               'test{}@example.com'.format(i),
                                               password='test{}pass'.format(i))
        recovered_docs = timdb.documents.recover_db(timdb.users.get_admin_group_id())

        if recovered_docs > 0:
            print('Recovered {} documents from documents directory.'.format(recovered_docs))
            print('Skipping creating example documents.')

        elif create_docs:
            timdb.documents.create('Testaus 1', anon_group)
            timdb.documents.create('Testaus 2', anon_group)
            timdb.documents.import_document_from_file('example_docs/programming_examples.md',
                                                      'Programming examples',
                                                      anon_group)
            timdb.documents.import_document_from_file('example_docs/mmcq_example.md',
                                                      'Multiple choice plugin example',
                                                      anon_group)
        log_info('Database initialization done.')
    timdb.close()


if __name__ == "__main__":
    initialize_database()
    initialize_temp_database()
