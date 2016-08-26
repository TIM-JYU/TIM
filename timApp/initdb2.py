"""Initializes the TIM database."""

import os

import sqlalchemy
import sqlalchemy.exc

from documentmodel.docparagraph import DocParagraph
from documentmodel.document import Document
from routes.logger import log_info
from sql.migrate_to_postgre import perform_migration
from tim_app import app, db
from timdb import tempdb_models
from timdb.tim_models import Version, AccessType
from timdb.timdb2 import TimDb
from timdb.timdbbase import TimDbException
from timdb.users import LOGGED_IN_USERNAME

NEWEST_DB_VERSION = 9


def postgre_create_database(db_name):
    engine = sqlalchemy.create_engine("postgresql://postgres@postgresql:5432/postgres")
    conn = engine.connect()
    conn.execute("commit")
    try:
        conn.execute("create database " + db_name)
        return True
    except sqlalchemy.exc.ProgrammingError as e:
        if 'already exists' not in str(e):
            raise e
        return False
    finally:
        conn.close()


def initialize_temp_database():
    postgre_create_database('tempdb_' + app.config['TIM_NAME'])
    tempdb_models.initialize_temp_database()


def initialize_database(create_docs=True):
    abspath = os.path.abspath(__file__)
    dname = os.path.dirname(abspath)
    os.chdir(dname)
    db_path = app.config['DATABASE']
    files_root_path = app.config['FILES_PATH']
    Document.default_files_root = files_root_path
    DocParagraph.default_files_root = files_root_path
    was_created = postgre_create_database(app.config['TIM_NAME'])
    log_info('Database {} {}.'.format(app.config['TIM_NAME'], 'was created' if was_created else 'exists'))
    timdb = TimDb(db_path=db_path, files_root_path=files_root_path)
    db.create_all(bind='tim_main')
    sess = timdb.session
    if sess.query(AccessType).count() > 0:
        log_info('Initial data already exists, skipping DB initialization.')
    else:
        old_db = app.config['OLD_SQLITE_DATABASE']
        if old_db and os.path.exists(old_db):
            perform_migration(app.config['OLD_SQLITE_DATABASE'], app.config['DATABASE'])
            return
        sess.add(AccessType(id=1, name='view'))
        sess.add(AccessType(id=2, name='edit'))
        sess.add(AccessType(id=3, name='teacher'))
        sess.add(AccessType(id=4, name='manage'))
        sess.add(AccessType(id=5, name='see answers'))
        sess.add(Version(version_id=NEWEST_DB_VERSION))
        sess.commit()

        timdb.users.create_special_usergroups()
        anon_group = timdb.users.get_anon_group_id()
        timdb.users.create_user_with_group('vesal', 'Vesa Lappalainen', 'vesa.t.lappalainen@jyu.fi', is_admin=True)
        timdb.users.create_user_with_group('tojukarp', 'Tomi Karppinen', 'tomi.j.karppinen@jyu.fi', is_admin=True)
        timdb.users.create_user_with_group('testuser1', 'Test user 1', 'test1@example.com', password='test1pass')
        timdb.users.create_user_with_group('testuser2', 'Test user 2', 'test2@example.com', password='test2pass')
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


def update_database():
    """Updates the database structure if needed.

    The dict `update_dict` is a dictionary that describes which database versions need which update.
    For example, if the current db version is 0, update_datamodel method will be called and also all other methods
    whose key in the dictionary is greater than 0.

    To add a new update method, create a new method in this file that performs the required updating steps and then
    add a new entry "key: val" to the `update_dict` dictionary where "key" is one larger than the currently largest
    key in the dictionary and "val" is the reference to the method you created.

    The update method should return True if the update was applied or False if it was skipped for some reason.
    """
    timdb = TimDb(db_path=app.config['DATABASE'], files_root_path=app.config['FILES_PATH'])
    ver = timdb.get_version()
    ver_old = ver
    update_dict = {0: update_datamodel,
                   1: update_answers,
                   2: update_rights,
                   3: add_seeanswers_right,
                   4: add_translation_table,
                   5: add_logged_in_user,
                   6: add_notifications,
                   7: add_yubikey,
                   8: add_timber}
    while ver in update_dict:
        # TODO: Take automatic backup of the db (tim_files) before updating
        log_info('Starting update {}'.format(update_dict[ver].__name__))
        result = update_dict[ver](timdb)
        if not result:
            log_info('Update {} was skipped.'.format(update_dict[ver].__name__))
        else:
            log_info('Update {} was completed.'.format(update_dict[ver].__name__))
        timdb.update_version()
        ver = timdb.get_version()
    if ver_old == ver:
        log_info('Database is up to date.')
    else:
        log_info('Database was updated from version {} to {}.'.format(ver_old, ver))
    timdb.close()


def add_timber(timdb: TimDb) -> bool:
    print('SQLAlchemy adds timber tables automatically.')
    return True


def add_notifications(timdb):
    if timdb.table_exists('Notification'):
        return False

    timdb.execute_sql("""
CREATE TABLE Notification (
  user_id   INTEGER NOT NULL,
  doc_id    INTEGER NOT NULL,

  email_doc_modify      BOOLEAN NOT NULL DEFAULT FALSE,
  email_comment_add     BOOLEAN NOT NULL DEFAULT FALSE,
  email_comment_modify  BOOLEAN NOT NULL DEFAULT FALSE,

  CONSTRAINT Notification_PK
  PRIMARY KEY (user_id, doc_id),

  CONSTRAINT Notification_docid
  FOREIGN KEY (doc_id)
  REFERENCES Block (id)
  ON DELETE CASCADE
  ON UPDATE CASCADE
);
""")

    return True

def add_yubikey(timdb):
    timdb.execute_sql('ALTER TABLE User ADD COLUMN yubikey VARCHAR(12)')

def add_logged_in_user(timdb):
    lu = timdb.users.get_user_id_by_name(LOGGED_IN_USERNAME)
    if lu is not None:
        return False
    uid = timdb.users.create_user(LOGGED_IN_USERNAME, LOGGED_IN_USERNAME, '')
    timdb.users.add_user_to_group(timdb.users.get_logged_group_id(), uid)
    return True


def add_translation_table(timdb):
    if timdb.table_exists('Translation'):
        return False
    timdb.execute_sql("""
CREATE TABLE Translation (
  doc_id      INTEGER      NOT NULL,
  src_docid   INTEGER      NOT NULL,
  lang_id     INTEGER      NOT NULL,
  doc_title   VARCHAR(50),

  CONSTRAINT Translation_PK
  PRIMARY KEY (doc_id),

  CONSTRAINT Translation_id
  FOREIGN KEY (doc_id)
  REFERENCES Block (id)
  ON DELETE CASCADE
  ON UPDATE CASCADE,

  CONSTRAINT Translation_src_docid
  FOREIGN KEY (src_docid)
  REFERENCES Block (id)
  ON DELETE CASCADE
  ON UPDATE CASCADE
);""")
    return True


def add_seeanswers_right(timdb):
    timdb.execute_sql("""
INSERT INTO AccessType(name) VALUES ('see answers')
    """)
    return True


def update_rights(timdb):
    timdb.execute_sql("""
BEGIN TRANSACTION;

CREATE TABLE BlockAccess (
  accessible_from TIMESTAMP NOT NULL,
  accessible_to   TIMESTAMP,
  Block_id      INTEGER   NOT NULL,
  UserGroup_id  INTEGER   NOT NULL,
  type INTEGER NOT NULL,

  CONSTRAINT BlockAccess_PK
  PRIMARY KEY (Block_id, UserGroup_id, type),

  CONSTRAINT BlockAccess_id
  FOREIGN KEY (Block_id)
  REFERENCES Block (id)
  ON DELETE NO ACTION
  ON UPDATE CASCADE,

  CONSTRAINT BlockAccess_id
  FOREIGN KEY (UserGroup_id)
  REFERENCES UserGroup (id)
  ON DELETE NO ACTION
  ON UPDATE CASCADE,

  FOREIGN KEY (type)
  REFERENCES AccessType(id)
  ON DELETE NO ACTION
  ON UPDATE CASCADE
);

CREATE TABLE AccessType (
  id INTEGER PRIMARY KEY,
  name TEXT NOT NULL
);

INSERT INTO AccessType(id, name) VALUES (1, 'view');
INSERT INTO AccessType(id, name) VALUES (2, 'edit');
INSERT INTO AccessType(id, name) VALUES (3, 'teacher');
INSERT INTO AccessType(id, name) VALUES (4, 'manage');

INSERT INTO BlockAccess(accessible_from, accessible_to, Block_id, UserGroup_id, type)
SELECT visible_from, visible_to, Block_id, UserGroup_id, 1
FROM BlockViewAccess;

INSERT INTO BlockAccess(accessible_from, accessible_to, Block_id, UserGroup_id, type)
SELECT editable_from, editable_to, Block_id, UserGroup_id, 2
FROM BlockEditAccess;

DROP TABLE BlockViewAccess;
DROP TABLE BlockEditAccess;

COMMIT TRANSACTION;
    """)
    return True


# noinspection PyUnusedLocal
def update_datamodel(timdb):
    raise TimDbException('This update is obsolete.')


def update_answers(timdb):
    timdb.execute_sql("""ALTER TABLE Answer ADD COLUMN valid BOOLEAN""")
    timdb.execute_sql("""UPDATE Answer SET valid = 1 WHERE id IN
(SELECT Answer.id
FROM Answer
JOIN UserAnswer ON UserAnswer.answer_id = Answer.id
WHERE Answer.content LIKE '[%'
GROUP BY UserAnswer.user_id, Answer.task_id
HAVING answered_on = MIN(answered_on)
ORDER BY Answer.content)""")
    timdb.execute_sql("""UPDATE Answer SET valid = 0 WHERE id IN
(SELECT Answer.id
FROM Answer
WHERE Answer.content LIKE '[%' AND valid IS NULL)
""")
    timdb.execute_sql("""UPDATE Answer SET valid = 1 WHERE id IN
(SELECT Answer.id
FROM Answer
WHERE valid IS NULL)
""")
    return True


if __name__ == "__main__":
    initialize_database()
    initialize_temp_database()
