"""Initializes the TIM database."""

import os

import sqlalchemy
import sqlalchemy.exc

import models
from tim_app import app
from timdb.timdb2 import TimDb
from timdb.timdbbase import TimDbException
from timdb.users import LOGGED_IN_USERNAME


def postgre_create_database(db_name):
    # app.config['SQLALCHEMY_DATABASE_URI'] = "postgresql://docker:docker@postgre:5432/tempdb_" + timname
    engine = sqlalchemy.create_engine("postgresql://docker:docker@postgre:5432/postgres")
    conn = engine.connect()
    conn.execute("commit")
    try:
        conn.execute("create database " + db_name)
    except sqlalchemy.exc.ProgrammingError as e:
        if 'already exists' not in str(e):
            raise e
    conn.close()


def initialize_temp_database():
    postgre_create_database('tempdb_' + app.config['TIM_NAME'])
    models.initialize_temp_database()


def initialize_database(db_path='tim_files/tim.db', files_root_path='tim_files', create_docs=True, print_progress=True):
    abspath = os.path.abspath(__file__)
    dname = os.path.dirname(abspath)
    os.chdir(dname)
    if os.path.exists(db_path):
        if print_progress:
            print('{} already exists, no need to initialize'.format(files_root_path))
        return
    if print_progress:
        print('initializing the database in {}...'.format(files_root_path), end='')
    timdb = TimDb(db_path=db_path, files_root_path=files_root_path)
    timdb.initialize_tables()
    timdb.users.createAnonymousAndLoggedInUserGroups()
    anon_group = timdb.users.get_anon_group_id()
    timdb.users.create_user_with_group('vesal', 'Vesa Lappalainen', 'vesa.t.lappalainen@jyu.fi', is_admin=True)
    timdb.users.create_user_with_group('tojukarp', 'Tomi Karppinen', 'tomi.j.karppinen@jyu.fi', is_admin=True)
    timdb.users.create_user_with_group('testuser1', 'Test user 1', 'test1@example.com', password='test1pass')
    timdb.users.create_user_with_group('testuser2', 'Test user 2', 'test2@example.com', password='test2pass')

    if create_docs:
        timdb.documents.create('Testaus 1', anon_group)
        timdb.documents.create('Testaus 2', anon_group)
        timdb.documents.import_document_from_file('example_docs/programming_examples.md',
                                                  'Programming examples',
                                                  anon_group)
        timdb.documents.import_document_from_file('example_docs/mmcq_example.md',
                                                  'Multiple choice plugin example',
                                                  anon_group)
    timdb.close()
    if print_progress:
        print(' done.')


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
    timdb = TimDb(db_path='tim_files/tim.db', files_root_path='tim_files')
    ver = timdb.get_version()
    ver_old = ver
    update_dict = {0: update_datamodel,
                   1: update_answers,
                   2: update_rights,
                   3: add_seeanswers_right,
                   4: add_translation_table,
                   5: add_logged_in_user}
    while ver in update_dict:
        # TODO: Take automatic backup of the db (tim_files) before updating
        print('Starting update {}'.format(update_dict[ver].__name__))
        result = update_dict[ver](timdb)
        if not result:
            print('Update {} was skipped.'.format(update_dict[ver].__name__))
        else:
            print('Update {} was completed.'.format(update_dict[ver].__name__))
        timdb.update_version()
        ver = timdb.get_version()
    if ver_old == ver:
        print('Database is up to date.')
    else:
        print('Database was updated from version {} to {}.'.format(ver_old, ver))
    timdb.close()


def add_logged_in_user(timdb):
    lu = timdb.users.getUserByName(LOGGED_IN_USERNAME)
    if lu is not None:
        return False
    uid = timdb.users.createUser(LOGGED_IN_USERNAME, LOGGED_IN_USERNAME, '')
    timdb.users.addUserToGroup(timdb.users.get_logged_group_id(), uid)
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
