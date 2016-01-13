
"""Initializes the TIM database."""

import os
import mkfolders
import sqlalchemy
from timdb.docidentifier import DocIdentifier

from timdb.timdb2 import TimDb
from timdb.timdbbase import blocktypes
from timdb.users import ANONYMOUS_GROUPNAME, ADMIN_GROUPNAME
from tim_app import app
import models


def create_user(timdb, name, real_name, email, password='', is_admin=False):
    user_id = timdb.users.createUser(name, real_name, email, password=password)
    user_group = timdb.users.createUserGroup(name)
    timdb.users.addUserToGroup(user_group, user_id)
    if is_admin:
        timdb.users.addUserToAdmins(user_id)
    return user_id, user_group

def postgre_create_database(db_name):
    #app.config['SQLALCHEMY_DATABASE_URI'] = "postgresql://docker:docker@postgre:5432/tempdb_" + timname
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
    anon_group = timdb.users.getUserGroupByName(ANONYMOUS_GROUPNAME)
    create_user(timdb, 'vesal', 'Vesa Lappalainen', 'vesa.t.lappalainen@jyu.fi', is_admin=True)
    create_user(timdb, 'tojukarp', 'Tomi Karppinen', 'tomi.j.karppinen@jyu.fi', is_admin=True)
    create_user(timdb, 'testuser1', 'Test user 1', 'test1@example.com', password='test1pass')
    create_user(timdb, 'testuser2', 'Test user 2', 'test2@example.com', password='test2pass')

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
                   3: add_seeanswers_right}
    while ver in update_dict:
        # TODO: Take automatic backup of the db (tim_files) before updating
        print('Starting update {}'.format(update_dict[ver].__name__))
        result = update_dict[ver]()
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


def add_seeanswers_right():
    timdb = TimDb(db_path='tim_files/tim.db', files_root_path='tim_files')
    timdb.execute_sql("""
INSERT INTO AccessType(name) VALUES ('see answers')
    """)
    return True


def update_rights():
    timdb = TimDb(db_path='tim_files/tim.db', files_root_path='tim_files')
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


def update_datamodel():
    timdb = TimDb(db_path='tim_files/tim.db', files_root_path='tim_files')
    if not timdb.table_exists('Folder'):
        print('Executing SQL script update_to_datamodel...', end="", flush=True)
        timdb.execute_script('sql/update_to_datamodel.sql')
        print(' done.', flush=True)
    if not timdb.table_exists('Question'):
        print('Executing SQL script update_to_timppa...', end="", flush=True)
        timdb.execute_script('sql/update_to_timppa.sql')
        print(' done.', flush=True)
    if not timdb.table_exists('Version'):
        print('Creating Version table...', end="", flush=True)
        timdb.execute_sql("""
CREATE TABLE Version (
  id INTEGER NOT NULL PRIMARY KEY,
  updated_on TIMESTAMP
);

INSERT INTO Version(updated_on, id) VALUES (CURRENT_TIMESTAMP, 0);
        """)
        print(' done.', flush=True)
    doc_ids = timdb.db.execute("""SELECT id FROM Block WHERE type_id = ?""", [blocktypes.DOCUMENT]).fetchall()
    for doc_id, in doc_ids:
        print('Migrating document {}...'.format(doc_id), end="", flush=True)
        try:
            timdb.documents.get_document_with_autoimport(DocIdentifier(doc_id, ''))
        except FileNotFoundError:
            print(' document was not found from file system, skipping.')
        print(' done.', flush=True)
    admin_group_id = timdb.users.getUserGroupByName(ADMIN_GROUPNAME)
    if admin_group_id is None:
        print('Administrators usergroup is missing, adding...', end="", flush=True)
        timdb.db.execute('INSERT INTO UserGroup (name) VALUES (?)', [ADMIN_GROUPNAME])
        timdb.db.commit()
        print(' done.', flush=True)
    mkfolders.update_tables(timdb.db)
    return True


def update_answers():
    timdb = TimDb(db_path='tim_files/tim.db', files_root_path='tim_files')
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

