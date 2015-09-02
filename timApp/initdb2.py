
"""Initializes the TIM database."""

import os
import mkfolders
from timdb.docidentifier import DocIdentifier

from timdb.timdb2 import TimDb
from timdb.timdbbase import blocktypes
from timdb.users import ANONYMOUS_GROUPNAME, ADMIN_GROUPNAME


def create_admin(timdb, name, real_name, email):
    user_id = timdb.users.createUser(name, real_name, email)
    user_group = timdb.users.createUserGroup(name)
    timdb.users.addUserToGroup(user_group, user_id)
    timdb.users.addUserToAdmins(user_id)
    return (user_id, user_group)


def initialize_database():
    abspath = os.path.abspath(__file__)
    dname = os.path.dirname(abspath)
    os.chdir(dname)
    FILES_ROOT_PATH = 'tim_files'
    if os.path.exists(FILES_ROOT_PATH):
        print('tim_files already exists, no need to initialize')
        return
    print('initializing tim_files')
    timdb = TimDb(db_path='tim_files/tim.db', files_root_path=FILES_ROOT_PATH)
    timdb.initialize_tables()
    timdb.users.createAnonymousAndLoggedInUserGroups()
    anon_group = timdb.users.getUserGroupByName(ANONYMOUS_GROUPNAME)
    (vesa_id, vesa_group) = create_admin(timdb, 'vesal', 'Vesa Lappalainen', 'vesa.t.lappalainen@jyu.fi')
    doc_id = timdb.documents.create('Testaus 1', anon_group)
    timdb.documents.create('Testaus 2', anon_group)
    timdb.documents.import_document_from_file('example_docs/programming_examples.md',
                                                     'Programming examples',
                                                     anon_group)
    timdb.documents.import_document_from_file('example_docs/mmcq_example.md',
                                                     'Multiple choice plugin example',
                                                     anon_group)

    create_admin(timdb, 'tojukarp', 'Tomi Karppinen', 'tomi.j.karppinen@jyu.fi')

    anon_group = timdb.users.getUserGroupByName(ANONYMOUS_GROUPNAME)
    # Grant access to anonymous users
    #timdb.users.grantViewAccess(anon_group, doc_id.id)
    #timdb.users.grantViewAccess(anon_group, doc_id2.id)
    
    #timdb.users.grantEditAccess(anon_group, doc_id2.id)
    
    #timdb.notes.addNote(anon_group, doc_id.id, doc_id.hash, 0, 'Tämä on testimuistiinpano.', 'everyone', [])
    #timdb.notes.addNote(anon_group, doc_id.id, doc_id.hash, 0, 'Tämä on toinen testimuistiinpano samassa kappaleessa.', 'everyone', [])
    #timdb.notes.addNote(anon_group, doc_id.id, doc_id.hash, 0,
    #                 """Vielä kolmas muistiinpano, jossa on pitkä teksti.
    #                    Vielä kolmas muistiinpano, jossa on pitkä teksti.
    #                    Vielä kolmas muistiinpano, jossa on pitkä teksti.
    #                    Vielä kolmas muistiinpano, jossa on pitkä teksti.
    #                    Vielä kolmas muistiinpano, jossa on pitkä teksti.
    #                    Vielä kolmas muistiinpano, jossa on pitkä teksti.""", 'everyone', [])


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
    update_dict = {0: update_datamodel}
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


if __name__ == "__main__":
    initialize_database()
