
"""Initializes the TIM database."""

from timdb.timdb2 import TimDb
import os
from timdb.gitclient import GitClient
import ephemeralclient
import sys
from timdb.users import ANONYMOUS_GROUPNAME


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
    doc_id = timdb.documents.create_document('Testaus 1', anon_group)
    timdb.documents.create_document('Testaus 2', anon_group)
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

if __name__ == "__main__":
    initialize_database()
