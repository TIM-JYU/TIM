
"""Initializes the TIM database."""

from timdb.timdb2 import TimDb
import os
from timdb.gitclient import GitClient
import ephemeralclient
import sys

def createAdmin(timdb, name, real_name, email):
    user_id = timdb.users.createUser(name, real_name, email)
    user_group = timdb.users.createUserGroup(name)
    timdb.users.addUserToGroup(user_group, user_id)
    timdb.users.addUserToAdmins(user_id)
    return (user_id, user_group)

if __name__ == "__main__":
    abspath = os.path.abspath(__file__)
    dname = os.path.dirname(abspath)
    os.chdir(dname)
    FILES_ROOT_PATH = 'tim_files'
    if os.path.exists(FILES_ROOT_PATH):
        print('tim_files already exists, no need to initialize')
        sys.exit()
    print('initializing tim_files')
    git = GitClient.initRepo(FILES_ROOT_PATH)
    p = ephemeralclient.launch_ephemeral()
    timdb = TimDb(db_path='tim_files/tim.db', files_root_path=FILES_ROOT_PATH)
    timdb.initializeTables()
    timdb.users.createAnonymousAndLoggedInUserGroups()
    (vesa_id, vesa_group) = createAdmin(timdb, 'vesal', 'Vesa Lappalainen', 'vesa.t.lappalainen@jyu.fi')
    doc_id = timdb.documents.createDocument('Testaus 1', vesa_group)
    doc_id2 = timdb.documents.createDocument('Testaus 2', vesa_group)

    createAdmin(timdb, 'tojukarp', 'Tomi Karppinen', 'tomi.j.karppinen@jyu.fi')

    # Grant access to anonymous users
    timdb.users.grantViewAccess(0, doc_id.id)
    timdb.users.grantViewAccess(0, doc_id2.id)
    
    timdb.users.grantEditAccess(0, doc_id2.id)
    
    timdb.notes.addNote(1, doc_id.id, doc_id.hash, 0, 'Tämä on testimuistiinpano.', 'everyone', [])
    timdb.notes.addNote(1, doc_id.id, doc_id.hash, 0, 'Tämä on toinen testimuistiinpano samassa kappaleessa.', 'everyone', [])
    timdb.notes.addNote(1, doc_id.id, doc_id.hash, 0,
                     """Vielä kolmas muistiinpano, jossa on pitkä teksti.
                        Vielä kolmas muistiinpano, jossa on pitkä teksti.
                        Vielä kolmas muistiinpano, jossa on pitkä teksti.
                        Vielä kolmas muistiinpano, jossa on pitkä teksti.
                        Vielä kolmas muistiinpano, jossa on pitkä teksti.
                        Vielä kolmas muistiinpano, jossa on pitkä teksti.""", 'everyone', [])

    p.kill()
