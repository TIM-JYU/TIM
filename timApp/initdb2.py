
"""Initializes the TIM database."""

from timdb.timdb2 import TimDb
import os
from timdb.gitclient import initRepo


if __name__ == "__main__":
    abspath = os.path.abspath(__file__)
    dname = os.path.dirname(abspath)
    os.chdir(dname)
    FILES_ROOT_PATH = 'tim_files'
    timdb = TimDb(db_path='tim_files/tim.db', files_root_path=FILES_ROOT_PATH)
    initRepo(FILES_ROOT_PATH)
    timdb.initializeTables()
    timdb.users.createAnonymousUser()
    vesa_id = timdb.users.createUser('vesal')
    vesa_group = timdb.users.createUserGroup('vesal')
    timdb.users.addUserToGroup(vesa_group, vesa_id)
    doc_id = timdb.documents.importDocumentFromFile('lecture.markdown', 'Ohjelmointi 1', vesa_group)
    doc_id2 = timdb.documents.importDocumentFromFile('lecture.markdown', 'Ohjelmointi 1 (saa testailla vapaasti)', vesa_group)
    
    # Grant access to anonymous users
    timdb.users.grantViewAccess(0, doc_id.id)
    timdb.users.grantViewAccess(0, doc_id2.id)
    
    timdb.users.grantEditAccess(0, doc_id2.id)
    
    timdb.notes.addNote(0, 'Tämä on testimuistiinpano.', doc_id.id, 2)
    timdb.notes.addNote(0, 'Tämä on toinen testimuistiinpano samassa kappaleessa.', doc_id.id, 2)
    timdb.notes.addNote(0, """Vielä kolmas muistiinpano, jossa on pitkä teksti.
                        Vielä kolmas muistiinpano, jossa on pitkä teksti.
                        Vielä kolmas muistiinpano, jossa on pitkä teksti.
                        Vielä kolmas muistiinpano, jossa on pitkä teksti.
                        Vielä kolmas muistiinpano, jossa on pitkä teksti.
                        Vielä kolmas muistiinpano, jossa on pitkä teksti.""", doc_id.id, 4)
    