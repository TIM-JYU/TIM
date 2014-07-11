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
    timdb.create()
    doc_id = timdb.documents.importDocument('lecture.markdown', 'Ohjelmointi 1 (ei testimuokkauksia!)')
    timdb.documents.importDocument('lecture.markdown', 'Ohjelmointi 1 (saa testailla vapaasti)')
    uid = timdb.users.createUser('testikäyttäjä')
    gid = timdb.users.createUserGroup('testikäyttäjän ryhmä')
    timdb.users.addUserToGroup(user_id=uid, group_id=gid)
    timdb.notes.addNote(gid, 'Tämä on testimuistiinpano.', doc_id, 2)
    timdb.notes.addNote(gid, 'Tämä on toinen testimuistiinpano samassa kappaleessa.', doc_id, 2)
    timdb.notes.addNote(gid, """Vielä kolmas muistiinpano, jossa on pitkä teksti.
                        Vielä kolmas muistiinpano, jossa on pitkä teksti.
                        Vielä kolmas muistiinpano, jossa on pitkä teksti.
                        Vielä kolmas muistiinpano, jossa on pitkä teksti.
                        Vielä kolmas muistiinpano, jossa on pitkä teksti.
                        Vielä kolmas muistiinpano, jossa on pitkä teksti.""", doc_id, 4)
    