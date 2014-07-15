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
    timdb.users.createAnonymousUser()
    doc_id = timdb.documents.importDocument('lecture.markdown', 'Ohjelmointi 1 (ei testimuokkauksia!)', 0)
    timdb.documents.importDocument('lecture.markdown', 'Ohjelmointi 1 (saa testailla vapaasti)', 0)
    timdb.users.grantViewAccess(0, doc_id)
    timdb.notes.addNote(0, 'Tämä on testimuistiinpano.', doc_id, 2)
    timdb.notes.addNote(0, 'Tämä on toinen testimuistiinpano samassa kappaleessa.', doc_id, 2)
    timdb.notes.addNote(0, """Vielä kolmas muistiinpano, jossa on pitkä teksti.
                        Vielä kolmas muistiinpano, jossa on pitkä teksti.
                        Vielä kolmas muistiinpano, jossa on pitkä teksti.
                        Vielä kolmas muistiinpano, jossa on pitkä teksti.
                        Vielä kolmas muistiinpano, jossa on pitkä teksti.
                        Vielä kolmas muistiinpano, jossa on pitkä teksti.""", doc_id, 4)
    