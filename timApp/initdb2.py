from timdb.timdb2 import TimDb
import os


if __name__ == "__main__":
    abspath = os.path.abspath(__file__)
    dname = os.path.dirname(abspath)
    os.chdir(dname)
    timdb = TimDb(db_path='tim_files/tim.db', files_root_path='tim_files')
    timdb.initRepo()
    timdb.create()
    doc_id = timdb.importDocument('lecture.markdown', 'Ohjelmointi 1 (ei testimuokkauksia!)')
    timdb.importDocument('lecture.markdown', 'Ohjelmointi 1 (saa testailla vapaasti)')
    uid = timdb.createUser('testikäyttäjä')
    gid = timdb.createUserGroup('testikäyttäjän ryhmä')
    timdb.addUserToGroup(user_id=uid, group_id=gid)
    timdb.addNote(gid, 'Tämä on testimuistiinpano.', doc_id, 2)
    timdb.addNote(gid, 'Tämä on toinen testimuistiinpano samassa kappaleessa.', doc_id, 2)
    timdb.addNote(gid, """Vielä kolmas muistiinpano, jossa on pitkä teksti.
                        Vielä kolmas muistiinpano, jossa on pitkä teksti.
                        Vielä kolmas muistiinpano, jossa on pitkä teksti.
                        Vielä kolmas muistiinpano, jossa on pitkä teksti.
                        Vielä kolmas muistiinpano, jossa on pitkä teksti.
                        Vielä kolmas muistiinpano, jossa on pitkä teksti.""", doc_id, 4)
    