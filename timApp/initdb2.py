from timdb.timdb2 import TimDb
import os


if __name__ == "__main__":
    abspath = os.path.abspath(__file__)
    dname = os.path.dirname(abspath)
    os.chdir(dname)
    timdb = TimDb(db_path='tim_files/tim.db', files_root_path='tim_files')
    timdb.initRepo()
    timdb.create()
    timdb.importDocument('lecture.markdown', 'Ohjelmointi 1')
    timdb.importDocument('lecture.markdown', 'Ohjelmointi1-TESTAILUA')
