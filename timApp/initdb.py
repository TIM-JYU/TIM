from timdb import TimDb
import os

if __name__ == "__main__":
    abspath = os.path.abspath(__file__)
    dname = os.path.dirname(abspath)
    os.chdir(dname)
    timdb = TimDb(db_path='tim.db', files_root_path='tim_files')
    timdb.create()