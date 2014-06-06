from timdb import TimDb
import os

abspath = os.path.abspath(__file__)
dname = os.path.dirname(abspath)
os.chdir(dname)
testdb = TimDb(db_path='test.db', files_root_path='test_files')
