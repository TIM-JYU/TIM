import os
import sys
from pathlib import Path

# Import the timApp package
root = Path(__file__).resolve().parents[1].path
sys.path.append(os.path.join(root, 'timApp'))

from timdb.timdb2 import TimDb

TIM_FILES_ROOT = '../timApp/tim_files'
DBFILE = '../timApp/tim_files/tim.db'
STDOUT = sys.stdout
STDERR = sys.stderr
TIMDB = None


def get_timdb():
    global TIMDB
    if TIMDB is None:
        TIMDB = TimDb(TIM_FILES_ROOT)
    return TIMDB


def docker_path(path: str):
    if os.path.isabs(path):
        try:
            timindex = path.rindex('/tim/')
            return '/service/' + path[timindex+5:]
        except ValueError:
            raise IOError('Cannot access the given absolute path from TIM Docker container')
    else:
        if path.startswith('..'):
            raise IOError('Cannot access the given relative path from TIM Docker container')
        return os.path.normpath(os.path.join('/service', path))


def stdout(s=''):
    print(s, file=STDOUT)


def stderr(s=''):
    print(s, file=STDERR)


def set_stdout(new_stdout = None):
    close_stdout()

    global STDOUT
    if stdout is None:
        STDOUT = sys.stdout
    else:
        STDOUT = new_stdout


def close_stdout():
    global STDOUT
    if STDOUT != sys.stdout:
        STDOUT.close()
        STDOUT = sys.stdout

