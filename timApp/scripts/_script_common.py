import sys

from timdb.timdb2 import TimDb


TIM_FILES = '../tim_files'
DBFILE = TIM_FILES + '/tim.db'
STDOUT = sys.stdout
STDERR = sys.stderr

__TIMDB = None


def get_tim_db():
    global __TIMDB
    if __TIMDB is None:
        global DBFILE, TIM_FILES
        __TIMDB = TimDb(DBFILE, TIM_FILES)
    return __TIMDB


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

