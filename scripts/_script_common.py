import sys

DBFILE = '../timApp/tim_files/tim.db'
STDOUT = sys.stdout
STDERR = sys.stderr


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

