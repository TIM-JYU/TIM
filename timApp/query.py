#!/usr/bin/python3
import sqlite3
import os
import sys
import time

DBFILE = 'tim_files/tim.db'
STDOUT = sys.stdout
STDERR = sys.stderr


def show_tables(db):
    c = db.cursor()
    c.execute("SELECT name, sql FROM sqlite_master WHERE type = 'table'")
    stderr("These are the tables in the database:")
    stderr([row[0] for row in c.fetchall()])
    stderr()


def command_help(db = None, params=None):
    stderr('Commands:')
    stderr('---')
    stderr('DESC <table>          - show the SQL used to create a table')
    stderr('HELP or ?             - show this command help')
    stderr('OUT <filename|stdout> - set command output')
    stderr('RUN <command>         - execute a shell command')
    stderr('SHOW TABLES           - list all tables')
    stderr('QUIT                  - quit the program')
    stderr('---')
    stderr('Any other commands are interpreted as SQL and executed as such.')
    stderr('Note: remember to end SQL queries with a semicolon.')
    stderr()


def stdout(s=''):
    print(s, file=STDOUT)


def stderr(s=''):
    print(s, file=STDERR)


def command_show(db, params):
    # Only "show tables" for now
    show_tables(db)


def command_desc(db, params):
    c = db.cursor()
    for table_name in params:
        c.execute("SELECT sql FROM sqlite_master WHERE type = 'table' AND name = ?", [table_name])
        for row in c.fetchall():
            stderr(row[0])
        stderr()


def command_out(db, params):
    if len(params) != 1:
        stderr('Syntax: OUT <filename|stdout>\n')
        return

    global STDOUT
    STDOUT = sys.stdout if params[0].lower() == 'stdout' else open(params[0], 'w')


def interpret_sql(db, command):
    c = db.cursor()
    try:
        t0 = time.time()
        c.execute(command)
        n = 0
        for row in c.fetchall():
            stdout(','.join([str(col) for col in row]))
            n += 1
        stderr('{0} row{1} returned in {2:.5} seconds.'.format(n, '' if n == 1 else 's', time.time() - t0))
        db.commit()
    except Exception as e:
        stderr('EXCEPTION {}: {}'.format(e.__class__, str(e)))

    stderr()


def interpret_command(db, cmdline):
    cmds = cmdline.split()
    if len(cmds) == 0:
        return True

    cmd = cmds[0].lower()
    params = cmds[1:]

    if cmd == 'quit':
        return False

    COMMANDS = {'help': command_help, '?': command_help, 'show': command_show, 'desc': command_desc, 'out': command_out}
    if cmd in COMMANDS:
        COMMANDS[cmd](db, params)
    else:
        query = cmdline
        while not query.rstrip().endswith(';'):
            query += ' ' + input().strip()
        interpret_sql(db, query)

    return True


def parse_cmdline():
    if len(sys.argv) == 1:
        pass
    elif len(sys.argv) == 2:
        global DBFILE
        DBFILE = sys.argv[1]
    else:
        print("Syntax: {} [database file]".format(sys.argv[0]))
        print("If none is specified, {} is used instead.".format(DBFILE))
        exit(1)


def main():
    parse_cmdline()

    if not os.path.isfile(DBFILE):
        stderr("Can't find {}!".format(DBFILE))
        return
    db = sqlite3.Connection(DBFILE)

    print("Using database " + DBFILE)
    show_tables(db)
    command_help()

    try:
        while interpret_command(db, input('tim.db > ')):
            pass
    except (EOFError, KeyboardInterrupt):
        pass

    db.close()

if __name__ == '__main__':
    main()
    STDOUT.close()
    STDERR.close()
