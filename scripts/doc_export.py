#!/usr/bin/python3
import os
import sqlite3
import shutil
import tarfile
import tempfile

from _script_common import *
from documentmodel.document import Document
from documentmodel.documentversion import DocumentVersion
from typing import List, Optional


class DocExportError(Exception):
    pass


def parse_cmdline():
    if len(sys.argv) == 2:
        try:
            doc_id = int(sys.argv[1])
            outfile = docker_path('doc_{}.tar.gz'.format(sys.argv[1]))
        except ValueError:
            print("Expected a document id, got " + sys.argv[1])
            exit(1)
    else:
        print("Syntax: {} <doc_id>".format(sys.argv[0]))
        exit(1)

    tmpdir = tempfile.mkdtemp()
    return doc_id, outfile, tmpdir


def desc_table(c, table_name) -> str:
    # SQLite dependent, for now
    c.execute("SELECT sql FROM sqlite_master WHERE type = 'table' AND name = ?", [table_name])
    return c.fetchone()[0]


def create_db(db_path: str, src_db, tables: List[str]):
    db = sqlite3.connect(db_path)
    c = db.cursor()
    src_c = src_db.cursor()
    for table in tables:
        create_statement = desc_table(src_c, table)
        c.execute(create_statement)
    return db


def copy_values(src_c, dest_c, table, condition):
    #print('SELECT * FROM ' + table + ' ' + condition)
    src_c.execute('SELECT * FROM ' + table + ' ' + condition)
    rows = [x for x in src_c.fetchall()]
    cols = [x[0] for x in src_c.description]
    params = ', '.join('?' for _ in cols)
    insert_statement = 'INSERT INTO ' + table + ' (' + ', '.join(cols) + ') VALUES (' + params + ')'
    #print(insert_statement)

    for row in rows:
        dest_c.execute(insert_statement, list(row))


def doc_export(doc_id: int, out_dir: str):
    doc_dir = os.path.join(TIM_FILES_ROOT, 'docs', str(doc_id))
    if not os.path.exists(doc_dir):
        raise DocExportError('Document directory does not exist!'.format(doc_dir))

    timdb = get_timdb()
    if not timdb.documents.exists(doc_id):
        raise DocExportError('Document #{} does not exist!'.format(doc_id))

    stdout('Using temp directory {}'.format(out_dir))

    stdout('Copying document indexes...')
    target_doc_dir = os.path.join(out_dir, 'docs', str(doc_id))
    os.makedirs(os.path.join(out_dir, 'docs'))
    shutil.copytree(doc_dir, target_doc_dir, symlinks=True)

    stdout('Copying document paragraphs...')
    par_dir = os.path.join(TIM_FILES_ROOT, 'pars', str(doc_id))
    if os.path.exists(par_dir):
        target_par_dir = os.path.join(out_dir, 'pars', str(doc_id))
        os.makedirs(os.path.join(out_dir, 'pars'))
        shutil.copytree(par_dir, target_par_dir, symlinks=True)

    stdout('Creating a temporary database...')
    targetdb_path = os.path.join(out_dir, 'tim_part.db')
    targetdb = create_db(targetdb_path, timdb.db, ['Block', 'BlockAccess', 'DocEntry', 'ReadParagraphs', 'User',
                                                   'UserGroup', 'UserGroupMember', 'UserNotes', 'Version'])
    src_cursor = timdb.db.cursor()
    dest_cursor = targetdb.cursor()

    stdout('Copying associated users and user groups...')
    copy_values(src_cursor, dest_cursor, 'UserGroup',
                """WHERE id IN (SELECT UserGroup_id FROM BlockAccess WHERE Block_id = "{0}"
UNION SELECT UserGroup_id FROM ReadParagraphs WHERE doc_id = "{0}"
UNION SELECT UserGroup_id FROM UserNotes WHERE doc_id = "{0}"
)""".format(doc_id))
    targetdb.commit()

    dest_cursor.execute('SELECT id FROM UserGroup')
    user_groups = list(set([str(row[0]) for row in dest_cursor.fetchall() or []]))
    copy_values(src_cursor, dest_cursor, 'UserGroupMember', 'WHERE UserGroup_id IN ({})'.format(', '.join(user_groups)))
    targetdb.commit()

    dest_cursor.execute('SELECT User_id FROM UserGroupMember')
    users = list(set([str(row[0]) for row in dest_cursor.fetchall() or []]))
    copy_values(src_cursor, dest_cursor, 'User', 'WHERE id IN ({})'.format(', '.join(users)))
    targetdb.commit()

    stdout('Copying metadata...')
    copy_values(src_cursor, dest_cursor, 'Block', 'WHERE id = ' + str(doc_id))
    copy_values(src_cursor, dest_cursor, 'BlockAccess', 'WHERE Block_id = ' + str(doc_id))  # UserGroup_id
    copy_values(src_cursor, dest_cursor, 'DocEntry', 'WHERE id = ' + str(doc_id))
    copy_values(src_cursor, dest_cursor, 'Version', 'WHERE id = ' + str(doc_id))
    targetdb.commit()

    stdout('Copying read markings...')
    copy_values(src_cursor, dest_cursor, 'ReadParagraphs', 'WHERE doc_id = ' + str(doc_id)) # UserGroup_id
    targetdb.commit()

    stdout('Copying user notes...')
    copy_values(src_cursor, dest_cursor, 'UserNotes', 'WHERE doc_id = ' + str(doc_id)) # UserGroup_id
    targetdb.commit()

    targetdb.close()


def compress(src_dir: str, dest_file: str, remove_src: bool = True):
    cwd = os.getcwd()
    os.chdir(src_dir)

    with tarfile.open(dest_file, mode='w:gz') as tar:
        for file in os.listdir('.'):
            tar.add(file, recursive=True)

    os.chdir(cwd)
    if remove_src:
        stdout('Removing temporary files...')
        shutil.rmtree(src_dir)


def main():
    doc_id, outfile, tmpdir = parse_cmdline()

    if not os.path.isfile(DBFILE):
        stderr("Can't find {}!".format(DBFILE))
        return

    try:
        print("Using database " + DBFILE)
        print("Exporting document {} to temp dir {}...".format(doc_id, tmpdir))
        doc_export(doc_id, tmpdir)

        print("Compressing to {}...".format(outfile))
        compress(tmpdir, outfile, remove_src=True)

    except DocExportError as e:
        print('Error: ' + str(e))


if __name__ == '__main__':
    main()
    close_stdout()

