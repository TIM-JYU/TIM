#!/usr/bin/python3
import sqlite3
import os
import shutil
import tempfile

from _script_common import *


def parse_cmdline():
    if len(sys.argv) == 2:
        try:
            doc_id = int(sys.argv[1])
            outfile = 'doc_' + sys.argv[1]
        except ValueError:
            print("Expected a document id, got " + sys.argv[1])
            exit(1)
    else:
        print("Syntax: {} <doc_id>".format(sys.argv[0]))
        exit(1)

    tmpdir = tempfile.mkdtemp()
    return doc_id, outfile, tmpdir


def doc_export(src_db, doc_id: int, outfile: str):
    doc_dir = os.path.join(TIM_FILES, 'docs')
    shutil.copytree()


def compress(src_dir: str, dest_file: str, remove_src: bool = True):
    if remove_src:
        os.rmdir(src_dir)


def main():
    doc_id, outfile, tmpdir = parse_cmdline()

    if not os.path.isfile(DBFILE):
        stderr("Can't find {}!".format(DBFILE))
        return
    db = sqlite3.Connection(DBFILE)

    print("Using database " + DBFILE)
    print("Exporting document {} to temp dir {}...".format(doc_id, tmpdir))
    doc_export(db, doc_id, tmpdir)
    db.close()

    print("Compressing to {}...".format(outfile))
    compress(tmpdir, outfile, remove_src=True)


if __name__ == '__main__':
    main()
    close_stdout()

