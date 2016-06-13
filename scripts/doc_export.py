#!/usr/bin/python3
import os
import sqlite3
import shutil
import tarfile
import tempfile

from _script_common import *
from documentmodel.document import Document
from documentmodel.documentversion import DocumentVersion
from initdb2 import initialize_database


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

    #stdout('Creating a temporary database...')
    #stdout('Copying metadata...')


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

