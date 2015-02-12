"""
    Diagnoses and fixes paragraph mappings.
"""

from timdb.gitclient import GitClient
from timdb.timdb2 import TimDb
from timdb.timdbbase import DocIdentifier, TimDbException
from ephemeralclient import EphemeralClient, EPHEMERAL_URL

import argparse
import ephemeralclient
import sqlite3
import os
import sys

def getcount(cursor, table_name, condition = None):
    try:
        if condition is None:
            cursor.execute("select * from %s" % table_name)
        else:
            cursor.execute("select * from %s where %s" % (table_name, condition))
        return len(cursor.fetchall())
    except sqlite3.OperationalError:
        return -1

def inform(description, count):
    if count < 0:
        print("The %s table does not exist." % description)
    else:
        print("Found %d %s" % (count, description))

def cprint(text, condition):
    if condition:
        print(text)

def initialize():
    global git
    global FILES_ROOT_PATH
    global timdb
    global cursor
    abspath = os.path.abspath(__file__)
    dname = os.path.dirname(abspath)
    os.chdir(dname)
    FILES_ROOT_PATH = 'tim_files'
    git = GitClient.connect(FILES_ROOT_PATH)
    timdb = TimDb(db_path='tim_files/tim.db', files_root_path=FILES_ROOT_PATH)
    cursor = timdb.db.cursor()

def get_all_mappings(doc_id):
    cursor.execute("SELECT * FROM ParMappings WHERE doc_id = ?", [doc_id])
    return timdb.documents.resultAsDictionary(cursor)

def get_all_references(doc_id):
    cursor.execute("SELECT doc_ver, par_index FROM ReadParagraphs WHERE doc_id = ?", [doc_id])
    refs = cursor.fetchall()
    cursor.execute("SELECT doc_ver, par_index FROM UserNotes WHERE doc_id = ?", [doc_id])
    refs += cursor.fetchall()
    return refs

def ref_in_mappings(ref, mappings):
    for m in mappings:
        if m['doc_ver'] == ref[0] and m['par_index'] == ref[1]:
            return True
    return False

def vpstr(ver, par):
    if ver == None and par == None:
        return None
    if ver == None:
        return "None[{}]".format(par)
    return "{}[{}]".format(ver[:6], par)

def startEphemeral():
    if not os.environ.get("WERKZEUG_RUN_MAIN") == "true":
        p = ephemeralclient.launch_ephemeral(ignore_signals='pudb' in sys.modules)
    global ec
    ec = EphemeralClient(EPHEMERAL_URL)

def process(doc_ids, fix=False, verbose=False):
    initialize()
    if ('all' in doc_ids):
        doc_ids = [doc['id'] for doc in timdb.documents.getDocuments()]

    if fix:
        startEphemeral()

    for sdoc_id in doc_ids:
        try:
            doc_id = int(sdoc_id)
        except ValueError:
            print(sdoc_id, "is not an integer value, skipping.")
            continue

        doc_vers = timdb.documents.getDocumentVersions(doc_id)
        refs = get_all_references(doc_id)
        mappings = get_all_mappings(doc_id)
        vmappings = {}
        vrefs = {}

        if verbose:
            print("== Document {} ==".format(doc_id))
            print("{} versions, {} readings+notes, {} mappings in total.".format(len(doc_vers), len(refs), len(mappings)))

        for ver_index in range(len(doc_vers) - 1, -1, -1):
            doc_ver = doc_vers[ver_index]['hash']
            vmappings[doc_ver] = [m for m in mappings if m['doc_ver'] == doc_ver]
            mappings = [m for m in mappings if m['doc_ver'] != doc_ver]
            vrefs[doc_ver] = [r for r in refs if r[0] == doc_ver]
            refs = [r for r in refs if r[0] != doc_ver]
            if verbose:
                print("...Version {} has {} readings+notes and {} mapping(s).".format(doc_ver[:6], len(vrefs[doc_ver]), len(vmappings[doc_ver])))

        if verbose:
            print()

        nonverbosestr = " in document {}".format(doc_id) if not verbose else ""

        if len(mappings) > 0:
            n = len(mappings)
            print("Detected {} orphaned mapping{}{}.".format(n, "s" if n > 1 else "", nonverbosestr))
            for m in mappings:
                print("...{} -> {}".format(vpstr(m["doc_ver"], m["par_index"]), vpstr(m["new_ver"], m["new_index"])))
            print()

        if len(refs) > 0:
            n = len(refs)
            print("Detected {0} orphaned reading{1} and/or note{1}{2}.".format(n, "s" if n != 1 else "", nonverbosestr))

        if verbose:
            print("Checking readings and notes")
        unmapped = 0
        build_mappings = []
        for ver_index in range(0, 0, -1):
            doc_ver = doc_vers[ver_index]["hash"]
            next_ver = doc_vers[ver_index-1]["hash"]

            for ref in vrefs[doc_ver]:
                if not ref_in_mappings(ref, vmappings):
                    unmapped += 1
                    if fix:
                        cur_id = DocIdentifier(doc_id, doc_ver)
                        next_id = DocIdentifier(doc_id, next_ver)
                        build_mappings.append((cur_id, next_id, ref[1]))

        if unmapped > 0:
            print("Found {} unmapped reference{}{}.".format(unmapped, "s" if unmapped != 1 else "", nonverbosestr))

        added = 0
        skipped = 0
        while len(build_mappings) > 0:
            print("Badger")
            (cur_id, next_id, par_index) = build_mappings.pop()
            affinities = ec.getSingleBlockMapping(cur_id, next_id, par_index)
            [affinity, new_index] = max(affinities, key=lambda x: x[0] if x[0] is not None else 0)
            if affinity > 0.5:
                timdb.documents.addParMapping(cur_id, next_id, par_index, new_index, str(affinity < 1), commit=False)
                build_mappings.append(new_index)
                added += 1
            else:
                print("Skipping paragraph ")

        if added > 0 or skipped > 0:
            timdb.commit()
            print("Added {} mapping{} ({} below affinity).".format(added, "s" if added != 1 else "", skipped))

        if verbose:
            print()



if __name__ == "__main__":
    parser = argparse.ArgumentParser(description="Diagnoses and fixes paragraph mappins.")
    parser.add_argument('doc_id', metavar='doc_id', nargs='+', help='Document id, or "all" for all documents.')
    parser.add_argument('-r', '--rebuild', action='store_true', help='Specify this to rebuild all mappings.')
    parser.add_argument('-v', '--verbose', action='store_true', help='Verbose output.')
    args = parser.parse_args()
    process(args.doc_id, args.rebuild, args.verbose)
