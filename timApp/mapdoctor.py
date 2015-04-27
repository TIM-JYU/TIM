"""
    Diagnoses and fixes paragraph mappings.
"""

from timdb.timdb2 import TimDb
from timdb.timdbbase import DocIdentifier, TimDbException
from ephemeralclient import EphemeralClient, EPHEMERAL_URL

import argparse
import ephemeralclient
import sqlite3
import os
import sys
import difflib

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

def getNextVersion(versions, doc_ver):
    for i in range(0, len(versions)):
        if (versions[i]['hash'] == doc_ver):
            return versions[i - 1]['hash'] if i > 0 else None
    return None

def getClosestIndex(texts, text, prevIndex, cutoff=0.5):
    closest = (None, 0)
    for i in range(0, len(texts)):
        aff = difflib.SequenceMatcher(lambda x: x == " ", text, texts[i]).ratio()
    if aff > closest[1] or (aff == closest[1] and closest[0] is not None and abs(prevIndex - i) < abs(prevIndex - closest[0])):
        closest = (i, aff)
    return closest

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

        doc_vers = timdb.documents.getDocumentVersions(doc_id, 500)
        refs = get_all_references(doc_id)
        mappings = get_all_mappings(doc_id)
        vmappings = {}
        vrefs = {}

        if verbose:
            print("== Document {} ==".format(doc_id))
            print("{} versions, {} readings+notes, {} mappings in total.".format(len(doc_vers), len(refs), len(mappings)))

        #for ver_index in range(len(doc_vers) - 1, -1, -1):
        #    doc_ver = doc_vers[ver_index]['hash']
        #    vmappings[doc_ver] = [m for m in mappings if m['doc_ver'] == doc_ver]
        #    mappings = [m for m in mappings if m['doc_ver'] != doc_ver]
        #    vrefs[doc_ver] = [r for r in refs if r[0] == doc_ver]
        #    refs = [r for r in refs if r[0] != doc_ver]
        #    if verbose:
        #        print("   Version {} has {} readings+notes and {} mapping(s).".format(doc_ver[:6], len(vrefs[doc_ver]), len(vmappings[doc_ver])))

        if verbose:
            print()

        nonverbosestr = " in document {}".format(doc_id) if not verbose else ""

        #if len(mappings) > 0:
        #    n = len(mappings)
        #    print("Detected {} orphaned mapping{}{}.".format(n, "s" if n > 1 else "", nonverbosestr))
        #    for m in mappings:
        #        print("   {} -> {}".format(vpstr(m["doc_ver"], m["par_index"]), vpstr(m["new_ver"], m["new_index"])))
        #    print()

        #if len(refs) > 0:
        #    n = len(refs)
        #    print("Detected {0} orphaned reading{1} and/or note{1}{2}.".format(n, "s" if n != 1 else "", nonverbosestr))

        if verbose:
            print("Checking readings and notes")

        loose = 0
        fixed = 0
        for ref in get_all_references(doc_id):
            current_ver = ref[0]
            current_par = ref[1]
            while current_ver != doc_vers[0]["hash"] and current_par is not None:
                cursor.execute(
                    """
                    select new_ver, new_index, modified
                    from ParMappings
                    where doc_id = ? and doc_ver = ? and par_index = ?
                    and new_ver is not null and new_index is not null
                    """, [doc_id, current_ver, current_par])
                mappings = timdb.documents.resultAsDictionary(cursor)
                if len(mappings) == 0:
                    #if verbose:
                    #    print('Loose end: doc {} {}({}) -> ???'.format (doc_id, current_ver[:6], current_par))
                    loose += 1
                    if fix:
                        next_ver = getNextVersion(doc_vers, current_ver)
                        cur_id = DocIdentifier(doc_id, current_ver)
                        next_id = DocIdentifier(doc_id, next_ver)
                        blocks_cur = timdb.documents.getDocumentAsBlocks(cur_id)
                        if blocks_cur is None:
                            if verbose:
                                print("Could not get document {} version {} as blocks!".format(doc_id, current_ver[:6]))
                            break
                        if current_par >= len(blocks_cur):
                            if verbose:
                                print("Paragraph index {} over bounds ({} paragraphs)".format(current_par, len(blocks_cur)))
                            break
                        blocks_next = timdb.documents.getDocumentAsBlocks(next_id)
                        affinity = getClosestIndex(blocks_next, blocks_cur[current_par], current_par)
                        #affinities = ec.getSingleBlockMapping(cur_id, next_id, current_par) 
                        #[affinity, next_par] = max(affinities, key=lambda x: x[0] if x[0] is not None else 0)            
                        if affinity is not None:
                            next_par = affinity[0]
                            modified = affinity[1] < 1
                            timdb.documents.addParMapping(cur_id, next_id, current_par, next_par, modified)
                            mappings.append({'new_ver': next_ver, 'new_index': next_par})
                            fixed += 1
                            #if verbose:
                            #    print("Added a mapping -> {}({})".format(next_ver[:6], next_par))
                        else:
                            if verbose:
                                print("Affinity {} too small to justify a mapping.".format(affinity))
                            break
                    else:
                        break

                #print('Found a mapping: doc %d %s(%d) -> %s(%d), modified: %s' %
                #      (doc_id, current_ver[:6], current_par, mappings[0]['new_ver'][:6], mappings[0]['new_index'], mappings[0]['modified']))
                current_ver = mappings[0]['new_ver']
                current_par = mappings[0]['new_index']

        if loose > 0:
            print("Found {} loose ends and fixed {} of them.".format(loose, fixed))
        if verbose:
            print()



if __name__ == "__main__":
    parser = argparse.ArgumentParser(description="Diagnoses and fixes paragraph mappins.")
    parser.add_argument('doc_id', metavar='doc_id', nargs='+', help='Document id, or "all" for all documents.')
    parser.add_argument('-r', '--rebuild', action='store_true', help='Specify this to rebuild all mappings.')
    parser.add_argument('-v', '--verbose', action='store_true', help='Verbose output.')
    args = parser.parse_args()
    process(args.doc_id, args.rebuild, args.verbose)
