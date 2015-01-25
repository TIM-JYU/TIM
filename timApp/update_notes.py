
"""
    Updates the notes and readings to the new database format.
"""

from timdb.gitclient import GitClient
from timdb.timdb2 import TimDb
from timdb.timdbbase import DocIdentifier, TimDbException
import sqlite3
import os
import sys

def createparmappings(cursor):
    cursor.execute(
        """
        CREATE TABLE ParMappings(
        doc_id INTEGER NOT NULL,
        doc_ver INTEGER NOT NULL,
        par_index INTEGER NOT NULL,
        new_ver INTEGER NULL,
        new_index INTEGER NULL,
        modified BOOLEAN NULL,

        CONSTRAINT ParMappings_PK
            PRIMARY KEY (doc_id, doc_ver, par_index)
        )
        ;
        """, []
    )

def createusernotes(cursor):
    cursor.execute(
        """
        CREATE TABLE UserNotes(
        UserGroup_id	INTEGER NOT NULL,
        doc_id INTEGER NOT NULL,
        doc_ver INTEGER NOT NULL,
        par_index INTEGER NOT NULL,
        note_index INTEGER NOT NULL,
        content VARCHAR(255) NOT NULL,
        created TIMESTAMP NOT NULL,
        modified TIMESTAMP NULL,
        access VARCHAR(20) NOT NULL,
        tags VARCHAR(20) NOT NULL,

        CONSTRAINT UserNotes_PK
            PRIMARY KEY (UserGroup_id, doc_id, doc_ver, par_index, note_index),

        CONSTRAINT UserNotes_id
            FOREIGN KEY (doc_id, doc_ver, par_index)
            REFERENCES ParMappings (doc_id, doc_ver, par_index)
                ON DELETE CASCADE
                ON UPDATE RESTRICT
        );
        """, []
    )

def createreadparagraphs(cursor):
    cursor.execute(
        """
        CREATE TABLE ReadParagraphs(
        UserGroup_id	INTEGER NOT NULL,
        doc_id INTEGER NOT NULL,
        doc_ver INTEGER NOT NULL,
        par_index INTEGER NOT NULL,
        timestamp TIMESTAMP NOT NULL,

        CONSTRAINT ReadParagraphs_PK
            PRIMARY KEY (UserGroup_id, doc_id, doc_ver, par_index),

        CONSTRAINT ReadParagraphs_id
            FOREIGN KEY (doc_id, doc_ver, par_index)
            REFERENCES ParMappings (doc_id, doc_ver, par_index)
                ON DELETE CASCADE
                ON UPDATE RESTRICT
        );
        """, []
    )

def dropnewtables(cursor):
    cursor.execute("DROP TABLE ParMappings")
    cursor.execute("DROP TABLE ReadParagraphs")
    cursor.execute("DROP TABLE UserNotes")

def write_progress(status):
    sys.stdout.write("\r%d%%" % (status * 100))
    sys.stdout.flush()

def print_stats(ok, inv):
    sys.stdout.write("\r\n")
    sys.stdout.flush()
    print("{0} entries successfully converted".format(ok))
    if inv > 0:
        print("{0} invalid / duplicate entries".format(inv))
    print("")

def upgrade_readings(timdb):
    cursor = timdb.db.cursor()
    cursor.execute("select id, UserGroup_id, description from Block where type_id = 0")
    docs = cursor.fetchall()
    log = open('upgrade_readings.log', 'w')
    i = 0
    ok = 0
    inv = 0

    for doc_id, doc_owner, doc_name in docs:
        try:
            versions = timdb.documents.getDocumentVersions(doc_id)
        except TimDbException as e:
            log.write(str(e))
            continue

        if len(versions) == 0:
            log.write("Document {} has no versions!\n".format(doc_id))
            continue

        latest = versions[0]['hash']
        version_count = len(versions)
        mapping_added = False
        blocks = timdb.documents.getDocumentAsBlocks(DocIdentifier(doc_id, latest))

        cursor.execute(
            """
            select block_id, parent_block_specifier
            from BlockRelation
            where parent_block_id = ?
            """, [doc_id])
        rels = cursor.fetchall()
        for reading_id, par_index in rels:
            if par_index >= len(blocks):
                log.write('Document {}: block index {} outside the document\n'.format(doc_id, par_index))
                inv += 1
                continue

            cursor.execute(
                """
                select UserGroup_id, type_id, description
                from Block where id = ?
                """, [reading_id])
            reading = cursor.fetchone()
            if reading is None:
                log.write("Could not find block {0}\n".format(reading_id))
                inv += 1
                continue
            r_owner, b_type, r_content = reading
            if b_type != 5:
                # Not a reading
                continue

            if version_count > 1 and blocks[par_index] != r_content:
                # Set reading to the oldest version... marks it as modified
                version = versions[version_count - 1]['hash']

                if not mapping_added:
                    try:
                        # Add a paragraph mapping
                        cursor.execute(
                            """
                            insert into ParMappings (doc_id, doc_ver, par_index, new_ver, new_index, modified)
                            values (?, ?, ?, ?, ?, 'True')
                            """, [doc_id, version, par_index, latest, par_index]
                        )
                        mapping_added = True
                    except sqlite3.IntegrityError:
                        log.write('Mapping for documennt {0} paragraph {1} already exists.\n'.format(doc_id, par_index))
                        mapping_added = True
            else:
                version = latest

            # Convert this reading
            try:
                cursor.execute(
                    """
                    insert into ReadParagraphs (UserGroup_id, doc_id, doc_ver, par_index, timestamp)
                    values (?, ?, ?, ?, CURRENT_TIMESTAMP)
                    """, [r_owner, doc_id, version, par_index]
                )
                ok += 1
            except sqlite3.IntegrityError as e:
                #log.write('Reading for user group {0} doc {1} paragraph {2} already marked.\n'.format(r_owner, doc_id, par_index))
                log.write('Integrity error: {}\n'.format(e))
                inv += 1
                continue

        i += 1
        write_progress(i / len(docs))
        if i % 64 == 0:
            # To avoid massive final commit
            timdb.db.commit()

    log.close()
    timdb.db.commit()
    print_stats(ok, inv)

def try_insert_block_relation(cursor, block_id, doc_id, par_index):
    while True:
        try:
            cursor.execute(
                """
                insert into BlockRelation
                (parent_block_specifier, parent_block_revision_id, parent_block_id, block_id)
                values (?, 0, ?, ?)
                """, [par_index, doc_id, block_id]
            )
            return True
        except sqlite3.IntegrityError:
            print('Relation for block {0} already exists.'.format(block_id))
            c = input('Overwrite (y = yes, n = no, a = abort, Enter to retry)? ')
            if c == 'y':
               cursor.execute('delete from BlockRelation where block_id = ?',  [block_id])
               cursor.execute('delete from BlockViewAccess where block_id = ?',  [block_id])
               cursor.execute('delete from BlockEditAccess where block_id = ?',  [block_id])
            elif c == 'n':
                tried = True
            elif c == 'a':
                print("Aborting.")
                return False

def downgrade_readings(timdb):
    cursor = timdb.db.cursor()
    i = 0
    ok = 0

    cursor.execute("select UserGroup_id, doc_id, par_index, timestamp from ReadParagraphs")
    data = cursor.fetchall()
    for grp_id, doc_id, par_index, timestamp in data:
        i += 1
        write_progress(i / len(data))

        cursor.execute(
            """
            insert into Block (latest_revision_id, type_id, description, created, UserGroup_id)
            values (0, 5, ?, ?, ?)
            """, ['', timestamp, grp_id]
        )
        block_id = cursor.lastrowid
        if not try_insert_block_relation(cursor, block_id, doc_id, par_index):
            return
        ok += 1
        
    print_stats(ok, 0)
    timdb.db.commit()

def strtotags(tagstr):
    tags = []
    if 'd' in tagstr:
        tags.append("difficult")
    if 'u' in tagstr:
        tags.append("unclear")
    return tags

def tagstostr(tags):
    tagstr = ''
    if 'difficult' in tags:
        tagstr += 'd'
    if 'unclear' in tags:
        tagstr += 'u'
    return tagstr

def downgrade_notes(timdb):
    cursor = timdb.db.cursor()
    cursor.execute("select UserGroup_id, doc_id, par_index, content, created, modified, access, tags from UserNotes")
    blockpath = os.path.join(FILES_ROOT_PATH, 'blocks', 'notes')
    i = 0
    ok = 0
    data = cursor.fetchall()
    for grp_id, doc_id, par_index, content, created, modified, access, tags in data:
        i += 1
        write_progress(i / len(data))
        
        cursor.execute(
            """
            insert into Block (latest_revision_id, type_id, description, created, modified, UserGroup_id)
            values (0, 2, ?, ?, ?, ?)
            """, [",".join(strtotags(tags)), created, modified, grp_id]
        )
        block_id = cursor.lastrowid

        if not try_insert_block_relation(cursor, block_id, doc_id, par_index):
            return

        blockfile = timdb.notes.getBlockPath(int(block_id))
        relfile = os.path.relpath(blockfile, FILES_ROOT_PATH).replace('\\', '/')
        timdb.notes.writeUtf8(content, blockfile)
        git.add(relfile)
        if access == 'everyone':
            timdb.users.grantViewAccess(0, block_id)
        ok += 1

    print_stats(ok, 0)
    timdb.db.commit()
    commit_files('Added notes and paragraphs.')

def upgrade_notes(timdb):
    cursor = timdb.db.cursor()
    cursor.execute("select id, UserGroup_id, description from Block where type_id = 0")
    docs = cursor.fetchall()
    log = open('upgrade_notes.log', 'w')
    i = 0
    ok = 0
    inv = 0

    for doc_id, doc_owner, doc_name in docs:
        try:
            versions = timdb.documents.getDocumentVersions(doc_id)
        except TimDbException as e:
            log.write(str(e))
            continue

        if len(versions) == 0:
            log.write("Document {} has no versions!\n".format(doc_id))
            continue

        version = versions[0]['hash']
        blocks = timdb.documents.getDocumentAsBlocks(DocIdentifier(doc_id, version))

        cursor.execute(
            """
            select block_id, parent_block_specifier
            from BlockRelation
            where parent_block_id = ?
            """, [doc_id])
        rels = cursor.fetchall()
        for note_id, par_index in rels:
            if par_index >= len(blocks):
                log.write('Document {}: block index {} outside the document\n'.format(doc_id, par_index))
                inv += 1
                continue

            cursor.execute(
                """
                select UserGroup_id, type_id, description
                from Block where id = ?
                """, [note_id])
            note = cursor.fetchone()
            if note is None:
                log.write("Could not find block {0}\n".format(note_id))
                inv += 1
                continue
            note_owner, b_type, note_content = note
            if b_type != 2:
                # Not a note
                continue

            # Convert this note
            with open(timdb.notes.getBlockPath(note_id), 'r', encoding='utf-8') as f:
                content = f.read()

            if timdb.users.userGroupHasViewAccess(0, note_id):
                access = 'everyone'
            else:
                access = 'justme'

            cursor.execute(
                """
                select note_index from UserNotes
                where UserGroup_id = ? and doc_id = ? and par_index = ?
                order by note_index desc
                """, [note_owner, doc_id, par_index]
            )
            indexrows = cursor.fetchone()
            note_index = indexrows[0] + 1 if indexrows is not None else 0
            tagstr = tagstostr(note_content.split(',')) if note_content is not None else ''

            cursor.execute(
                """
                insert into UserNotes
                (UserGroup_id, doc_id, doc_ver, par_index, note_index, content, created, access, tags)
                values (?, ?, ?, ?, ?, ?, CURRENT_TIMESTAMP, ?, ?)
                """, [note_owner, doc_id, version, par_index, note_index, content, access, tagstr]
            )
            ok += 1

        i += 1
        write_progress(i / len(docs))
        if i % 64 == 0:
            # To avoid massive final commit
            timdb.db.commit()

    print_stats(ok, inv)
    timdb.db.commit()

def delete_old(timdb):
    cursor = timdb.db.cursor()
    
    cursor.execute("select id from Block where type_id = 5")
    for row in cursor.fetchall():
        cursor.execute("delete from BlockRelation where Block_id = ?", [row[0]])
        cursor.execute("delete from BlockViewAccess where Block_id = ?", [row[0]])
        cursor.execute("delete from BlockEditAccess where Block_id = ?", [row[0]])
    cursor.execute("delete from Block where type_id = 5")
    
    cursor.execute("select id from Block where type_id = 2")
    for row in cursor.fetchall():
        cursor.execute("delete from BlockRelation where Block_id = ?", [row[0]])
        cursor.execute("delete from BlockViewAccess where Block_id = ?", [row[0]])
        cursor.execute("delete from BlockEditAccess where Block_id = ?", [row[0]])
        
        git.rm('blocks/notes/' + str(row[0]))
    cursor.execute("delete from Block where type_id = 2")
    
    timdb.db.commit()
    commit_files('Deleted old notes and paragraphs.')

def commit_files(msg):
    git.commit(msg, 'update_notes.py')

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

if __name__ == "__main__":
    global git
    global FILES_ROOT_PATH

    abspath = os.path.abspath(__file__)
    dname = os.path.dirname(abspath)
    os.chdir(dname)
    FILES_ROOT_PATH = 'tim_files'
    git = GitClient.connect(FILES_ROOT_PATH)
    timdb = TimDb(db_path='tim_files/tim.db', files_root_path=FILES_ROOT_PATH)

    cursor = timdb.db.cursor()
    c = ''

    while c != 'q':
        pmcount = getcount(cursor, 'ParMappings')
        rpcount = getcount(cursor, 'ReadParagraphs')
        notecount = getcount(cursor, 'UserNotes')
        oldrpcount = getcount(cursor, 'Block', 'type_id = 5')
        oldnotecount = getcount(cursor, 'Block', 'type_id = 2')

        inform("users", getcount(cursor, 'User') - 1)
        inform("user groups", getcount(cursor, 'UserGroup') - 1)
        inform("old format read paragraphs", oldrpcount)
        inform("old format notes", oldnotecount)
        inform("paragraph mappings", pmcount)
        inform("new format read paragraphs", rpcount)
        inform("new format notes", notecount)

        cprint("'u' to upgrade everything to the new format", oldnotecount > 0 or oldrpcount > 0)
        cprint("'d' to downgrade everything", notecount > 0 or rpcount > 0)
        cprint("'c' to create the new tables", pmcount < 0 or rpcount < 0 or notecount < 0)
        cprint("'dn' to delete the new tables", pmcount >= 0 or rpcount >= 0 or notecount >= 0)
        cprint("'do' to delete the old format notes & read markings", oldrpcount > 0 or oldnotecount > 0)
        print("'q' to quit.")

        c = input(">")

        if (c == 'c' or c == 'u') and (pmcount < 0 or rpcount < 0 or notecount < 0):
            print("Creating the new tables.")
            if pmcount < 0:
                print('...ParMappings')
                createparmappings(cursor)
            if rpcount < 0:
                print('...ReadParagraphs')
                createreadparagraphs(cursor)
            if notecount < 0:
                print('...UserNotes')
                createusernotes(cursor)

        if c == 'u' and (oldnotecount > 0 or oldrpcount > 0):
            print("Upgrading readings...")
            upgrade_readings(timdb)
            print("Upgrading notes...")
            upgrade_notes(timdb)

        elif c == 'd' and (notecount > 0 or rpcount > 0):
            print("Downgrading readings...")
            downgrade_readings(timdb)
            print("Downgrading notes...")
            downgrade_notes(timdb)

        elif c == 'dn' and (pmcount >= 0 or rpcount >= 0 or notecount >= 0):
            print("Deleting the new tables.")
            dropnewtables(cursor)

        elif c == 'do' and (oldrpcount > 0 or oldnotecount > 0):
            print("Deleting the old format data.")
            delete_old(timdb)
            
        elif c == 'q':
            print("Exiting.")

        else:
            print("Unrecognized command.")

        print("")

