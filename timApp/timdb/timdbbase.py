""""""
import os
from contracts import contract, new_contract
import collections
import sqlite3
from collections import namedtuple


# A document identifier consists of the id of the document and the version hash.
class DocIdentifier(namedtuple("DocIdentifier", "id hash")):
    __slots__ = ()

    def __str__(self):
        return str(self.id) + ':' + str(self.hash)


new_contract('Connection', sqlite3.Connection)
new_contract('DocIdentifier', DocIdentifier)

BLOCKTYPES = collections.namedtuple('blocktypes', ('DOCUMENT', 'COMMENT', 'NOTE', 'ANSWER', 'IMAGE', 'READING'))
blocktypes = BLOCKTYPES(0, 1, 2, 3, 4, 5)


class TimDbException(Exception):
    """The exception that is thrown when an error occurs during a TimDb operation."""
    pass


class TimDbBase(object):
    """Base class for TimDb classes (e.g. Users, Notes)."""

    @contract
    def __init__(self, db: 'Connection', files_root_path: 'str', type_name: 'str', current_user_name: 'str'):
        """Initializes TimDB with the specified database and root path.
        
        :param db: The database connection.
        :param files_root_path: The root path where all the files will be stored.
        :param type_name: The type name.
        :param current_user_name: The current user name.
        """
        self.files_root_path = os.path.abspath(files_root_path)
        self.current_user_name = current_user_name

        self.blocks_path = os.path.join(self.files_root_path, 'blocks', type_name)
        for path in [self.blocks_path]:
            if not os.path.exists(path):
                os.makedirs(path)
        self.db = db

    @contract
    def getBlockPath(self, block_id: 'int') -> 'str':
        """Gets the path of the specified block.
        
        :param block_id: The id of the block.
        :returns: The path of the block.
        """
        return os.path.join(self.blocks_path, str(block_id))

    @contract
    def blockExists(self, block_id: 'int', block_type: 'int') -> 'bool':
        """Checks whether the specified block exists.
        
        :param block_id: The id of the block to check.
        :param block_type: The type of the block to check.
        :returns: True if the block exists, false otherwise.
        """

        cursor = self.db.cursor()
        cursor.execute('SELECT exists(SELECT id FROM Block WHERE id = ? AND type_id = ? LIMIT 1)',
                       [block_id, block_type])
        result = cursor.fetchone()
        if result[0] == 1:
            if not os.path.exists(self.getBlockPath(block_id)):
                print ('blockExists: the block {} was in database but the file was not found'.format(block_id))
                return False
            #assert os.path.exists(
            #    self.getBlockPath(block_id)), 'the block {} was in database but the file was not found'.format(block_id)
            return True
        return False

    # TODO: contract
    def resultAsDictionary(self, cursor):
        """Converts the result in database cursor object to JSON."""

        rows = [x for x in cursor.fetchall()]
        cols = [x[0] for x in cursor.description]
        results = []
        for row in rows:
            result = {}
            for prop, val in zip(cols, row):
                result[prop] = val
            results.append(result)
        return results

    @contract
    def writeUtf8(self, content: 'str', path: 'str'):
        with open(path, 'w', encoding='utf-8', newline='\n') as f:
            f.write(content)

    @contract
    def getOwnedBlockRelations(self, block_id: 'int', user_id: 'int', relation_type: 'int') -> 'list(dict)':
        cursor = self.db.cursor()
        cursor.execute("""SELECT id, parent_block_specifier, description, created, modified FROM Block,BlockRelation WHERE
                             Block.id = BlockRelation.Block_id
                          AND id IN
                             (SELECT Block_id FROM BlockRelation WHERE parent_block_id = ?)
                          AND type_id = ?
                          AND UserGroup_id IN
                                 (SELECT UserGroup_id FROM UserGroupMember WHERE User_id = ?)""",
                       [block_id, relation_type, user_id])
        return self.resultAsDictionary(cursor)

    @contract
    def getBlockRelations(self, block_id: 'int', relation_type: 'int') -> 'list(dict)':
        cursor = self.db.cursor()
        cursor.execute("""SELECT id, parent_block_specifier, description, created, modified FROM Block,BlockRelation WHERE
                             Block.id = BlockRelation.Block_id
                          AND id IN
                             (SELECT Block_id FROM BlockRelation WHERE parent_block_id = ?)
                          AND type_id = ?""", [block_id, relation_type])
        return self.resultAsDictionary(cursor)

    @contract
    def addEmptyParMapping(self, doc_id : 'int', doc_ver : 'str', par_index : 'int', commit : 'bool' = True):
        cursor = self.db.cursor()

        cursor.execute(
            """
            select par_index from ParMappings
            where doc_id = ? and doc_ver = ? and par_index = ?
            """,
            [doc_id, doc_ver, par_index])

        if cursor.fetchone() is not None:
            #print("Mapping already exists - document {0} version {1} paragraph {2}".format(
            #doc_id, doc_ver[:6], par_index))
            return

        #print("addEmptyParMapping(doc {0}, version {1}, paragraph {2})".format(
        # doc_id, doc_ver[:6], par_index))

        cursor.execute(
            """
            insert into ParMappings (doc_id, doc_ver, par_index) values (?, ?, ?)
            """,
            [doc_id, doc_ver, par_index])

        if commit:
            self.db.commit()

    @contract
    def getParMapping(self, doc_id : 'int', doc_ver : 'str', target_ver : 'str', par_index : 'int', commit = True) -> 'tuple(int, bool)|None':
        """
        Returns a paragraph to which a previous version paragraph maps to.
        :param doc_id: Document id.
        :param doc_ver: Document version to map from.
        :param target_ver: Document version to map to.
        :param par_index: Paragraph index in the original document version.
        :returns: A tuple of the new paragraph index and whether the paragraph has been modified (boolean),
                    or None if no mapping was found.
        """
        cursor = self.db.cursor()
        current_ver = doc_ver
        current_par = par_index
        modified = False
        num_links = 0
        while current_ver != target_ver:
            cursor.execute(
                """
                select new_ver, new_index, modified
                from ParMappings
                where doc_id = ? and doc_ver = ? and par_index = ?
                and new_ver is not null and new_index is not null
                """, [doc_id, current_ver, current_par])
            mappings = self.resultAsDictionary(cursor)
            if len(mappings) == 0:
                #print('Loose end: doc %d %s(%d) -> ???' % (doc_id, current_ver[:6], current_par))
                return None

            #print('Found a mapping: doc %d %s(%d) -> %s(%d), modified: %s' %
            #      (doc_id, current_ver[:6], current_par, mappings[0]['new_ver'][:6], mappings[0]['new_index'], mappings[0]['modified']))
            current_ver = mappings[0]['new_ver']
            current_par = mappings[0]['new_index']
            modified |= mappings[0]['modified'] == 'True'
            num_links += 1

        #print('num_links = %d, current_ver = %s, doc_ver = %s, modified = %s' %
        #      (num_links, current_ver[:6], doc_ver[:6], str(modified)))
        if num_links > 1 and current_ver == doc_ver:
            # Flatten mappings to speed up future queries
            # a -> b -> c becomes a -> c
            print('Mapping can be optimized: %s(%s) -> %s(%s)' %
                  (read_ver[:6], read_par, current_ver[:6], current_par))
            #cursor.execute(
            #    """
            #    update ParMappings
            #    set new_ver = ?, new_index = ?, modified = ?
            #    where doc_id = ? and doc_ver = ? and par_index = ?
            #    """, [current_ver, current_par, str(modified), doc_id, read_ver, read_par])
            #if commit:
            #    self.db.commit()

        return (current_par, modified)

    @contract
    def getMappedValues(self, UserGroup_id: 'int|None', doc_id: 'int', doc_ver: 'str', table: 'str',
                        status_unmodified="unmodified", status_modified="modified", extra_fields=None,
                        custom_access: 'str'='0', order_by_sql: 'str'='') -> 'list(dict)':
        if not extra_fields:
            extra_fields = []
        cursor = self.db.cursor()
        fields = ['par_index', 'doc_ver'] + extra_fields

        query = "select {} from {} where ({}) and doc_id = ? {}"\
            .format(','.join(fields),
                    table,
                    'UserGroup_id = ? OR ({})'.format(custom_access) if UserGroup_id is not None else custom_access,
                    order_by_sql)

        cursor.execute(query, [UserGroup_id, doc_id] if UserGroup_id is not None else [doc_id])
        rows = self.resultAsDictionary(cursor)
        results = []

        # To remove duplicates.
        # Is a dictionary because hashing is fast.
        mapped_pars = {}

        # Check for modifications
        # We first go through the rows trying to find exact match
        for row in rows:
            read_ver = row['doc_ver']
            par_index = int(row['par_index'])

            if read_ver == doc_ver:
                row['status'] = status_unmodified

                # We don't want to return read markings multiple times per paragraph (but notes yes)
                if table == 'UserNotes' or par_index not in mapped_pars:
                    results.append(row)
                    mapped_pars[par_index] = True

        # In case we didn't find exact match for some row, try to map it
        for row in rows:
            read_ver = row['doc_ver']
            par_index = int(row['par_index'])

            if read_ver != doc_ver:
                # Document has been modified, see if the paragraph has changed
                #print('Paragraph {0} refers to old version, trying to find mappings.'.format(read_par))
                mapping = self.getParMapping(doc_id, read_ver, doc_ver, row['par_index'], commit = False)

                # We don't want to return read markings multiple times per paragraph (but notes yes)
                if mapping is not None and (table == 'UserNotes' or mapping[0] not in mapped_pars):
                    par_index_new = mapping[0]
                    modified = mapping[1]
                    row['par_index'] = par_index_new
                    row['status'] = status_modified if modified else status_unmodified
                    row['doc_ver'] = read_ver if modified else doc_ver
                    results.append(row)
                    mapped_pars[par_index_new] = True

                    # Update UserNotes table; otherwise the notes won't be modifiable/deletable from UI
                    if table == 'UserNotes':
                        cursor = self.db.cursor()
                        try:
                            cursor.execute("""update UserNotes set par_index = ?, doc_ver = ?
                                              where par_index = ? and doc_ver = ? and doc_id = ?""", [
                                           par_index_new,
                                           row['doc_ver'],
                                           par_index,
                                           read_ver,
                                           doc_id
                                           ])
                        except sqlite3.IntegrityError:
                            # Already exists
                            pass

        # Commit in case of getParMapping optimizing the mappings
        self.db.commit()
        return results
