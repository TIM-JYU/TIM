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
            assert os.path.exists(
                self.getBlockPath(block_id)), 'the block {} was in database but the file was not found'.format(block_id)
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
    def getMappingsForDoc(self, doc_id : 'int') -> 'list(dict)':
        cursor = self.db.cursor()
        cursor.execute(
            """
            select doc_ver, par_index, new_ver, new_index, modified
            from ParMappings
            where doc_id = ?
            and new_ver is not null and new_index is not null
            order by doc_ver
            """, [doc_id])
        return self.resultAsDictionary(cursor)

    def findMapping(self, mappings : 'list(dict)', doc_ver : 'str', par_index : 'int', start_index : 'int' = 0, length : 'int' = -1) -> 'tuple(str, int, bool)|None':
        n = length if length >= 0 else len(mappings)
        if n == 0:
            return None
        if n == 1:
            m = mappings[start_index]
            if m["doc_ver"] == doc_ver and m["par_index"] == par_index:
                return (m["new_ver"], m["new_index"], bool(m["modified"]))
            else:
                return None

        m = mappings[start_index + n / 2]

        if m["doc_ver"] == doc_ver:
            if m["par_index"] == par_index:
                return (m["new_ver"], m["new_index"], bool(m["modified"]))
            if m["par_index"] > par_index:
                return self.findMapping(mappings, doc_ver, par_index, start_index + length / 2, length / 2)
            # if m["par_index"] < par_index:
            return self.findMapping(mappings, doc_ver, par_index, start_index, length / 2)

        if m["doc_ver"] > doc_ver:
            return self.findMapping(mappings, doc_ver, par_index, start_index + length / 2, length / 2)
        #if m["doc_ver"] < doc_ver:
        return self.findMapping(mappings, doc_ver, par_index, start_index, length / 2)


    @contract
    def getMappedValues(self, UserGroup_id : 'int', doc_id : 'int', doc_ver : 'str', table : 'str',
                        status_unmodified = "unmodified", status_modified = "modified",
                        extra_fields : 'list' = [], custom_access : 'str|None' = None) -> 'list(dict)':
        cursor = self.db.cursor()

        fields = ['par_index', 'doc_ver'] + extra_fields

        if custom_access is None:
            query = "select {0} from {1} where UserGroup_id = ? and doc_id = ?".format(','.join(fields), table)
        else:
            query = "select {0} from {1} where (UserGroup_id = ? OR ({2})) and doc_id = ?".format(','.join(fields), table, custom_access)

        cursor.execute(query, [UserGroup_id, doc_id])
        rows = self.resultAsDictionary(cursor)
        remove_rows = []
        maps = None

        # Check for modifications
        db_modified = False

        for row in rows:
            read_ver = row['doc_ver']
            row['status'] = status_unmodified

            if read_ver != doc_ver:
                # Document has been modified, see if the paragraph has changed
                read_par = row['par_index']
                current_ver = read_ver
                modified = False
                num_links = 0
                #print('Paragraph {0} refers to old version, trying to find mappings.'.format(read_par))

                if maps is None:
                    maps = self.getMappingsForDoc(doc_id)

                while current_ver != doc_ver:
                    current_par = read_par
                    map = self.findMapping(maps, current_ver, current_par)
                    if map is None:
                        #print('Loose end: document %d, paragraph %s' % (doc_id, read_par))
                        #row['status'] = status_modified
                        remove_rows.append(row)
                        break
                    else:
                        (current_ver, current_par, mod) = map
                        modified |= mod
                        num_links += 1

                #print('num_links = %d, current_ver = %s, doc_ver = %s, modified = %s' %
                #      (num_links, current_ver[:6], doc_ver[:6], str(modified)))
                if num_links > 1 and current_ver == doc_ver:
                    # Flatten mappings to speed up future queries
                    # a -> b -> c becomes a -> c
                    #print('Updating mapping: %s(%s) -> %s(%s)' %
                    #      (read_ver[:6], read_par, current_ver[:6], current_par))
                    cursor.execute(
                        """
                        update ParMappings
                        set new_ver = ?, new_index = ?, modified = ?
                        where doc_id = ? and doc_ver = ? and par_index = ?
                        """, [current_ver, current_par, str(modified), doc_id, read_ver, read_par])
                    db_modified = True
                #print("")
                row['par_index'] = current_par
                if modified:
                    row['status'] = status_modified
                else:
                    # No changes
                    row['doc_ver'] = current_ver

        for row in remove_rows:
            rows.remove(row)

        if db_modified:
            self.db.commit()

        return rows
