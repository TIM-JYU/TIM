import os
from shutil import copyfile
from datetime import datetime

from contracts import contract
import ansiconv
import posixpath

from timdb.timdbbase import TimDbBase, TimDbException, blocktypes
from timdb.docidentifier import DocIdentifier
from ephemeralclient import EphemeralClient, EphemeralException, EPHEMERAL_URL
from timdb.gitclient import NothingToCommitException, GitClient
from utils import date_to_relative


class Folders(TimDbBase):
    @contract
    def __init__(self, db_path: 'Connection', files_root_path: 'str', type_name: 'str', current_user_name: 'str'):
        """Initializes TimDB with the specified database and root path.

        :param type_name: The type name.
        :param current_user_name: The name of the current user.
        :param db_path: The path of the database file.
        :param files_root_path: The root path where all the files will be stored.
        """
        TimDbBase.__init__(self, db_path, files_root_path, type_name, current_user_name)

    @contract
    def createFolder(self, name: 'str', owner_group_id: 'int') -> 'int':
        """Creates a new folder with the specified name.

        :param name: The name of the folder to be created.
        :param owner_group_id: The id of the owner group.
        :returns: The id of the newly created folder.
        """

        if '\0' in name:
            raise TimDbException('Folder name cannot contain null characters.')

        return self.insertBlockToDb(name, owner_group_id, blocktypes.FOLDER)

    @contract
    def getFolderId(self, name: 'str') -> 'int|None':
        """Gets the folders's identifier by its name or None if not found.

        :param name: The name of the folder.
        :returns: Integer block identifier.
        """
        cursor = self.db.cursor()
        cursor.execute('SELECT id FROM Block WHERE description = ? AND type_id = ?',
                       [document_name, blocktypes.DOCUMENT])

        row = cursor.fetchone()
        return row[0] if row is not None else None

    @contract
    def getFolder(self, block_id: 'int') -> 'dict|None':
        """Gets the metadata information of the specified folder.

        :param document_id: The block id of the folder to be retrieved.
        :returns: A row representing the folder.
        """
        cursor = self.db.cursor()
        cursor.execute('SELECT id, description AS name FROM Block WHERE id = ? AND type_id = ?',
                       [block_id, blocktypes.FOLDER])

        result = self.resultAsDictionary(cursor)
        return result[0] if len(result) > 0 else None

    @contract
    def getContainingFolderName(self, document_name: 'str') -> 'str':
        separator = document_name.rfind('/')
        return '' if separator < 0 else document_name[:separator]

    @contract
    def folderExists(self, document_id: 'int') -> 'bool':
        """Checks whether a folder with the specified id exists.

        :param document_id: The id of the folder.
        :returns: True if the folder exists, false otherwise.
        """
        return self.blockExists(document_id, blocktypes.FOLDER, check_file = False)

    @contract
    def getFolderId(self, folderName: 'str', createWithOwner: 'int|None' = None) -> 'int|None':
        cursor = self.db.cursor()
        cursor.execute('SELECT id FROM Block WHERE type_id = ? AND description = ?', [blocktypes.FOLDER, folderName])
        result = cursor.fetchone()
        if result is None and createWithOwner is not None:
            #print("I created a folder record for " + folderName)
            return self.insertBlockToDb(folderName, createWithOwner, blocktypes.FOLDER)
        return result[0] if result is not None else None

    @contract
    def getFolders(self, root_path: 'str|None' = None, group_id: 'int|None' = None) -> 'list(dict)':
        """Gets all the folders under a path.
        :returns: A list of dictionaries of the form {'id': <folder_id>, 'name': 'folder_name', 'fullname': 'folder_path'}
        """
        cursor = self.db.cursor()
        #cursor.execute('DELETE FROM Block WHERE type_id = ?', [blocktypes.FOLDER])
        #self.db.commit()

        if root_path is None or root_path == '' or root_path == '.':
            cursor.execute(
                """
                SELECT description FROM Block WHERE type_id = ? AND description LIKE "%/%"
                UNION
                SELECT description || "/" FROM Block WHERE type_id = ?
                """, [blocktypes.DOCUMENT, blocktypes.FOLDER])
            startindex = 0
        else:
            cursor.execute(
                """
                SELECT description FROM Block WHERE type_id = ? AND description LIKE "{0}/%/%"
                UNION
                SELECT description || "/" FROM Block WHERE type_id = ? AND description LIKE "{0}/%"
                """.format(root_path), [blocktypes.DOCUMENT, blocktypes.FOLDER])
            startindex = len(root_path) + 1

        foldernames = {doc['description'][startindex:doc['description'].find('/', startindex)] for doc in self.resultAsDictionary(cursor)}

        folders = [{
                       'id': self.getFolderId(posixpath.join(root_path, name), group_id),
                       'name': name,
                       'fullname': posixpath.join(root_path, name)
                   } for name in foldernames]
        folders.sort(key=lambda x: x['name'])
        return folders

    @contract
    def renameFolder(self, block_id: 'int', new_name: 'str') -> 'None':
        """Renames a folder, updating all the documents within.

        :param block_id: The id of the folder to be renamed.
        :param new_name: The new name for the folder.
        """

        folder_info = self.getFolder(block_id)
        assert folder_info is not None, 'folder does not exist: ' + str(block_id)
        old_name = folder_info['name']

        cursor = self.db.cursor()
        cursor.execute('UPDATE Block SET description = ? WHERE type_id = ? AND id = ?',
                       [new_name, blocktypes.FOLDER, block_id])

        cursor.execute('SELECT description FROM Block WHERE description LIKE "{}/%"'.format(old_name))
        for row in cursor.fetchall():
            # TODO: update sqlite and use sql update set description = replace(...) ...
            old_docname = row[0]
            new_docname = old_docname.replace(old_name, new_name)
            cursor.execute('UPDATE Block SET description = ? WHERE description = ?',
                           [new_docname, old_docname])

        self.db.commit()