from contracts import contract
from timdb.timdbbase import TimDbBase


class Readings(TimDbBase):
    @contract
    def __init__(self, db_path : 'Connection', files_root_path : 'str', type_name : 'str', current_user_name : 'str'):
        """Initializes TimDB with the specified database and root path.

        :param db_path: The path of the database file.
        :param files_root_path: The root path where all the files will be stored.
        """
        TimDbBase.__init__(self, db_path, files_root_path, type_name, current_user_name)
        #self.ec = EphemeralClient(EPHEMERAL_URL)

    @contract
    def getReadings(self, UserGroup_id : 'int', doc_id : 'int', doc_ver : 'str') -> 'list(dict)':
        """Gets the reading info for a document for a user.

        :param UserGroup_id: The id of the user group whose readings will be fetched.
        :param block_id: The id of the block whose readings will be fetched.
        """
        return self.getMappedValues(UserGroup_id, doc_id, doc_ver, 'ReadParagraphs', status_unmodified = 'read')
        
    @contract
    def setAsRead(self, UserGroup_id: 'int', doc_id : 'int', doc_ver : 'str', par_index: 'int', commit : 'bool' = True):
        self.addEmptyParMapping(doc_id, doc_ver, par_index, commit=False)
        cursor = self.db.cursor()

        # Remove previous markings for this paragraph to reduce clutter
        cursor.execute(
            'delete from ReadParagraphs where UserGroup_id = ? and doc_id = ? and par_index = ?',
            [UserGroup_id, doc_id, par_index])

        # Set current version as read
        cursor.execute(
            'insert into ReadParagraphs (UserGroup_id, doc_id, doc_ver, par_index, timestamp) values (?, ?, ?, ?, CURRENT_TIMESTAMP)',
            [UserGroup_id, doc_id, doc_ver, par_index])

        if commit:
            self.db.commit()
