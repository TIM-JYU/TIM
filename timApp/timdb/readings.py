from contracts import contract
from documentmodel.docparagraph import DocParagraph
from documentmodel.document import Document
from timdb.timdbbase import TimDbBase
from sqlite3 import Connection

class Readings(TimDbBase):
    @contract
    def __init__(self, db_path: 'Connection', files_root_path: 'str', type_name: 'str', current_user_name: 'str'):
        """Initializes TimDB with the specified database and root path.

        :param db_path: The path of the database file.
        :param files_root_path: The root path where all the files will be stored.
        """
        TimDbBase.__init__(self, db_path, files_root_path, type_name, current_user_name)

    @contract
    def getReadings(self, usergroup_id: 'int', doc: 'Document') -> 'list(dict)':
        """Gets the reading info for a document for a user.

        :param doc: The document for which to get the readings.
        :param usergroup_id: The id of the user group whose readings will be fetched.
        """
        return self.resultAsDictionary(self.db.execute("""SELECT par_id, par_hash, timestamp FROM ReadParagraphs
                           WHERE doc_id = ? AND UserGroup_id = ?""", [doc.doc_id, usergroup_id]))

    @contract
    def setAsRead(self, usergroup_id: 'int', doc: 'Document', par: 'DocParagraph', commit: 'bool'=True):
        cursor = self.db.cursor()
        # Remove previous markings for this paragraph to reduce clutter
        cursor.execute(
            'DELETE FROM ReadParagraphs WHERE UserGroup_id = ? AND doc_id = ? AND par_id = ?',
            [usergroup_id, doc.doc_id, par.get_id()])

        # Set current version as read
        cursor.execute(
            'INSERT INTO ReadParagraphs (UserGroup_id, doc_id, par_id, timestamp, par_hash)'
            'VALUES (?, ?, ?, CURRENT_TIMESTAMP, ?)',
            [usergroup_id, doc.doc_id, par.get_id(), par.get_hash()])

        if commit:
            self.db.commit()

    @contract
    def setAllAsRead(self, usergroup_id: 'int',
                     doc: 'Document',
                     commit: 'bool'=True):
        for i in doc:
            self.setAsRead(usergroup_id, doc.doc_id, i, commit=False)
        if commit:
            self.db.commit()
