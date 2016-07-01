from typing import List

from documentmodel.docparagraph import DocParagraph
from documentmodel.document import Document
from timdb.timdbbase import TimDbBase
from sqlite3 import Connection
import time


class Readings(TimDbBase):
    def get_readings(self, usergroup_id: int, doc: Document) -> List[dict]:
        """Gets the reading info for a document for a user.

        :param doc: The document for which to get the readings.
        :param usergroup_id: The id of the user group whose readings will be fetched.
        """
        ids = doc.get_referenced_document_ids()
        ids.add(doc.doc_id)
        template = ','.join('?' * len(ids))
        return self.resultAsDictionary(self.db.execute("""SELECT par_id, doc_id, par_hash, timestamp FROM ReadParagraphs
                           WHERE doc_id IN (%s) AND UserGroup_id = ?""" % template, list(ids) + [usergroup_id]))

    def mark_read(self, usergroup_id: int, doc: Document, par: DocParagraph, commit: bool=True):
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

    def mark_all_read(self, usergroup_id: int,
                      doc: Document,
                      commit: bool=True):
        for i in doc:
            self.mark_read(usergroup_id, doc, i, commit=False)
        if commit:
            self.db.commit()

    def copy_readings(self, src_par: DocParagraph, dest_par: DocParagraph, commit: bool = False):
        if str(src_par.doc.doc_id) == str(dest_par.doc.doc_id) and str(src_par.get_id()) == str(dest_par.get_id()):
            return

        cursor = self.db.cursor()

        cursor.execute(
            """
DELETE FROM ReadParagraphs WHERE doc_id = ? AND par_id = ? AND UserGroup_id IN
(SELECT UserGroup_id FROM ReadParagraphs WHERE doc_id = ? AND par_id = ?)
            """, [dest_par.doc.doc_id, dest_par.get_id(), src_par.doc.doc_id, src_par.get_id()]
        )

        cursor.execute(
            """
INSERT INTO ReadParagraphs (UserGroup_id, doc_id, par_id, timestamp, par_hash)
SELECT UserGroup_id, ?, ?, timestamp, par_hash
FROM ReadParagraphs
WHERE doc_id = ? AND par_id = ?
            """, [dest_par.doc.doc_id, dest_par.get_id(), src_par.doc.doc_id, src_par.get_id()]
        )

        if commit:
            self.db.commit()
