import os
from typing import List

from sqlalchemy import select

from documentmodel.document import Document
from timdb.dbutils import insert_block
from timdb.models.docentry import DocEntry
from timdb.printsettings import PrintSettings
from timdb.timdbbase import TimDbBase
from timdb.tim_models import PrintedDocs as PrintedDocsModel


class PrintedDocs(TimDbBase):

    def save_printed_doc(self, doc_id: int, doc_content: str, temp: bool, settings_hash: int):
        """

        :param doc_id:
        :param doc_content:
        :param temp:
        :param settings_hash:
        :return:
        """

        # TODO: Check that the file extension is allowed.
        # TODO: Do basic validation of the file contents.

        doc_name = DocEntry.find_by_id(doc_id).name
        doc_path = os.path.join(self.blocks_path, doc_id, doc_name)

        self._add_printed_doc(doc_id, temp, doc_path, settings_hash)

        os.makedirs(os.path.dirname(doc_path))  # TODO: Set mode. Does not take into account if file already exists.

        with open(doc_path, 'w') as f:
            f.write(doc_content)

        return doc_path

    def _add_printed_doc(self, doc_id: int, temp: bool, path: str, settings_hash:int, commit: bool=True):
        """
        Adds a marking for a printed document to the db


        :param doc_id: Document that is being printed
        :param temp: Is the doc saved on a temporary basis
        :param path: Path to the printed doc on disk
        :param settings_hash: The settings object of the printing transaction
        :param commit:
        :return:
        """

        cursor = self.db.cursor()
        cursor.execute(
            """
                INSERT INTO PrintedDocs
                (doc_id, temp, path, settings_hash, created)
                VALUES (%s, %s, %s, %s, CURRENT_DATE)
            """, [doc_id, temp, path, settings_hash])

        if commit:
            self.db.commit()

    def delete_printed_doc(self, printed_doc_id: int):
        """Deletes a printed document.

        :param note_id: The id of the note.

        """
        cursor = self.db.cursor()

        cursor.execute('DELETE FROM PrintedDocs WHERE id = %s', [printed_doc_id])

        self.db.commit()

    def get_printed_docs(self, doc: Document) -> List[str]:
        """Gets all the printed instances of a given document

        :param doc: The document for which to get the printed counterparts.

        """


        cursor = self.db.cursor()
        cursor.execute("""
            SELECT id, doc_id, temp, path, settings_hash, created
            FROM PrintedDocs WHERE doc_id = %s""",
                       [doc.doc_id])
        printed_docs = self.resultAsDictionary(cursor)
        return printed_docs

    def get_printed_docs_path(self, printed_doc_id: int, doc_name: str):
        """Gets the path of an image.

        :param printed_doc_id: The id of the printed doc.
        :param doc_name: The name of the document
        :returns: The path of the image file.

        """

        return os.path.join(self.blocks_path, str(printed_doc_id), doc_name)