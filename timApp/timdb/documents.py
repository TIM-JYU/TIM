"""Defines the Documents class."""

import os

from typing import List, Optional, Dict, Tuple, Iterable

from timApp.documentmodel.docparagraph import DocParagraph
from timApp.documentmodel.docsettings import DocSettings
from timApp.documentmodel.document import Document
from timApp.documentmodel.documentparser import DocumentParser
from timApp.timdb.docinfo import DocInfo
from timApp.timdb.models.block import Block
from timApp.timdb.models.docentry import DocEntry
from timApp.timdb.models.translation import Translation
from timApp.timdb.tim_models import ReadParagraph, UserNotes, db, BlockAccess
from timApp.timdb.timdbbase import TimDbBase
from timApp.timdb.blocktypes import blocktypes
from timApp.timdb.timdbexception import TimDbException


def create_citation(original_doc: Document,
                    owner_group_id: int,
                    path: Optional[str]=None,
                    title: Optional[str]=None,
                    ref_attribs: Optional[Dict[str, str]] = None) -> DocInfo:
    """Creates a citation document with the specified name. Each paragraph of the citation document references the
    paragraph in the original document.

    :param title: The document title.
    :param original_doc: The original document to be cited.
    :param path: The path of the document to be created.
    :param owner_group_id: The id of the owner group.
    :param ref_attribs: Reference attributes to be used globally.
    :returns: The newly created document object.

    """

    if not original_doc.exists():
        raise TimDbException('The document does not exist!')

    ref_attrs = ref_attribs if ref_attribs is not None else {}

    # For translations, name is None and no DocEntry is created in database.
    doc_entry = DocEntry.create(path, owner_group_id, title)
    doc = doc_entry.document

    r = ref_attrs['r'] if 'r' in ref_attrs else 'tr'

    settings = original_doc.get_settings()
    settings.set_source_document(original_doc.doc_id)
    doc.set_settings(settings.get_dict())

    for par in original_doc:
        if par.is_setting():
            continue
        ref_par = par.create_reference(doc, r, add_rd=False)
        for attr in ref_attrs:
            ref_par.set_attr(attr, ref_attrs[attr])

        doc.add_paragraph_obj(ref_par)

    return doc_entry


class Documents(TimDbBase):
    """Represents a collection of Document objects."""

    def __repr__(self):
        """For caching - we consider two Documents collections to be the same if their
        files_root_paths are equal."""
        return self.files_root_path

    def add_paragraph(self, doc: Document,
                      content: str,
                      prev_par_id: Optional[str]=None,
                      attrs: Optional[dict]=None) -> Tuple[List[DocParagraph], Document]:
        """Adds a new markdown block to the specified document.

        :param attrs: The attributes for the paragraph.
        :param doc: The id of the document.
        :param content: The content of the block.
        :param prev_par_id: The id of the previous paragraph. None if this paragraph should become the last.
        :returns: A list of the added blocks.

        """

        assert doc.exists(), 'document does not exist: %r' % doc.doc_id
        content = self.trim_markdown(content)
        par = doc.insert_paragraph(content, insert_before_id=prev_par_id, attrs=attrs)
        self.update_last_modified(doc)
        return [par], doc

    def delete(self, document_id: int):
        """Deletes the specified document.

        :param document_id: The id of the document to be deleted.

        """

        assert self.exists(document_id), 'document does not exist: %d' % document_id
        DocEntry.query.filter_by(id=document_id).delete()
        BlockAccess.query.filter_by(block_id=document_id).delete()
        Block.query.filter_by(type_id=blocktypes.DOCUMENT, id=document_id).delete()
        ReadParagraph.query.filter_by(doc_id=document_id).delete()
        UserNotes.query.filter_by(doc_id=document_id).delete()
        Translation.query.filter((Translation.doc_id == document_id) | (Translation.src_docid == document_id)).delete()
        db.session.commit()
        Document.remove(document_id)

    def recover_db(self, usergroup_id: int, folder: str = None) -> int:
        """Recreates database entries for documents that already exist on the disk.

        :param usergroup_id Owner for recovered documents
        :param folder Folder in which the recovered documents are placed
        :returns Number of recovered documents.

        """
        doc_dir = Document.get_documents_dir(self.files_root_path)
        if not os.path.exists(doc_dir):
            return 0

        cursor = self.db.cursor()
        recovered = 0
        for doc_item in os.listdir(doc_dir):
            if not os.path.isdir(os.path.join(doc_dir, doc_item)):
                continue
            try:
                doc_id = int(doc_item)
                cursor.execute('SELECT EXISTS(SELECT id FROM Block WHERE id = %s)', [doc_id])
                if not cursor.fetchone()[0]:
                    doc_name = "Recovered document " + str(doc_id)
                    cursor.execute("""INSERT INTO Block (id, type_id, description, created, UserGroup_id)
                                      VALUES (%s, %s, %s, CURRENT_TIMESTAMP, %s)""",
                                   [doc_id, blocktypes.DOCUMENT, doc_name, usergroup_id])
                    recovered += 1
                    doc_path = doc_name if not folder else folder + '/' + doc_name
                    cursor.execute('SELECT EXISTS(SELECT id FROM DocEntry WHERE id = %s)', [doc_id])
                    if not cursor.fetchone()[0]:
                        cursor.execute('INSERT INTO DocEntry (id, name, public) VALUES (%s, %s, TRUE)',
                                       [doc_id, doc_path])

            except ValueError:
                pass

        if recovered:
            self.db.commit()

        return recovered

    def delete_paragraph(self, doc: Document, par_id: str) -> Document:
        """Deletes a paragraph from a document.

        :param doc: The id of the document from which to delete the paragraph.
        :param par_id: The id of the paragraph in the document that should be deleted.

        """

        doc.delete_paragraph(par_id)
        self.update_last_modified(doc)
        return doc

    def exists(self, document_id: int) -> bool:
        """Checks whether a document with the specified id exists.

        :param document_id: The id of the document.
        :returns: True if the documents exists, false otherwise.

        """

        return self.blockExists(document_id, blocktypes.DOCUMENT)

    def get_documents(self, include_nonpublic: bool = False,
                      filter_ids: Optional[Iterable[int]]=None,
                      filter_folder: str=None,
                      search_recursively: bool=True) -> List[DocEntry]:
        """Gets all the documents in the database matching the given criteria.

        :param search_recursively: Whether to search recursively.
        :param filter_folder: Optionally restricts the search to a specific folder.
        :param filter_ids: An optional iterable of document ids for filtering the documents.
               Must be non-empty if supplied.
        :param include_nonpublic: Whether to include non-public document names or not.
        :returns: A list of DocEntry objects.

        """

        q = DocEntry.query
        if not include_nonpublic:
            q = q.filter_by(public=True)
        if filter_ids:
            q = q.filter(DocEntry.id.in_(filter_ids))
        if filter_folder:
            filter_folder = filter_folder.strip('/') + '/'
            if filter_folder == '/':
                filter_folder = ''
            q = q.filter(DocEntry.name.like(filter_folder + '%'))
        if not search_recursively:
            q = q.filter(DocEntry.name.notlike(filter_folder + '%/%'))
        return q.all()

    def get_documents_in_folder(self, folder_pathname: str,
                                include_nonpublic: bool = False,
                                filter_ids: Optional[Iterable[int]]=None) -> List[DocEntry]:
        """Gets all the documents in a folder.

        :param filter_ids: An optional iterable of document ids for filtering the documents.
               Must be non-empty if supplied.
        :param folder_pathname: path to be searched for documents without ending '/'
        :param include_nonpublic: Whether to include non-public document names or not.
        :returns: A list of dictionaries of the form {'id': <doc_id>, 'name': 'document_name'}

        """
        return self.get_documents(include_nonpublic=include_nonpublic,
                                  filter_folder=folder_pathname,
                                  search_recursively=False,
                                  filter_ids=filter_ids)

    def import_document_from_file(self, document_file: str,
                                  path: str,
                                  owner_group_id: int,
                                  title: Optional[str]=None) -> Document:
        """Imports the specified document in the database.

        :param document_file: The file path of the document to import.
        :param path: The path for the document.
        :param owner_group_id: The owner group of the document.
        :returns: The created document object.

        """
        with open(document_file, 'r', encoding='utf-8') as f:
            content = f.read()  # todo: use a stream instead
        return self.import_document(content, path, owner_group_id, title=title)

    def import_document(self, content: str, path: str, owner_group_id: int, title: Optional[str]=None) -> Document:
        doc = DocEntry.create(path, owner_group_id, title=title).document
        parser = DocumentParser(content)
        for block in parser.get_blocks():
            doc.add_paragraph(text=block['md'], attrs=block.get('attrs'))
        return doc

    def modify_paragraph(self, doc: Document, par_id: str,
                         new_content: str, new_attrs: Optional[dict]=None) -> Tuple[List[DocParagraph], Document]:
        """Modifies a paragraph in a document.

        :param new_attrs: The attributes for the paragraph.
        :param doc: The document.
        :param par_id: The id of the paragraph to be modified.
        :param new_content: The new content of the paragraph.
        :returns: The paragraphs and the new document as a tuple.

        """

        assert self.exists(doc.doc_id), 'document does not exist: ' + str(doc.doc_id)
        new_content = self.trim_markdown(new_content)
        par = doc.modify_paragraph(par_id, new_content, new_attrs)
        self.update_last_modified(doc)
        return [par], doc

    def update_document(self, doc: Document, new_content: str, original_content: str=None,
                        strict_validation=True) -> Document:
        """Updates a document.

        :param doc: The id of the document to be updated.
        :param new_content: The new content of the document.
        :param original_content: The original content of the document.
        :param strict_validation: Whether to use stricter validation rules for areas etc.
        :returns: The id of the new document.

        """

        assert self.exists(doc.doc_id), 'document does not exist: ' + str(doc)

        doc.update(new_content, original_content, strict_validation)
        self.update_last_modified(doc, commit=False)
        self.db.commit()
        return doc

    def trim_markdown(self, text: str):
        """Trims the specified text. Don't trim spaces from left side because they may indicate a code block.

        :param text: The text to be trimmed.
        :return: The trimmed text.

        """
        return text.rstrip().strip('\r\n')

    def update_last_modified(self, doc: Document, commit: bool=True):
        cursor = self.db.cursor()
        cursor.execute('UPDATE Block SET modified = CURRENT_TIMESTAMP WHERE type_id = %s and id = %s',
                       [blocktypes.DOCUMENT, doc.doc_id])
        if commit:
            self.db.commit()
