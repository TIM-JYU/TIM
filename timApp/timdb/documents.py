"""Defines the Documents class."""

import os
from sqlite3 import Connection

from typing import List, Optional, Dict, Tuple

from documentmodel.docparagraph import DocParagraph
from documentmodel.docsettings import DocSettings
from documentmodel.document import Document
from documentmodel.documentparser import DocumentParser
from timdb.timdbbase import TimDbBase, TimDbException, blocktypes

NOTIFICATION_TYPES = ['email_doc_modify', 'email_comment_add', 'email_comment_modify']


class Documents(TimDbBase):
    """Represents a collection of Document objects."""

    def __repr__(self):
        """For caching - we consider two Documents collections to be the same if their
        files_root_paths are equal."""
        return self.files_root_path

    def add_paragraph(self, doc: Document,
                      content: str,
                      prev_par_id: Optional[str]=None,
                      attrs: Optional[dict]=None, properties: Optional[dict]=None) -> Tuple[List[DocParagraph], Document]:
        """Adds a new markdown block to the specified document.
        
        :param attrs: The attributes for the paragraph.
        :param doc: The id of the document.
        :param content: The content of the block.
        :param prev_par_id: The id of the previous paragraph. None if this paragraph should become the last.
        :returns: A list of the added blocks.
        """

        assert doc.exists(), 'document does not exist: %r' % doc.doc_id
        content = self.trim_markdown(content)
        par = doc.insert_paragraph(content, insert_before_id=prev_par_id, attrs=attrs, properties=properties)
        self.update_last_modified(doc)
        return [par], doc

    def create(self, name: Optional[str], owner_group_id: int) -> Document:
        """Creates a new document with the specified name.
        
        :param name: The name of the document to be created (can be None).
        :param owner_group_id: The id of the owner group (can be None).
        :returns: The newly created document object.
        """

        if name is not None and '\0' in name:
            raise TimDbException('Document name cannot contain null characters.')

        document_id = self.insertBlockToDb(name, owner_group_id, blocktypes.DOCUMENT)
        document = Document(document_id, modifier_group_id=owner_group_id)
        document.create()

        if name is not None:
            self.add_name(document_id, name)

        return document

    def create_translation(self, original_doc: Document, name: Optional[str], owner_group_id: int,
                           ref_attribs: Optional[Dict[str, str]] = None) -> Document:
        """Creates a translation document with the specified name.

        :param original_doc: The original document to be translated.
        :param name: The name of the document to be created.
        :param owner_group_id: The id of the owner group.
        :param ref_attribs: Reference attributes to be used globally.
        :returns: The newly created document object.
        """

        if not original_doc.exists():
            raise TimDbException('The document does not exist!')

        ref_attrs = ref_attribs if ref_attribs is not None else []

        doc = self.create(name, owner_group_id)
        first_par = True
        r = ref_attrs['r'] if 'r' in ref_attrs else 'tr'

        for par in original_doc:
            if first_par:
                first_par = False
                settings = DocSettings.from_paragraph(par) if par.is_setting() else DocSettings()
                settings.set_source_document(original_doc.doc_id)
                doc.add_paragraph_obj(settings.to_paragraph(doc))
                if par.is_setting():
                    continue

            ref_par = par.create_reference(doc, r, add_rd=False)
            for attr in ref_attrs:
                ref_par.set_attr(attr, ref_attrs[attr])

            doc.add_paragraph_obj(ref_par)

        return doc

    def delete(self, document_id: int):
        """Deletes the specified document.
        
        :param document_id: The id of the document to be deleted.
        """

        assert self.exists(document_id), 'document does not exist: %d' % document_id

        cursor = self.db.cursor()
        cursor.execute('DELETE FROM DocEntry WHERE id = %s', [document_id])
        cursor.execute('DELETE FROM Block WHERE type_id = %s AND id = %s', [blocktypes.DOCUMENT, document_id])
        cursor.execute('DELETE FROM ReadParagraphs where doc_id = %s', [document_id])
        cursor.execute('DELETE FROM UserNotes where doc_id = %s', [document_id])
        cursor.execute('DELETE FROM Translation WHERE doc_id = %s OR src_docid = %s', [document_id, document_id])
        self.db.commit()

        Document.remove(document_id)

    def recover_db(self, usergroup_id: int, folder: str = None) -> bool:
        """Recreates database entries for documents that already exist on the disk
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
                    doc_fullname = doc_name if not folder else folder + '/' + doc_name
                    cursor.execute('SELECT EXISTS(SELECT id FROM DocEntry WHERE id = %s)', [doc_id])
                    if not cursor.fetchone()[0]:
                        cursor.execute('INSERT INTO DocEntry (id, name, public) VALUES (%s, %s, 1)',
                                           [doc_id, doc_fullname])

            except ValueError:
                pass

        if recovered:
            self.db.commit()

        return recovered

    def get_names(self, document_id: int, return_json: bool = False, include_nonpublic: bool = False) -> List[dict]:
        """Gets the list of all names a document is known by.

        :param document_id: The id of the document to be retrieved.
        :param include_nonpublic: Whether to include non-public document names or not.
        :returns: A list of dictionaries with items {name, location, fullname, public}
        """
        cursor = self.db.cursor()
        public_clause = '' if include_nonpublic else ' AND public = TRUE'
        cursor.execute('SELECT name, public FROM DocEntry WHERE id = %s' + public_clause, [document_id])
        names = self.resultAsDictionary(cursor)

        for item in names:
            name = item['name']
            item['fullname'] = name
            item['location'], item['name'] = self.split_location(name)

        return names

    def add_name(self, doc_id: int, name: str, public: bool = True):
        cursor = self.db.cursor()
        cursor.execute("INSERT INTO DocEntry (id, name, public) VALUES (%s, %s, %s)",
                       [doc_id, name, public])
        self.db.commit()

    def delete_name(self, doc_id: int, name: str):
        cursor = self.db.cursor()
        cursor.execute("DELETE FROM DocEntry WHERE id = %s AND name = %s",
                       [doc_id, name])
        self.db.commit()

    def change_name(self, doc_id: int, old_name: str, new_name: str, public: bool = True):
        cursor = self.db.cursor()
        cursor.execute("UPDATE DocEntry SET name = %s, public = %s WHERE id = %s AND name = %s",
                       [new_name, public, doc_id, old_name])
        self.db.commit()

    def add_translation(self, doc_id: int, src_docid: int, lang_id: str, title: Optional[str]=None):
        cursor = self.db.cursor()
        cursor.execute("INSERT INTO Translation (doc_id, src_docid, lang_id, doc_title) VALUES (%s, %s, %s, %s)",
                       [doc_id, src_docid, lang_id, title])
        self.db.commit()

    def remove_translation(self, doc_id: int, commit=True):
        cursor = self.db.cursor()
        cursor.execute("DELETE FROM Translation WHERE doc_id = %s", [doc_id])
        if commit:
            self.db.commit()

    def get_translation_source(self, doc_id: int) -> int:
        cursor = self.db.cursor()
        cursor.execute("SELECT src_docid FROM Translation WHERE doc_id = %s", [doc_id])
        result = cursor.fetchone()
        return result[0] if result is not None else doc_id

    def get_translations(self, doc_id: int) -> List[dict]:
        cursor = self.db.cursor()
        cursor.execute("SELECT src_docid FROM Translation WHERE doc_id = %s", [doc_id])
        result = cursor.fetchone()
        src_docid = doc_id if result is None else result[0]
        src_name = self.get_first_document_name(src_docid, check_translations=False)

        cursor.execute("""SELECT doc_id as id, lang_id, doc_title as title FROM Translation
                          WHERE src_docid = %s
                       """, [src_docid])
        results = self.resultAsDictionary(cursor)

        for tr in results:
            tr['src_docid'] = src_docid
            tr['owner_id'] = self.get_owner(tr['id'])
            tr['name'] = self.get_translation_path(doc_id, src_name, tr['lang_id'])

        return results

    def get_translation(self, src_docid: int, lang_id: str) -> Optional[int]:
        cursor = self.db.cursor()
        cursor.execute("SELECT doc_id FROM Translation WHERE src_docid = %s AND lang_id = %s",
                       [src_docid, lang_id])
        result = cursor.fetchone()
        return result[0] if result is not None else None

    def get_translation_title(self, doc_id: int) -> Optional[str]:
        cursor = self.db.cursor()
        cursor.execute("SELECT doc_title FROM Translation WHERE doc_id = %s", [doc_id])
        result = cursor.fetchone()
        return result[0] if result is not None else None

    def get_translation_path(self, doc_id: int, src_doc_name: Optional[str], lang_id: Optional[str]) -> str:
        if src_doc_name is None or lang_id is None:
            return str(doc_id)

        return src_doc_name + '/' + lang_id

    def translation_exists(self, src_doc_id: int, lang_id: Optional[str]=None, doc_id: Optional[int]=None) -> bool:
        if lang_id is None and doc_id is None:
            raise TimDbException("translation_exists called with all parameters null")

        cursor = self.db.cursor()
        base_statement = "SELECT EXISTS(SELECT doc_id FROM Translation WHERE src_docid = %s{0})"
        langid_clause = " AND lang_id = %s"
        docid_clause = " AND doc_id = %s"

        if doc_id is None:
            cursor.execute(base_statement.format(langid_clause), [src_doc_id, lang_id])
        elif lang_id is None:
            cursor.execute(base_statement.format(docid_clause), [src_doc_id, doc_id])
        else:
            cursor.execute(base_statement.format(docid_clause + langid_clause), [src_doc_id, doc_id, lang_id])

        result = cursor.fetchone()
        return result[0] == 1

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

    def get_document_id(self, document_name: str, try_translation=True) -> Optional[int]:
        """Gets the document's identifier by its name or None if not found.
        
        :param document_name: The name of the document.
        :returns: The document id, or none if not found.
        """
        cursor = self.db.cursor()
        cursor.execute('SELECT id FROM DocEntry WHERE name = %s', [document_name])
        row = cursor.fetchone()
        if row is None:
            # Try if it's a name for a translation
            parts = document_name.rsplit('/', 1)
            if len(parts) < 2:
                return None
            src_docid = self.get_document_id(parts[0], try_translation=False)
            if src_docid is None:
                return None
            return self.get_translation(src_docid, parts[1])

        return row[0]

    def get_document_names(self, document_id: int, include_nonpublic=True) -> List[dict]:
        """Gets the document's names by its id.

        :param document_id: The id of the document.
        :returns: A list of dictionaries in format [{'name': (str), 'public': (bool)}, ...].
        """
        cursor = self.db.cursor()
        public_clause = '' if include_nonpublic else ' WHERE public = True'
        cursor.execute('SELECT name, public FROM DocEntry WHERE id = %s' + public_clause, [document_id])
        return self.resultAsDictionary(cursor)

    def get_first_document_name(self, document_id: int, check_translations: bool = True) -> str:
        """Gets the first public (or non-public if not found) name for a document id.

        :param document_id: The id of the document.
        :returns: A name for the document.
        """
        aliases = self.get_document_names(document_id)
        for alias in aliases:
            if alias['public']:
                return alias['name']
        if len(aliases) > 0:
            return aliases[0]['name']

        if check_translations:
            translations = self.get_translations(document_id)
            for tr in translations:
                if tr['id'] == document_id:
                    return tr['name']

        return 'Untitled document'

    def get_document(self, document_id: int) -> Optional[dict]:
        """Gets the metadata information of the specified document.
        
        :param document_id: The id of the document to be retrieved.
        :returns: A row representing the document.
        """
        cursor = self.db.cursor()
        cursor.execute("SELECT id, name FROM DocEntry WHERE id = %s", [document_id])
        rows = self.resultAsDictionary(cursor)
        return rows[0] if len(rows) > 0 else None

    def get_documents(self, include_nonpublic: bool = False) -> List[dict]:
        """Gets all the documents in the database.

        :historylimit Maximum depth in version history.
        :param include_nonpublic: Whether to include non-public document names or not.
        :returns: A list of dictionaries of the form {'id': <doc_id>, 'name': 'document_name'}
        """
        cursor = self.db.cursor()
        public_clause = '' if include_nonpublic else ' WHERE public = TRUE'
        cursor.execute('SELECT id, name FROM DocEntry' + public_clause)
        results = self.resultAsDictionary(cursor)

        for result in results:
            doc = Document(result['id'])
            result['modified'] = doc.get_last_modified()

        return results

    def get_documents_in_folder(self, folder_pathname: str, include_nonpublic: bool = False) -> List[dict]:
        """Gets all the documents in a folder

        :param folder_pathname: path to be searched for documents without ending '/'
        :param include_nonpublic: Whether to include non-public document names or not.
        :returns: A list of dictionaries of the form {'id': <doc_id>, 'name': 'document_name'}
        """
       # timdb = getTimDb()
        documents = self.get_documents(include_nonpublic=include_nonpublic)
        results = []


        for document in documents:
            document_path, _ = self.split_location(document['name'])
            if document_path == folder_pathname:
                results.append(document)

        return results

    def getDocumentPath(self, document_id: int) -> str:
        """Gets the path of the specified document.
        
        :param document_id: The id of the document.
        :returns: The path of the document.
        """
        return self.getBlockPath(document_id)

    def getDocumentPathAsRelative(self, document_id: int):
        return os.path.relpath(self.getDocumentPath(document_id), self.files_root_path).replace('\\', '/')

    def import_document_from_file(self, document_file: str, document_name: str,
                               owner_group_id: int) -> Document:
        """Imports the specified document in the database.

        :param document_file: The file path of the document to import.
        :param document_name: The name for the document.
        :param owner_group_id: The owner group of the document.
        :returns: The created document object.
        """
        with open(document_file, 'r', encoding='utf-8') as f:
            content = f.read()  # todo: use a stream instead
        return self.import_document(content, document_name, owner_group_id)

    def import_document(self, content: str, document_name: str, owner_group_id: int) -> Document:
        doc = self.create(document_name, owner_group_id)
        parser = DocumentParser(content)
        for block in parser.get_blocks():
            doc.add_paragraph(text=block['md'], attrs=block.get('attrs'))
        return doc

    def modify_paragraph(self, doc: Document, par_id: str,
                         new_content: str, new_attrs: Optional[dict]=None,
                         new_properties: Optional[dict]=None) -> Tuple[List[DocParagraph], Document]:
        """Modifies a paragraph in a document.
        
        :param new_attrs: The attributes for the paragraph.
        :param doc: The document.
        :param par_id: The id of the paragraph to be modified.
        :param new_content: The new content of the paragraph.
        :returns: The paragraphs and the new document as a tuple.
        """

        assert self.exists(doc.doc_id), 'document does not exist: ' + str(doc.doc_id)
        new_content = self.trim_markdown(new_content)
        par = doc.modify_paragraph(par_id, new_content, new_attrs, new_properties)
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
        """Trims the specified text. Don't trim spaces from left side because they may indicate a code block

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

    def resolve_doc_id_name(self, doc_path: str) -> Optional[dict]:
        """Returns document id and name based on its path.
        :param doc_path: The document path.
        """
        doc_id = self.get_document_id(doc_path)
        if doc_id is None or not self.exists(doc_id):
            # Backwards compatibility: try to use as document id
            try:
                doc_id = int(doc_path)
                if not self.exists(doc_id):
                    return None
                doc_name = self.get_first_document_name(doc_id)
                return {'id': doc_id,
                        'fullname': doc_name,
                        'name': self.get_short_name(doc_name),
                        'title': self.get_doc_title(doc_id, doc_name)}
            except ValueError:
                return None
        return {'id': doc_id,
                'fullname': doc_path,
                'name': self.get_short_name(doc_path),
                'title': self.get_doc_title(doc_id, doc_path)}

    def get_doc_title(self, doc_id: int, full_name: Optional[str]) -> Optional[str]:
        title = self.get_translation_title(doc_id)
        if title is not None:
            return title
        if full_name is None:
            return "Untitled"
        return self.get_short_name(full_name)

    def get_short_name(self, full_name: str) -> str:
        parts = full_name.rsplit('/', 1)
        return parts[len(parts) - 1]

    def get_notify_settings(self, user_id: int, doc_id: int) -> dict:
        cursor = self.db.cursor()
        fieldnames = ', '.join(NOTIFICATION_TYPES)
        query = 'SELECT {} FROM Notification WHERE user_id = %s AND doc_id = %s'.format(fieldnames)
        cursor.execute(query, [user_id, doc_id])

        results = self.resultAsDictionary(cursor)
        if len(results) == 0:
            return {k: False for k in NOTIFICATION_TYPES}

        return {key: bool(results[0][key]) for key in results[0]}

    def set_notify_settings(self, user_id: int, doc_id: int, settings: dict):
        keys = NOTIFICATION_TYPES
        cursor = self.db.cursor()
        cursor.execute('SELECT EXISTS(SELECT user_id FROM Notification WHERE user_id = %s AND doc_id = %s)',
                       [user_id, doc_id])
        row_exists = cursor.fetchone()[0]

        if row_exists:
            update_statement = 'UPDATE Notification SET ' \
                               + ', '.join(['{}={}'.format(k, settings[k]) for k in keys]) \
                               + ' WHERE user_id = %s AND doc_id = %s'
            cursor.execute(update_statement, [user_id, doc_id])
        else:
            insert_statement = 'INSERT INTO Notification (user_id, doc_id, {}) VALUES (%s, %s, {})'.format(
                ', '.join(keys), ', '.join(['%s' for _ in range(len(keys))]))
            cursor.execute(insert_statement, [user_id, doc_id] + [settings[k] for k in keys])

        self.db.commit()
