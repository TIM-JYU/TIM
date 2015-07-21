"""Defines the Documents class."""

import os
import re
from shutil import copyfile
from datetime import datetime
from sqlite3 import Connection

from contracts import contract
from ansi2html import Ansi2HTMLConverter
from documentmodel.attributeparser import AttributeParser

from documentmodel.docparagraph import DocParagraph
from documentmodel.documentparser import DocumentParser
from timdb.timdbbase import TimDbBase, TimDbException, blocktypes
from timdb.docidentifier import DocIdentifier
from ephemeralclient import EphemeralException, EphemeralClient, EPHEMERAL_URL
from timdb.gitclient import NothingToCommitException
from utils import date_to_relative
from documentmodel.document import Document


class Documents(TimDbBase):
    """Represents a collection of Document objects."""

    def __repr__(self):
        """For caching - we consider two Documents collections to be the same if their
        files_root_paths are equal."""
        return self.files_root_path

    @contract
    def __init__(self, db_path: 'Connection', files_root_path: 'str', type_name: 'str', current_user_name: 'str'):
        """Initializes TimDB with the specified database and root path.
        
        :param type_name: The type name.
        :param current_user_name: The name of the current user.
        :param db_path: The path of the database file.
        :param files_root_path: The root path where all the files will be stored.
        """
        TimDbBase.__init__(self, db_path, files_root_path, type_name, current_user_name)
        self.ec = EphemeralClient(EPHEMERAL_URL)

    @contract
    def add_paragraph(self, doc: 'Document',
                      content: 'str',
                      prev_par_id: 'str|None'=None,
                      attrs: 'dict|None'=None) -> 'tuple(list(DocParagraph),Document)':
        """Adds a new markdown block to the specified document.
        
        :param attrs: The attributes for the paragraph.
        :param doc: The id of the document.
        :param content: The content of the block.
        :param prev_par_id: The id of the previous paragraph. None if this paragraph should become the last.
        :returns: A list of the added blocks.
        """

        assert doc.exists(), 'document does not exist: %r' % doc.doc_id
        content = self.trim_markdown(content)
        par = doc.insert_paragraph(content, prev_par_id, attrs)
        self.update_last_modified(doc)
        return [par], doc

    @contract
    def create(self, name: 'str', owner_group_id: 'int') -> 'Document':
        """Creates a new document with the specified name.
        
        :param name: The name of the document to be created.
        :param owner_group_id: The id of the owner group.
        :returns: The newly created document object.
        """

        if '\0' in name:
            raise TimDbException('Document name cannot contain null characters.')

        document_id = self.insertBlockToDb(name, owner_group_id, blocktypes.DOCUMENT)
        document = Document(document_id, modifier_group_id=owner_group_id)
        document.create()
        document.add_paragraph('Click right side to edit. You can get help with editing from editors Help tab.')

        self.insertBlockToDb(name, owner_group_id, blocktypes.DOCUMENT)
        self.add_name(document_id, name)
        return document

    @contract
    def delete(self, document_id: 'int'):
        """Deletes the specified document.
        
        :param document_id: The id of the document to be deleted.
        """

        assert self.documentExists(document_id), 'document does not exist: %d' % document_id

        cursor = self.db.cursor()
        cursor.execute('DELETE FROM Block WHERE type_id = ? AND id = ?', [blocktypes.DOCUMENT, document_id])
        cursor.execute('DELETE FROM DocEntry WHERE id = ?', [document_id])
        cursor.execute('DELETE FROM ReadParagraphs where doc_id = ?', [document_id])
        cursor.execute('DELETE FROM UserNotes where doc_id = ?', [document_id])
        self.db.commit()

        Document.remove(document_id)


    @contract
    def get_names(self, document_id: 'int', return_json: 'bool' = False, include_nonpublic: 'bool' = False) -> 'list':
        """Gets the list of all names a document is known by.

        :param document_id: The id of the document to be retrieved.
        :param include_nonpublic: Whether to include non-public document names or not.
        :returns: A list of dictionaries with items {name, location, fullname, public}
        """
        cursor = self.db.cursor()
        public_clause = '' if include_nonpublic else ' AND public > 0'
        cursor.execute('SELECT name, public FROM DocEntry WHERE id = ?' + public_clause, [document_id])
        names = self.resultAsDictionary(cursor)

        for item in names:
            name = item['name']
            item['fullname'] = name
            item['location'], item['name'] = self.split_location(name)

        return names

    @contract
    def add_name(self, doc_id: 'int', name: 'str', public: 'bool' = True):
        cursor = self.db.cursor()
        cursor.execute("INSERT INTO DocEntry (id, name, public) VALUES (?, ?, ?)",
                       [doc_id, name, public])
        self.db.commit()

    @contract
    def delete_name(self, doc_id: 'int', name: 'str'):
        cursor = self.db.cursor()
        cursor.execute("DELETE FROM DocEntry WHERE id = ? AND name = ?",
                       [doc_id, name])
        self.db.commit()

    @contract
    def change_name(self, doc_id: 'int', old_name: 'str', new_name: 'str', public: 'bool' = True):
        cursor = self.db.cursor()
        cursor.execute("UPDATE DocEntry SET name = ?, public = ? WHERE id = ? AND name = ?",
                       [new_name, public, doc_id, old_name])
        self.db.commit()


    @contract
    def delete_paragraph(self, doc: 'Document', par_id: 'str') -> 'Document':
        """Deletes a paragraph from a document.
        
        :param doc: The id of the document from which to delete the paragraph.
        :param par_id: The id of the paragraph in the document that should be deleted.
        """

        doc.delete_paragraph(par_id)
        self.update_last_modified(doc)
        return doc

    @contract
    def documentExists(self, document_id: 'int') -> 'bool':
        """Checks whether a document with the specified id exists.
        
        :param document_id: The id of the document.
        :returns: True if the documents exists, false otherwise.
        """

        return self.blockExists(document_id, blocktypes.DOCUMENT)

    @contract
    def get_document_id(self, document_name: 'str') -> 'int|None':
        """Gets the document's identifier by its name or None if not found.
        
        :param document_name: The name of the document.
        :returns: The document id, or none if not found.
        """
        cursor = self.db.cursor()
        cursor.execute('SELECT id FROM DocEntry WHERE name = ?', [document_name])

        row = cursor.fetchone()
        return row[0] if row else None

    @contract
    def get_document_names(self, document_id: 'int') -> 'list(dict)':
        """Gets the document's names by its id.

        :param document_id: The id of the document.
        :returns: A list of dictionaries in format [{'name': (str), 'public': (bool)}, ...].
        """
        cursor = self.db.cursor()
        cursor.execute('SELECT name, public FROM DocEntry WHERE id = ?', [document_id])
        return self.resultAsDictionary(cursor)

    def get_first_document_name(self, document_id: 'int') -> 'str':
        """Gets the first public (or non-public if not found) name for a document id.

        :param document_id: The id of the document.
        :returns: A name for the document.
        """
        aliases = self.get_document_names(document_id)
        for alias in aliases:
            if alias['public']:
                return alias['name']
        return aliases[0]['name'] if len[aliases] > 0 else 'Untitled document'

    @contract
    def getDocument(self, document_id: 'int') -> 'dict':
        """Gets the metadata information of the specified document.
        
        :param document_id: The id of the document to be retrieved.
        :returns: A row representing the document.
        """
        # To be deleted (use get_document_names)
        cursor = self.db.cursor()
        cursor.execute('SELECT id, description AS name FROM Block WHERE id = ? AND type_id = ?',
                       [document_id, blocktypes.DOCUMENT])

        return self.resultAsDictionary(cursor)[0]

    @contract
    def get_documents(self, include_nonpublic: 'bool' = False) -> 'list(dict)':
        """Gets all the documents in the database.

        :historylimit Maximum depth in version history.
        :param include_nonpublic: Whether to include non-public document names or not.
        :returns: A list of dictionaries of the form {'id': <doc_id>, 'name': 'document_name'}
        """
        cursor = self.db.cursor()
        public_clause = '' if include_nonpublic else ' WHERE public > 0'
        cursor.execute('SELECT id, name FROM DocEntry' + public_clause)
        results = self.resultAsDictionary(cursor)

        for result in results:
            doc = Document(result['id'])
            result['modified'] = doc.get_last_modified()

        return results

    @contract
    def update_latest_revision(self, doc_id: 'DocIdentifier', commit: 'bool'=True):
        cursor = self.db.cursor()
        cursor.execute('SELECT latest_revision_id FROM Block WHERE id = ?', [doc_id.id])
        revid = cursor.fetchone()[0]
        if revid is None:
            cursor.execute("INSERT INTO ReadRevision (Block_id, Hash) VALUES (?, ?)",
                [doc_id.id, doc_id.hash])
            cursor.execute("UPDATE Block SET latest_revision_id = ? WHERE type_id = ? and id = ?",
                [cursor.lastrowid, blocktypes.DOCUMENT, doc_id.id])
        else:
            cursor.execute("UPDATE ReadRevision SET Hash = ? WHERE revision_id = ?",
                [doc_id.hash, revid])

        if commit:
            self.db.commit()

    @contract
    def getDocumentsForGroup(self, group_id : 'int', historylimit: 'int'=100) -> 'list(dict)':
        """Gets all the documents owned by a group.

        :returns: A list of dictionaries of the form {'id': <doc_id>, 'name': 'document_name'}
        """
        cursor = self.db.cursor()
        cursor.execute('SELECT id,description AS name FROM Block WHERE type_id = ? AND UserGroup_id = ?', [blocktypes.DOCUMENT, group_id])
        results = self.resultAsDictionary(cursor)
        for result in results:
            result['versions'] = self.getDocumentVersions(result['id'], limit=historylimit)
        return results

    @contract
    def getDocumentsByIds(self, document_ids: 'list(int)') -> 'seq(row)':
        """Gets all the documents in the database."""
        cursor = self.db.cursor()
        cursor.execute('select id,description as name from Block where id in (%s)' %
                       ','.join('?' * len(document_ids)), document_ids)
        return cursor.fetchall()

    @contract
    def getBlock(self, document_id: 'DocIdentifier', block_id: 'int') -> 'str':
        """Gets a block of a document.
        
        :param document_id: The id of the document.
        :param block_id: The id (index) of the block in the document.
        """

        content = self.ephemeralCall(document_id, self.ec.getBlock, block_id)
        return self.trim_markdown(content)

    def getBlockAsHtml(self, document_id: 'DocIdentifier', block_id: 'int') -> 'str':
        """Gets a block of a document in HTML.
        
        :param document_id: The id of the document.
        :param block_id: The id (index) of the block in the document.
        """

        return self.ephemeralCall(document_id, self.ec.getBlockAsHtml, block_id)

    @contract
    def getDocumentAsBlocks(self, document_id: 'DocIdentifier') -> 'list(str)':
        """Gets all the blocks of the specified document in markdown format.
        
        :param document_id: The id of the document.
        :returns: The blocks of the document in markdown format.
        """

        return [self.trim_markdown(block) for block in self.ephemeralCall(document_id, self.ec.getDocumentAsBlocks)]

    @contract
    def get_document_with_autoimport(self, document_id: 'DocIdentifier') -> 'Document|None':
        """Attempts to load a document from the new model. If it doesn't exist, attempts to load from old model.
        If found, an autoimport is performed and a Document object is returned. Otherwise, None is returned.

        :param document_id: The id of the document.
        :returns: The Document object or None if it doesn't exist.
        """
        d = Document(doc_id=document_id.id)
        if Document.doc_exists(document_id.id):
            return d
        if not self.documentExists(document_id.id):
            return None
        md_blocks = self.ephemeralCall(document_id, self.ec.getDocumentAsBlocks)
        d.create()
        ap = AttributeParser()
        for md in md_blocks:
            ap.set_str(md.split('\n', 1)[0])
            attrs, index = ap.get_attributes()
            if index is None:
                attrs = None
            d.add_paragraph(text=md, attrs=attrs)
        return d

    @contract
    def ephemeralCall(self, document_id: 'DocIdentifier', ephemeral_function, *args):
        """Calls a function of EphemeralClient, ensuring that the document is in cache.

        :param args: Required arguments for the function.
        :param ephemeral_function: The function to call.
        :param document_id: The id of the document.
        """

        try:
            result = ephemeral_function(document_id, *args)
        except EphemeralException:
            if self.documentExists(document_id.id):
                with open(self.getBlockPath(document_id.id), 'rb') as f:
                    self.ec.loadDocument(document_id, f.read())
                result = ephemeral_function(document_id, *args)
            else:
                raise TimDbException('The requested document was not found.')
        return result

    @contract
    def getDocumentPath(self, document_id: 'int') -> 'str':
        """Gets the path of the specified document.
        
        :param document_id: The id of the document.
        :returns: The path of the document.
        """
        return self.getBlockPath(document_id)

    @contract
    def getDocumentPathAsRelative(self, document_id: 'int'):
        return os.path.relpath(self.getDocumentPath(document_id), self.files_root_path).replace('\\', '/')

    @contract
    def getDocumentMarkdown(self, document_id: 'DocIdentifier') -> 'str':
        content = self.git.get_contents(document_id.hash, self.getDocumentPathAsRelative(document_id.id))
        return self.trim_markdown(content)

    @contract
    def getDifferenceToPrevious(self, document_id: 'DocIdentifier') -> 'str':
        try:
            out, _ = self.git.command('diff --word-diff=color --unified=5 {}^! {}'.format(document_id.hash,
                                                                                self.getDocumentPathAsRelative(
                                                                                    document_id.id)))
        except TimDbException as e:
            e.message = 'The requested revision was not found.'
            raise
        conv = Ansi2HTMLConverter(inline=True, dark_bg=False)
        html = conv.convert(out, full=False)
        return html

    @contract
    def getDocumentVersions(self, document_id: 'int', limit: 'int'=100, date_format: 'str'='relative') -> 'list(dict(str:str))':
        """Gets the versions of a document.
        
        :param date_format: The date format.
        :param limit: Maximum number of versions to get.
        :param document_id: The id of the document whose versions will be fetched.
        :returns: A list of the versions of the document.
        """

        if limit <= 0:
            return []

        if not self.documentExists(document_id):
            raise TimDbException('The specified document does not exist.')

        file_path = self.getDocumentPathAsRelative(document_id)
        output, _ = self.git.command('log --format=%H|%ad|%an|%s --date={} -n {} {}'.format(date_format, limit, file_path))
        lines = output.splitlines()
        versions = []
        for line in lines:
            pieces = line.split('|')
            versions.append({'hash': pieces[0], 'timestamp': pieces[1], 'user': pieces[2], 'message': pieces[3]})
        return versions

    @contract
    def getNewestVersion(self, document_id: 'int') -> 'dict(str:str)|None':
        """Gets the hash of the newest version for a document.

        :param document_id: The id of the document.
        :returns: A version dictionary, or none if not found.
        """
        return self.git.getLatestVersionDetails(self.getDocumentPathAsRelative(document_id))

    @contract
    def import_document_from_file(self, document_file: 'str', document_name: 'str',
                               owner_group_id: 'int') -> 'Document':
        """Imports the specified document in the database.

        :param document_file: The file path of the document to import.
        :param document_name: The name for the document.
        :param owner_group_id: The owner group of the document.
        :returns: The created document object.
        """
        with open(document_file, 'r', encoding='utf-8') as f:
            content = f.read()  # todo: use a stream instead
        return self.import_document(content, document_name, owner_group_id)

    @contract
    def import_document(self, content: 'str', document_name: 'str', owner_group_id: 'int') -> 'Document':
        doc = self.create(document_name, owner_group_id)
        parser = DocumentParser(content)
        for block in parser.get_blocks():
            doc.add_paragraph(text=block['md'], attrs=block.get('attrs'))
        return doc

    def __commitDocumentChanges(self, document_id: 'DocIdentifier', doc_content: 'str', msg: 'str') -> 'str':
        """Commits the changes of the specified document to Git.
        
        :param document_id: The document identifier.
        :param msg: The commit message.
        :returns: The hash of the commit.
        """

        self.writeUtf8(doc_content, self.getDocumentPath(document_id.id))
        self.git.add(self.getDocumentPathAsRelative(document_id.id))
        return self.git.commit('Document {}: {}'.format(document_id.id, msg), author=self.current_user_name)

    @contract
    def modify_paragraph(self, doc: 'Document', par_id: 'str',
                         new_content: 'str', new_attrs: 'dict|None'=None) -> 'tuple(list(DocParagraph), Document)':
        """Modifies a paragraph in a document.
        
        :param new_attrs: The attributes for the paragraph.
        :param doc: The document.
        :param par_id: The id of the paragraph to be modified.
        :param new_content: The new content of the paragraph.
        :returns: The paragraphs and the new document as a tuple.
        """

        assert Document.doc_exists(doc.doc_id), 'document does not exist: ' + str(doc.doc_id)
        new_content = self.trim_markdown(new_content)
        par = doc.modify_paragraph(par_id, new_content, new_attrs)
        self.update_last_modified(doc)
        return [par], doc

    @contract
    def renameDocument(self, document_id: 'DocIdentifier', new_name: 'str') -> 'None':
        """Renames a document.
        
        :param document_id: The id of the document to be renamed.
        :param new_name: The new name for the document.
        """

        assert self.documentExists(document_id.id), 'document does not exist: ' + str(document_id)

        cursor = self.db.cursor()
        cursor.execute('UPDATE Block SET description = ? WHERE type_id = ? AND id = ?',
                       [new_name, blocktypes.DOCUMENT, document_id.id])
        self.db.commit()

    @contract
    def update_document(self, doc: 'Document', new_content: 'str') -> 'Document':
        """Updates a document.
        
        :param doc: The id of the document to be updated.
        :param new_content: The new content of the document.
        :returns: The id of the new document.
        """

        assert self.documentExists(doc.doc_id), 'document does not exist: ' + str(doc)

        new_content = self.trim_markdown(new_content)
        # TODO: Do the update

        self.update_last_modified(doc, commit=False)
        self.db.commit()
        return doc

    def trim_markdown(self, text: 'str'):
        """Trims the specified text. Don't trim spaces from left side because they may indicate a code block

        :param text: The text to be trimmed.
        :return: The trimmed text.
        """
        return text.rstrip().strip('\r\n')

    @contract
    def update_last_modified(self, doc: 'Document', commit: 'bool'=True):
        cursor = self.db.cursor()
        cursor.execute('UPDATE Block SET modified = CURRENT_TIMESTAMP WHERE type_id = ? and id = ?',
                       [blocktypes.DOCUMENT, doc.doc_id])
        if commit:
            self.db.commit()
