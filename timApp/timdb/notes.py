from sqlite3 import Connection

from contracts import contract

from documentmodel.docparagraph import DocParagraph
from documentmodel.document import Document
from ephemeralclient import EphemeralClient, EPHEMERAL_URL
from markdownconverter import md_to_html
from timdb.timdbbase import TimDbBase


class Notes(TimDbBase):
    @contract
    def __init__(self, db_path: 'Connection', files_root_path: 'str', type_name: 'str', current_user_name: 'str'):
        """Initializes TimDB with the specified database and root path.
        
        :param db_path: The path of the database file.
        :param files_root_path: The root path where all the files will be stored.
        """
        TimDbBase.__init__(self, db_path, files_root_path, type_name, current_user_name)

    @contract
    def __tagstostr(self, tags: 'list(str)') -> 'str':
        tagstr = ''
        if 'difficult' in tags:
            tagstr += 'd'
        if 'unclear' in tags:
            tagstr += 'u'
        return tagstr

    @contract
    def __strtotags(self, tagstr: 'str') -> 'list(str)':
        tags = []
        if 'd' in tagstr:
            tags.append("difficult")
        if 'u' in tagstr:
            tags.append("unclear")
        return tags

    @contract
    def hasEditAccess(self, usergroup_id: 'int', note_id: 'int') -> 'bool':
        """Checks whether the specified usergroup has access to the specified note.

        :param usergroup_id: The usergroup id for which to check access.
        :param note_id: The id of the note.
        """
        cursor = self.db.cursor()

        cursor.execute(
            """
                SELECT UserGroup_id FROM UserNotes
                WHERE id = ?
            """, [note_id])
        row = cursor.fetchone()
        return row is not None and int(row[0]) == usergroup_id

    @contract
    def addNote(self, usergroup_id: 'int', doc: 'Document', par: 'DocParagraph', content: 'str', access: 'str',
                tags: 'list(str)', commit: 'bool'=True):
        """Adds a note to the document.

        :param commit:
        :param usergroup_id: The user group who owns the note.
        :param doc: The document in which the note exists.
        :param par: The paragraph which the note is for.
        :param content: The content of the note.
        :param access: Who can read the note.
        :param tags: Tags for the note (difficult, unclear).
        """
        cursor = self.db.cursor()
        note_html = md_to_html(content)
        cursor.execute(
            """
                INSERT INTO UserNotes
                (UserGroup_id, doc_id, par_id, par_hash,
                content, created, modified, access, tags, html)
                VALUES (?, ?, ?, ?, ?, CURRENT_TIMESTAMP, NULL, ?, ?, ?)
            """, [usergroup_id, doc.doc_id, par.get_id(), par.get_hash(),
                  content, access, self.__tagstostr(tags), note_html])

        if commit:
            self.db.commit()

    @contract
    def modifyNote(self, note_id: 'int', new_content: 'str',
                   access: 'str', new_tags: 'list(str)'):
        """Modifies an existing note.

        :param note_id: The id of the note.
        :param access: The access of the note.
        :param new_content: New note text to set.
        :param new_tags: New tags to set.
        """
        cursor = self.db.cursor()
        new_html = md_to_html(new_content)
        cursor.execute(
            """
                UPDATE UserNotes
                SET content = ?, tags = ?, access = ?, html = ?, modified = CURRENT_TIMESTAMP
                WHERE id = ?
            """, [new_content, self.__tagstostr(new_tags), access, new_html, note_id])

        self.db.commit()

    @contract
    def deleteNote(self, note_id: 'int'):
        """Deletes a note.

        :param note_id: The id of the note.
        """
        cursor = self.db.cursor()

        cursor.execute(
            """
                DELETE FROM UserNotes
                WHERE id = ?
            """, [note_id])

        self.db.commit()

    @contract
    def getNotes(self, usergroup_id: 'int', doc: 'Document', include_public=True) -> 'list(dict)':
        """Gets all notes for a document a particular user has access to.

        :param usergroup_id: The usergroup id.
        :param doc: The document for which to get the notes.
        """
        ids = doc.get_referenced_document_ids()
        ids.add(doc.doc_id)
        template = ','.join('?' * len(ids))
        include_public_sql = ''
        if include_public:
            include_public_sql = "OR access = 'everyone'"
        result = self.resultAsDictionary(
            self.db.execute("""SELECT id, par_id, doc_id, par_hash, content,
                                      created, modified, access, tags, html, UserGroup_id
                               FROM UserNotes
                               WHERE (UserGroup_id = ? %s) AND doc_id IN (%s)""" % (include_public_sql, template),
                            [usergroup_id] + list(ids)))


        return self.process_notes(result)

    @contract
    def get_note(self, note_id: 'int') -> 'dict':
        result = self.resultAsDictionary(
            self.db.execute('SELECT id, doc_id, par_id, par_hash, content, created, modified, access, tags, html, UserGroup_id '
                            'FROM UserNotes '
                            'WHERE id = ?', [note_id]))

        return self.process_notes(result)[0]

    @contract
    def process_notes(self, result: 'list(dict)') -> 'list(dict)':
        for note in result:
            note["tags"] = self.__strtotags(note["tags"])
            if note['html'] is None:
                note['html'] = md_to_html(note['content'])
                self.db.execute('UPDATE UserNotes SET html = ? '
                                'WHERE id = ?', [note['html'], note['id']])
                self.db.commit()
        return result
