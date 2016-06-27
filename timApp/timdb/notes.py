from typing import List

from documentmodel.docparagraph import DocParagraph
from documentmodel.document import Document
from markdownconverter import md_to_html
from timdb.timdbbase import TimDbBase


class Notes(TimDbBase):
    def __tagstostr(self, tags: List[str]) -> str:
        tagstr = ''
        if 'difficult' in tags:
            tagstr += 'd'
        if 'unclear' in tags:
            tagstr += 'u'
        return tagstr

    def __strtotags(self, tagstr: str) -> List[str]:
        tags = []
        if 'd' in tagstr:
            tags.append("difficult")
        if 'u' in tagstr:
            tags.append("unclear")
        return tags

    def has_edit_access(self, usergroup_id: int, note_id: int) -> bool:
        """Checks whether the specified usergroup has access to the specified note.

        :param usergroup_id: The usergroup id for which to check access.
        :param note_id: The id of the note.
        """
        cursor = self.db.cursor()

        cursor.execute(
            """
                SELECT UserGroup_id FROM UserNotes
                WHERE id = %s
            """, [note_id])
        row = cursor.fetchone()
        return row is not None and int(row[0]) == usergroup_id

    def add_note(self, usergroup_id: int, doc: Document, par: DocParagraph, content: str, access: str,
                 tags: List[str], commit: bool=True):
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
                VALUES (%s, %s, %s, %s, %s, CURRENT_TIMESTAMP, NULL, %s, %s, %s)
            """, [usergroup_id, doc.doc_id, par.get_id(), par.get_hash(),
                  content, access, self.__tagstostr(tags), note_html])

        if commit:
            self.db.commit()

    def modify_note(self, note_id: int, new_content: str,
                    access: str, new_tags: List[str]):
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
                SET content = %s, tags = %s, access = %s, html = %s, modified = CURRENT_TIMESTAMP
                WHERE id = %s
            """, [new_content, self.__tagstostr(new_tags), access, new_html, note_id])

        self.db.commit()

    def move_notes(self, src_par: DocParagraph, dest_par: DocParagraph, commit: bool = True):
        if str(src_par.doc.doc_id) == str(dest_par.doc.doc_id) and str(src_par.get_id()) == str(dest_par.get_id()):
            return

        """Moves all notes from one paragraph to another.
        :param src_par: Source paragraph
        :param dest_par: Destination paragraph
        :param commit Whether to commit changes to database
        """
        cursor = self.db.cursor()
        cursor.execute("UPDATE UserNotes SET doc_id = %s, par_id = %s WHERE doc_id = %s AND par_id = %s",
                       [dest_par.doc.doc_id, dest_par.get_id(), src_par.doc.doc_id, src_par.get_id()])

        if commit:
            self.db.commit()

    def delete_note(self, note_id: int):
        """Deletes a note.

        :param note_id: The id of the note.
        """
        cursor = self.db.cursor()

        cursor.execute(
            """
                DELETE FROM UserNotes
                WHERE id = %s
            """, [note_id])

        self.db.commit()

    def get_notes(self, usergroup_id: int, doc: Document, include_public=True) -> List[dict]:
        """Gets all notes for a document a particular user has access to.

        :param usergroup_id: The usergroup id.
        :param doc: The document for which to get the notes.
        """
        ids = doc.get_referenced_document_ids()
        ids.add(doc.doc_id)
        template = ','.join(['%s'] * len(ids))
        include_public_sql = ''
        if include_public:
            include_public_sql = "OR access = 'everyone'"
        c = self.db.cursor()
        c.execute("""SELECT UserNotes.id, par_id, doc_id, par_hash, content,
                            created,
                            modified,
                            access, tags, html,
                            UserNotes.UserGroup_id, UserAccount.name as user_name,
                            UserAccount.real_name, UserAccount.email as user_email
                     FROM UserNotes
                     JOIN UserGroupMember ON UserNotes.UserGroup_id = UserGroupMember.UserGroup_id
                     JOIN UserAccount ON UserGroupMember.User_id = UserAccount.id
                     WHERE (UserNotes.UserGroup_id = %s {}) AND doc_id IN ({})""".format(include_public_sql, template),
                  [usergroup_id] + list(ids))
        result = self.resultAsDictionary(c)
        return self.process_notes(result)

    def get_note(self, note_id: int) -> dict:
        c = self.db.cursor()
        c.execute('SELECT id, doc_id, par_id, par_hash, content, created, modified, access, tags, html, UserGroup_id '
                  'FROM UserNotes '
                  'WHERE id = %s', [note_id])
        result = self.resultAsDictionary(c)

        return self.process_notes(result)[0]

    def process_notes(self, result: List[dict]) -> List[dict]:
        c = self.db.cursor()
        for note in result:
            note["tags"] = self.__strtotags(note["tags"])
            if note['html'] is None:
                note['html'] = md_to_html(note['content'])
                c.execute('UPDATE UserNotes SET html = %s '
                                'WHERE id = %s', [note['html'], note['id']])

        self.db.commit()
        return result
