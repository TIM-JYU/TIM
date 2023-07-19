from typing import NamedTuple

from sqlalchemy import select
from sqlalchemy.orm import lazyload

from timApp.document.docparagraph import DocParagraph
from timApp.document.document import Document
from timApp.markdown.markdownconverter import md_to_html
from timApp.note.usernote import UserNote
from timApp.timdb.sqa import db
from timApp.user.user import User
from timApp.user.usergroup import UserGroup


def tagstostr(tags: list[str]) -> str:
    tagstr = ""
    if "difficult" in tags:
        tagstr += "d"
    if "unclear" in tags:
        tagstr += "u"
    return tagstr


def strtotags(tagstr: str) -> list[str]:
    tags = []
    if "d" in tagstr:
        tags.append("difficult")
    if "u" in tagstr:
        tags.append("unclear")
    return tags


class UserNoteAndUser(NamedTuple):
    user: User
    note: UserNote
    editable: bool
    private: bool


def process_notes(result: list[tuple[UserNote, User]]) -> list[tuple[UserNote, User]]:
    for note, u in result:
        if note.html is None:
            note.html = md_to_html(note.content)
    return result


def get_notes(
    usergroup_id: int, doc: Document, include_public=True
) -> list[tuple[UserNote, User]]:
    """Gets all notes for a document a particular user has access to.

    :param usergroup_id: The usergroup id.
    :param doc: The document for which to get the notes.

    """
    ids = doc.get_referenced_document_ids()
    ids.add(doc.doc_id)
    f = UserGroup.id == usergroup_id
    if include_public:
        f = f | (UserNote.access == "everyone")
    stmt = (
        select(UserNote, User)
        .filter(UserNote.doc_id.in_(ids))
        .join(UserGroup)
        .join(User, User.name == UserGroup.name)
        .options(lazyload("*"))
        .filter(f)
        .order_by(UserNote.id)
    )
    return process_notes(db.session.execute(stmt).all())


def move_notes(src_par: DocParagraph, dest_par: DocParagraph):
    """Moves all notes from one paragraph to another.
    :param src_par: Source paragraph
    :param dest_par: Destination paragraph
    """
    if str(src_par.doc.doc_id) == str(dest_par.doc.doc_id) and str(
        src_par.get_id()
    ) == str(dest_par.get_id()):
        return

    for u in db.session.execute(
        select(UserNote).filter_by(doc_id=src_par.doc.doc_id, par_id=src_par.get_id())
    ).scalars():
        u.doc_id = dest_par.doc.doc_id
        u.par_id = dest_par.get_id()
