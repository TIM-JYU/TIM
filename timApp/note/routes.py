from dataclasses import dataclass, field
from datetime import datetime
from typing import Any

from flask import Response
from sqlalchemy import true, select
from sqlalchemy.orm import selectinload

from timApp.auth.accesshelper import (
    verify_comment_right,
    verify_logged_in,
    get_doc_or_abort,
    AccessDenied,
    verify_teacher_access,
    has_manage_access,
    verify_route_access,
)
from timApp.auth.sessioninfo import get_current_user_object
from timApp.document.caching import clear_doc_cache
from timApp.document.docentry import DocEntry
from timApp.document.docinfo import DocInfo
from timApp.document.docparagraph import DocParagraph
from timApp.document.document import Document
from timApp.document.editing.globalparid import GlobalParId
from timApp.document.editing.routes import par_response
from timApp.document.viewcontext import ViewRoute
from timApp.folder.folder import Folder
from timApp.item.block import Block
from timApp.markdown.markdownconverter import md_to_html
from timApp.note.notes import tagstostr
from timApp.note.usernote import get_comment_by_id, UserNote
from timApp.notification.notification import NotificationType
from timApp.notification.notify import notify_doc_watchers
from timApp.notification.pending_notification import PendingNotification
from timApp.timdb.exceptions import TimDbException
from timApp.timdb.sqa import db
from timApp.user.user import User
from timApp.util.flask.requesthelper import RouteException, NotExist
from timApp.util.flask.responsehelper import json_response
from timApp.util.flask.typedblueprint import TypedBlueprint
from timApp.util.utils import get_current_time

notes = TypedBlueprint(
    "notes",
    __name__,
    url_prefix="",
)

KNOWN_TAGS = ["difficult", "unclear"]


def has_note_edit_access(n: UserNote) -> bool:
    d = get_doc_or_abort(n.doc_id)
    g = get_current_user_object().get_personal_group()
    return n.usergroup == g or has_manage_access(d)


def get_comment_and_check_exists(note_id: int) -> UserNote:
    note = get_comment_by_id(note_id)
    if not note:
        raise NotExist("Comment not found. It may have been deleted.")
    return note


@notes.get("/note/<int:note_id>")
def get_note(note_id: int) -> Response:
    note = get_comment_and_check_exists(note_id)
    if not has_note_edit_access(note):
        raise AccessDenied()
    return json_response(
        {"text": note.content, "extraData": note}, date_conversion=True
    )


@dataclass
class DeletedNote:
    notification: PendingNotification

    @property
    def access(self) -> str:
        return "everyone"

    def to_json(self) -> dict[str, Any]:
        d = self.notification.block.docentries[0]
        return {
            "id": None,
            "doc_id": self.notification.doc_id,
            "doc_title": d.title,
            "par_id": self.notification.par_id,
            "par_hash": None,
            "content": self.notification.text,
            "created": None,
            "modified": None,
            "deleted_on": self.notification.created,
            "access": "everyone",
            "usergroup": None,
            "user_who_deleted": self.notification.user,
            "url": d.url + "#" + self.notification.par_id,
        }


@notes.get("/notes/<path:item_path>")
def get_notes(
    item_path: str,
    private: bool = False,
    deleted: bool = False,
    start: datetime | None = None,
    end: datetime | None = None,
) -> Response:
    """Gets all notes in a document or folder.

    :param item_path: Path of the item.
    :param private: Whether private comments should be included; only usable for admins
    :param deleted: Whether deleted public comments should be included
    :param start: Start timestamp of notes.
    :param end: End timestamp of notes.
    """
    i: Folder | DocInfo | None = Folder.find_by_path(item_path)
    if not i:
        i = DocEntry.find_by_path(item_path)
    if not i:
        raise RouteException("Item not found.")
    u = get_current_user_object()
    if isinstance(i, Folder):
        all_docs = i.get_all_documents(
            include_subdirs=True,
        )
        if not all(u.has_teacher_access(d) for d in all_docs):
            raise AccessDenied(
                "You do not have teacher access to all documents in this folder."
            )
        docs = all_docs
    else:
        verify_teacher_access(i)
        docs = [i]
    access_restriction = UserNote.access == "everyone"
    if private:
        access_restriction = true()
    time_restriction = true()
    if start:
        time_restriction = time_restriction & (UserNote.created >= start)
    if end:
        time_restriction = time_restriction & (UserNote.created < end)
    d_ids = [d.id for d in docs]
    ns = (
        db.session.execute(
            select(UserNote)
            .filter(UserNote.doc_id.in_(d_ids) & access_restriction & time_restriction)
            .options(selectinload(UserNote.usergroup))
            .options(selectinload(UserNote.block).selectinload(Block.docentries))
        )
        .scalars()
        .all()
    )
    all_count = len(ns)
    if not u.is_admin:
        ns = [n for n in ns if n.access == "everyone"]
    if deleted:
        deleted_notes = list(
            map(
                DeletedNote,
                db.session.execute(
                    select(PendingNotification)
                    .filter(
                        PendingNotification.doc_id.in_(d_ids)
                        & (PendingNotification.kind == NotificationType.CommentDeleted)
                    )
                    .options(
                        selectinload(PendingNotification.block).selectinload(
                            Block.docentries
                        )
                    )
                )
                .scalars()
                .all(),
            )
        )
        ns += deleted_notes
        all_count += len(deleted_notes)
    public_count = 0
    deleted_count = 0
    for n in ns:
        if isinstance(n, DeletedNote):
            deleted_count += 1
        if n.access == "everyone":
            public_count += 1
    extra = {}
    if deleted:
        extra["deleted_everyone"] = deleted_count
        extra["not_deleted_everyone"] = public_count - deleted_count
    if private:
        extra["justme"] = all_count - public_count
    return json_response(
        {
            "counts": {
                **extra,
                "everyone": public_count,
                "all": all_count,
            },
            "notes": ns,
        }
    )


def check_note_access_ok(is_public: bool, doc: Document) -> None:
    if is_public and doc.get_settings().comments() == "private":
        raise AccessDenied("Only private comments can be posted on this document.")


def clear_doc_cache_after_comment(
    docinfo: DocInfo, user: User, is_public: bool
) -> None:
    if is_public:
        clear_doc_cache(docinfo, user=None)
    else:
        clear_doc_cache(docinfo, user)


@dataclass
class ParContext:
    """This roughly corresponds to the ParContext TypeScript class."""

    curr: GlobalParId
    orig: GlobalParId

    def has_diff_orig(self) -> bool:
        return self.curr != self.orig


@notes.post("/postNote")
def post_note(
    text: str,
    access: str,
    ctx: ParContext,
    tags: dict[str, bool] | None = None,
    view: ViewRoute = field(default=ViewRoute.View, metadata={"by_value": True}),
) -> Response:
    is_public = access == "everyone"
    got_tags = []
    for tag in KNOWN_TAGS:
        if tags and tags.get(tag):
            got_tags.append(tag)
    orig_docinfo, p = check_permissions_and_get_orig(ctx, is_public)
    verify_route_access(orig_docinfo, view)

    curr_user = get_current_user_object()
    n = UserNote(
        usergroup=curr_user.get_personal_group(),
        doc_id=p.get_doc_id(),
        par_id=p.get_id(),
        par_hash=p.get_hash(),
        content=text,
        access=access,
        html=md_to_html(text),
        tags=tagstostr(got_tags),
    )
    db.session.add(n)

    if is_public:
        notify_doc_watchers(orig_docinfo, text, NotificationType.CommentAdded, p)
    clear_doc_cache_after_comment(orig_docinfo, curr_user, is_public)
    return comment_response(ctx, orig_docinfo, p, view)


def check_permissions_and_get_orig(
    ctx: ParContext, is_public: bool
) -> tuple[DocInfo, DocParagraph]:
    orig_docinfo = get_doc_or_abort(ctx.orig.doc_id)
    orig_doc = orig_docinfo.document
    check_note_access_ok(is_public, orig_doc)
    verify_comment_right(orig_docinfo)
    try:
        orig_p = orig_doc.get_paragraph(ctx.orig.par_id)
    except TimDbException as e:
        raise NotExist(str(e))
    if ctx.has_diff_orig() and not orig_p.is_translation():
        docinfo = get_doc_or_abort(ctx.curr.doc_id)
        doc = docinfo.document
        try:
            p = doc.get_paragraph(ctx.curr.par_id)
        except TimDbException as e:
            raise NotExist(str(e))
        check_note_access_ok(is_public, doc)
        verify_comment_right(docinfo)
    else:
        p = orig_p
    return orig_docinfo, p


@notes.post("/editNote")
def edit_note(
    id: int,
    ctx: ParContext,
    text: str,
    access: str,
    tags: dict[str, bool] | None = None,
    view: ViewRoute = field(default=ViewRoute.View, metadata={"by_value": True}),
) -> Response:
    verify_logged_in()
    note_id = id
    n = get_comment_and_check_exists(note_id)
    d = get_doc_or_abort(n.doc_id)
    verify_route_access(d, view)
    is_public = access == "everyone"
    orig_docinfo, p = check_permissions_and_get_orig(ctx, is_public)
    check_note_ctx_match(n, p)
    par = p
    got_tags = []
    for tag in KNOWN_TAGS:
        if tags and tags.get(tag):
            got_tags.append(tag)
    if not has_note_edit_access(n):
        raise AccessDenied("Sorry, you don't have permission to edit this note.")
    n.content = text
    n.html = md_to_html(text)
    was_public = n.is_public
    n.access = access
    n.tags = tagstostr(got_tags)
    n.modified = get_current_time()

    if n.is_public:
        notify_doc_watchers(d, text, NotificationType.CommentModified, par)
    clear_doc_cache_after_comment(d, get_current_user_object(), is_public or was_public)
    return comment_response(ctx, orig_docinfo, p, view)


@notes.post("/deleteNote")
def delete_note(
    id: int,
    ctx: ParContext,
    view: ViewRoute = field(default=ViewRoute.View, metadata={"by_value": True}),
) -> Response:
    note_id = id
    note = get_comment_and_check_exists(note_id)
    orig_docinfo, p = check_permissions_and_get_orig(ctx, is_public=False)
    verify_route_access(orig_docinfo, view)
    if not has_note_edit_access(note):
        raise AccessDenied("Sorry, you don't have permission to remove this note.")
    check_note_ctx_match(note, p)
    try:
        orig_p = orig_docinfo.document.get_paragraph(ctx.orig.par_id)
    except TimDbException:
        raise RouteException(
            "Cannot delete the note because the paragraph has been deleted."
        )
    db.session.delete(note)
    is_public = note.is_public
    if is_public:
        notify_doc_watchers(
            orig_docinfo, note.content, NotificationType.CommentDeleted, orig_p
        )
    clear_doc_cache_after_comment(orig_docinfo, get_current_user_object(), is_public)
    return comment_response(ctx, orig_docinfo, p, view)


def comment_response(
    ctx: ParContext, orig_docinfo: DocInfo, p: DocParagraph, view: ViewRoute
) -> Response:
    return par_response(
        [orig_docinfo.document.get_paragraph(ctx.orig.par_id)],
        orig_docinfo,
        filter_return=ctx.curr
        if not p.is_translation()
        else GlobalParId(doc_id=ctx.orig.doc_id, par_id=ctx.curr.par_id),
        for_view=view,
    )


def check_note_ctx_match(n: UserNote, p: DocParagraph) -> None:
    if n.par_id != p.get_id():
        raise RouteException("par_id mismatch")
    if n.doc_id != p.get_doc_id():
        raise RouteException("doc_id mismatch")
