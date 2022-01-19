from dataclasses import dataclass
from typing import Optional

from flask import current_app, Response

from timApp.auth.accesshelper import (
    verify_logged_in,
    get_doc_or_abort,
    verify_view_access,
    verify_admin,
)
from timApp.auth.sessioninfo import get_current_user_object
from timApp.bookmark.bookmarks import (
    Bookmarks,
    MY_COURSES_GROUP,
    BookmarkDictGroup,
    HIDDEN_COURSES_GROUP,
)
from timApp.document.course.validate import CourseException, verify_valid_course
from timApp.document.docentry import DocEntry
from timApp.document.docinfo import DocInfo
from timApp.timdb.sqa import db
from timApp.user.user import User
from timApp.util.flask.requesthelper import use_model, RouteException, NotExist
from timApp.util.flask.responsehelper import json_response
from timApp.util.flask.typedblueprint import TypedBlueprint

bookmarks = TypedBlueprint("bookmarks", __name__, url_prefix="/bookmarks")


@dataclass
class BookmarkNoLink:
    group: str
    name: str


@dataclass
class BookmarkWithLink(BookmarkNoLink):
    link: str


@bookmarks.post("/add", own_model=True)
@use_model(BookmarkWithLink)
def add_bookmark(m: BookmarkWithLink) -> Response:
    verify_logged_in()
    u = get_current_user_object()
    u.bookmarks.add_bookmark(m.group, m.name, m.link).save_bookmarks()
    db.session.commit()
    return _bookmarks_response()


@bookmarks.post("/addCourse")
def add_course_bookmark(path: str, require_group: bool = False) -> Response:
    d = DocEntry.find_by_path(path)
    if not d:
        raise NotExist("Course not found")
    verify_view_access(d)
    added_to_group = False
    u = get_current_user_object()
    try:
        ug = verify_valid_course(path)
    except CourseException as e:
        if require_group:
            raise RouteException(str(e))
    else:
        u.add_to_group(ug, added_by=u)
        added_to_group = True
    add_to_course_bookmark(u.bookmarks, d)
    db.session.commit()
    return json_response(
        {
            "bookmarks": get_current_user_object().bookmarks.as_dict(),
            "added_to_group": added_to_group,
        }
    )


def add_to_course_bookmark(b: Bookmarks, d: DocInfo) -> None:
    if b.has_bookmark(HIDDEN_COURSES_GROUP, d.title):
        return
    b.add_bookmark(
        MY_COURSES_GROUP, d.title, d.url_relative, move_to_top=False
    ).save_bookmarks()


@bookmarks.post("/edit")
def edit_bookmark(old: BookmarkNoLink, new: BookmarkWithLink) -> Response:
    verify_logged_in()
    old_group = old.group
    old_name = old.name
    groupname = new.group
    item_name = new.name
    item_path = new.link
    u = get_current_user_object()
    u.bookmarks.delete_bookmark(old_group, old_name).add_bookmark(
        groupname, item_name, item_path
    ).save_bookmarks()
    db.session.commit()
    return _bookmarks_response()


@bookmarks.post("/createGroup/<groupname>")
def create_bookmark_group(groupname: str) -> Response:
    verify_logged_in()
    u = get_current_user_object()
    u.bookmarks.add_group(groupname).save_bookmarks()
    db.session.commit()
    return _bookmarks_response()


@bookmarks.post("/deleteGroup")
def delete_bookmark_group(group: str) -> Response:
    verify_logged_in()
    u = get_current_user_object()
    u.bookmarks.delete_group(group).save_bookmarks()
    db.session.commit()
    return _bookmarks_response()


@bookmarks.post("/delete", own_model=True)
@use_model(BookmarkNoLink)
def delete_bookmark(bm: BookmarkNoLink) -> Response:
    verify_logged_in()
    u = get_current_user_object()
    hide_course = False
    course_link: Optional[str] = None
    if bm.group == MY_COURSES_GROUP:
        hide_course = True
        bks = u.bookmarks.as_dict()
        my_courses_group: Optional[BookmarkDictGroup] = next(
            (b for b in bks if b["name"] == bm.group), None
        )
        if my_courses_group:
            for item in my_courses_group["items"]:
                if item["name"] != bm.name:
                    continue
                course_link = item["link"]
                if not course_link:
                    continue
                path = course_link[len("/view/") :]
                try:
                    ug = verify_valid_course(path)
                except CourseException:
                    continue
                self_enrolled_group_membership = next(
                    (
                        m
                        for m in u.memberships
                        if m.usergroup_id == ug.id and m.adder == u
                    ),
                    None,
                )
                if self_enrolled_group_membership:
                    self_enrolled_group_membership.set_expired()
                    # Don't hide the course if it was self-enrolled, instead just remove it fully
                    hide_course = False

    if hide_course and course_link:
        u.bookmarks.add_bookmark(HIDDEN_COURSES_GROUP, bm.name, course_link)

    u.bookmarks.delete_bookmark(bm.group, bm.name).save_bookmarks()
    db.session.commit()
    return _bookmarks_response()


@bookmarks.post("/markLastRead/<int:doc_id>")
def mark_last_read(doc_id: int, view: str) -> Response:
    verify_logged_in()
    u = get_current_user_object()
    d = get_doc_or_abort(doc_id)
    verify_view_access(d)
    u.bookmarks.add_bookmark(
        "Last read",
        d.title,
        d.get_relative_url_for_view(view),
        move_to_top=True,
        limit=current_app.config["LAST_READ_BOOKMARK_LIMIT"],
    ).save_bookmarks()
    db.session.commit()
    return _bookmarks_response()


def _bookmarks_response() -> Response:
    return json_response(get_current_user_object().bookmarks.as_dict())


def _get_bookmarks_for(user: User) -> Response:
    return json_response(user.bookmarks.as_dict())


@bookmarks.get("/get/<int:user_id>")
def get_bookmarks(user_id: int) -> Response:
    """Gets user bookmark data for a specific user."""
    verify_admin()
    u = User.get_by_id(user_id)
    if not u:
        raise NotExist("User not found")
    return _get_bookmarks_for(u)


@bookmarks.get("/get")
def get_bookmarks_current() -> Response:
    """Gets user bookmark data for the currently logged-in user."""
    verify_logged_in()
    return _get_bookmarks_for(get_current_user_object())
