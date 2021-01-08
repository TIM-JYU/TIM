from dataclasses import dataclass

from flask import Blueprint, request
from flask import current_app
from flask import g

from marshmallow_dataclass import class_schema
from timApp.auth.accesshelper import verify_logged_in, get_doc_or_abort, verify_view_access
from timApp.auth.sessioninfo import get_current_user_object
from timApp.bookmark.bookmarks import Bookmarks
from timApp.document.course.validate import CourseException, verify_valid_course
from timApp.document.docentry import DocEntry
from timApp.document.docinfo import DocInfo
from timApp.timdb.sqa import db
from timApp.util.flask.requesthelper import use_model, RouteException, NotExist
from timApp.util.flask.responsehelper import json_response

bookmarks = Blueprint('bookmarks',
                      __name__,
                      url_prefix='/bookmarks')


@dataclass
class WithBookmarks:
    bookmarks: Bookmarks


wb: WithBookmarks = g


@bookmarks.before_request
def verify_login():
    verify_logged_in()
    wb.bookmarks = Bookmarks(get_current_user_object())


@dataclass
class BookmarkNoLink:
    group: str
    name: str


@dataclass
class BookmarkModel(BookmarkNoLink):
    link: str


@bookmarks.route('/add', methods=['POST'])
@use_model(BookmarkModel)
def add_bookmark(m: BookmarkModel):
    wb.bookmarks.add_bookmark(m.group, m.name, m.link).save_bookmarks()
    return get_bookmarks()


@dataclass
class AddCourseModel:
    path: str
    require_group: bool = False


@bookmarks.route('/addCourse', methods=['POST'])
@use_model(AddCourseModel)
def add_course_bookmark(m: AddCourseModel):
    d = DocEntry.find_by_path(m.path)
    if not d:
        raise NotExist('Course not found')
    verify_view_access(d)
    added_to_group = False
    try:
        ug = verify_valid_course(m.path)
    except CourseException as e:
        if m.require_group:
            raise RouteException(str(e))
    else:
        u = get_current_user_object()
        u.add_to_group(ug, added_by=u)
        added_to_group = True
        db.session.commit()
    add_to_course_bookmark(wb.bookmarks, d)
    return {
        'bookmarks': g.bookmarks.as_dict(),
        'added_to_group': added_to_group,
    }


def add_to_course_bookmark(b: Bookmarks, d: DocInfo):
    b.add_bookmark('My courses', d.title, d.url_relative, move_to_top=False).save_bookmarks()


@dataclass
class EditBookmarkModel:
    old: BookmarkNoLink
    new: BookmarkModel


@bookmarks.route('/edit', methods=['POST'])
@use_model(EditBookmarkModel)
def edit_bookmark(args: EditBookmarkModel):
    old_group = args.old.group
    old_name = args.old.name
    groupname = args.new.group
    item_name = args.new.name
    item_path = args.new.link
    wb.bookmarks.delete_bookmark(old_group, old_name).add_bookmark(groupname, item_name, item_path).save_bookmarks()
    return get_bookmarks()


@bookmarks.route('/createGroup/<groupname>', methods=['POST'])
def create_bookmark_group(groupname):
    wb.bookmarks.add_group(groupname).save_bookmarks()
    return get_bookmarks()


@dataclass
class DeleteBookmarkGroupModel:
    group: str


@bookmarks.route('/deleteGroup', methods=['POST'])
@use_model(DeleteBookmarkGroupModel)
def delete_bookmark_group(args: DeleteBookmarkGroupModel):
    wb.bookmarks.delete_group(args.group).save_bookmarks()
    return get_bookmarks()


@bookmarks.route('/delete', methods=['POST'])
@use_model(BookmarkNoLink)
def delete_bookmark(args: BookmarkNoLink):
    if args.group == 'My courses':
        bks = wb.bookmarks.as_dict()
        my_courses_group = None
        for b in bks:
            if b.get('name') == 'My courses':
                my_courses_group = b
                break
        if my_courses_group:
            for item in my_courses_group['items']:
                if item.get('name') == args.name:
                    path = item.get('link')
                    if path:
                        path = path[len('/view/'):]
                        try:
                            ug = verify_valid_course(path)
                        except CourseException:
                            continue
                        u = get_current_user_object()
                        for m in u.memberships:
                            if m.usergroup_id == ug.id and m.adder == u:
                                m.set_expired()
                                break
                        db.session.commit()
    wb.bookmarks.delete_bookmark(args.group, args.name).save_bookmarks()
    return get_bookmarks()


@dataclass
class MarkLastReadModel:
    view: str


MLRSchema = class_schema(MarkLastReadModel)

@bookmarks.route('/markLastRead/<int:doc_id>', methods=['POST'])
def mark_last_read(doc_id):
    d = get_doc_or_abort(doc_id)
    verify_view_access(d)
    mlrm: MarkLastReadModel = MLRSchema().load(request.get_json())
    wb.bookmarks.add_bookmark('Last read',
                             d.title,
                             d.get_relative_url_for_view(mlrm.view),
                             move_to_top=True,
                             limit=current_app.config['LAST_READ_BOOKMARK_LIMIT']).save_bookmarks()
    return get_bookmarks()


# noinspection PyUnusedLocal
@bookmarks.route('/get')
@bookmarks.route('/get/<int:user_id>')
def get_bookmarks(user_id=None):
    """Gets user bookmark data for the currently logged in user.

    Parameter user_id is unused for now.

    """

    db.session.commit()
    return json_response(g.bookmarks.as_dict())
