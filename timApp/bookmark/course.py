from sqlalchemy import select

from timApp.auth.sessioninfo import get_current_user_object
from timApp.bookmark.bookmarks import Bookmarks, HIDDEN_COURSES_GROUP, MY_COURSES_GROUP
from timApp.document.course.validate import is_course
from timApp.document.docentry import DocEntry
from timApp.document.docinfo import DocInfo
from timApp.item.block import Block
from timApp.item.tag import Tag, GROUP_TAG_PREFIX
from timApp.timdb.sqa import db
from timApp.user.usergroup import UserGroup
from timApp.util.utils import get_current_time


def update_user_course_bookmarks() -> None:
    u = get_current_user_object()
    now = get_current_time()
    for gr in u.groups:  # type: UserGroup
        if gr.is_sisu_student_group or gr.is_self_join_course:
            docs = db.session.execute(
                select(DocEntry).join(Block)
                .join(Tag)
                .filter(
                    (Tag.name == GROUP_TAG_PREFIX + gr.name)
                    & ((Tag.expires == None) | (Tag.expires > now))
                )

            ).scalars().all()
            if not docs:
                continue
            if len(docs) > 1:
                continue
            d: DocEntry = docs[0]
            if not is_course(d):
                continue
            if d.document.get_settings().sisu_require_manual_enroll():
                continue
            b = Bookmarks(u)
            add_to_course_bookmark(b, d)


def add_to_course_bookmark(
    b: Bookmarks, d: DocInfo, skip_if_hidden: bool = True
) -> None:
    if b.has_bookmark(HIDDEN_COURSES_GROUP, d.title):
        if skip_if_hidden:
            return
        b.delete_bookmark(HIDDEN_COURSES_GROUP, d.title)
    b.add_bookmark(
        MY_COURSES_GROUP, d.title, d.url_relative, move_to_top=False
    ).save_bookmarks()
