from timApp.auth.accesshelper import verify_view_access
from timApp.document.docentry import DocEntry
from timApp.document.docinfo import DocInfo
from timApp.item.tag import TagType
from timApp.user.groups import verify_group_edit_access
from timApp.user.usergroup import UserGroup


def is_course(d: DocInfo) -> bool:
    return any(t.type == TagType.CourseCode for t in d.block.tags)


class CourseException(Exception):
    pass


def verify_valid_course(path: str) -> UserGroup:
    d = DocEntry.find_by_path(path)
    if not d:
        raise CourseException('Document not found.')
    verify_view_access(d)
    s = d.document.get_settings()
    if is_course(d):
        if not s.course_allow_manual_enroll():
            raise CourseException('Course does not allow manual enrollment.')
        doc_group = s.group()
        if isinstance(doc_group, str):
            ug = UserGroup.get_by_name(doc_group)
            if not ug:
                raise CourseException(f'The specified course group "{doc_group}" does not exist.')
            group_tags = [t.get_group_name() for t in d.block.tags if t.get_group_name()]
            if doc_group not in group_tags:
                raise CourseException('Document group setting not found in tags.')
            owners = d.owners
            if not owners:
                raise CourseException('Document does not have owners.')
            if not all(verify_group_edit_access(ug, u, require=False) for owner in owners for u in owner.users):
                raise CourseException(f'Some of the document owners does not have edit access to the course group "{doc_group}".')
            return ug
        elif doc_group:
            raise CourseException(f'Invalid group setting: {doc_group}')
        else:
            raise CourseException('Document does not have associated course group')
    else:
        raise CourseException('Document is not tagged as a course')
