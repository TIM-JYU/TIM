"""
Routes related to tags.
"""
from dataclasses import dataclass, field
from datetime import datetime

from flask import request, Response
from sqlalchemy import func, select
from sqlalchemy.exc import IntegrityError
from sqlalchemy.orm import joinedload
from sqlalchemy.orm.exc import UnmappedInstanceError, FlushError  # type: ignore

from timApp.auth.accesshelper import (
    verify_view_access,
    verify_manage_access,
    check_admin_access,
)
from timApp.auth.sessioninfo import get_current_user_object
from timApp.document.docentry import DocEntry, get_documents
from timApp.document.docinfo import DocInfo
from timApp.item.block import Block
from timApp.item.tag import Tag, TagType, GROUP_TAG_PREFIX
from timApp.timdb.sqa import db
from timApp.user.groups import verify_group_view_access
from timApp.user.special_group_names import TEACHERS_GROUPNAME
from timApp.user.usergroup import UserGroup
from timApp.util.flask.requesthelper import (
    get_option,
    RouteException,
    NotExist,
)
from timApp.util.flask.responsehelper import ok_response, json_response
from timApp.util.flask.typedblueprint import TypedBlueprint

tags_blueprint = TypedBlueprint("tags", __name__, url_prefix="/tags")


@dataclass
class TagInfo:
    name: str
    type: TagType = field(metadata={"by_value": True})
    expires: datetime | None = None
    block_id: int | None = None

    def to_tag(self) -> Tag:
        return Tag(name=self.name, type=self.type, expires=self.expires)


@tags_blueprint.post("/add/<path:doc>")
def add_tag(doc: str, tags: list[TagInfo]) -> Response:
    """
    Adds a tag-document entry into the database.

    :param doc: The target document.
    :param tags: Tags to add.
    :returns Tag adding success response.
    """

    d = DocEntry.find_by_path(doc)
    if not d:
        raise NotExist()
    verify_manage_access(d)
    add_tags(d, tags)
    return commit_and_ok()


def commit_and_ok() -> Response:
    try:
        db.session.commit()
    except (IntegrityError, FlushError):
        db.session.rollback()
        raise RouteException("Tag name is already in use.")
    return ok_response()


def add_tags(d: DocInfo, tags: list[TagInfo]) -> None:
    for tag_info in tags:
        tag = tag_info.to_tag()
        check_tag_access(tag)
        d.block.tags.append(tag)


def check_tag_access(tag: Tag, check_group: bool = True) -> None:
    """
    Checks whether the user is allowed to make changes to the tag type.
    If not allowed, gives abort response.

    :param check_group: Whether to check group tag right. Should be false when deleting a tag.
    :param tag: The tag to check.
    """
    if tag.type != TagType.Regular:
        ug = UserGroup.get_by_name(TEACHERS_GROUPNAME)
        if ug not in get_current_user_object().groups and not check_admin_access():
            raise RouteException(
                f"Managing this tag requires admin or {TEACHERS_GROUPNAME} rights."
            )
    groupname = tag.get_group_name()
    if groupname and check_group:
        ug = UserGroup.get_by_name(groupname)
        if not ug:
            raise NotExist(f'Usergroup "{groupname}" not found.')
        verify_group_view_access(ug)


@tags_blueprint.post("/setCourseTags/<path:doc>")
def set_group_tags(
    doc: str,
    groups: list[str],
    tags: list[TagInfo],
    groups_expire: datetime | None = None,
) -> Response:
    d = DocEntry.find_by_path(doc)
    if not d:
        raise NotExist()
    verify_manage_access(d)
    existing_tags: list[Tag] = d.block.tags
    tags_to_remove = [
        t
        for t in existing_tags
        if t.get_group_name() or t.type in (TagType.CourseCode, TagType.Subject)
    ]
    for t in tags_to_remove:
        check_tag_access(t, check_group=False)
        db.session.delete(t)

    add_tags(d, tags)

    for g in groups:
        t = Tag(name=GROUP_TAG_PREFIX + g, type=TagType.Regular, expires=groups_expire)
        check_tag_access(t)
        d.block.tags.append(t)
    return commit_and_ok()


@tags_blueprint.post("/edit/<path:doc>")
def edit_tag(doc: str, old_tag: TagInfo, new_tag: TagInfo) -> Response:
    """
    Replaces a tag-document entry in the database with new one.

    :param doc: The target document.
    :param old_tag: Tag to remove
    :param new_tag: Tag to replace old tag with
    :returns Edit success response.
    """
    d = DocEntry.find_by_path(doc)
    if not d:
        raise NotExist()
    verify_manage_access(d)

    # If commas in the name, use first part.
    new_tag_name = new_tag.name.split(",", 1)[0]

    new_tag_obj = Tag(name=new_tag_name, expires=new_tag.expires, type=new_tag.type)
    old_tag_obj = (
        db.session.execute(
            select(Tag).filter_by(block_id=d.id, name=old_tag.name, type=old_tag.type)
        )
        .scalars()
        .first()
    )

    if not old_tag_obj:
        raise RouteException("Tag to edit not found.")
    check_tag_access(old_tag_obj)
    check_tag_access(new_tag_obj)
    try:
        db.session.delete(old_tag_obj)
        d.block.tags.append(new_tag_obj)
        db.session.commit()
    except (IntegrityError, FlushError):
        raise RouteException("Tag editing failed! New tag name may already be in use")
    return ok_response()


@tags_blueprint.post("/remove/<path:doc>")
def remove_tag(doc: str, tag: TagInfo) -> Response:
    """
    Removes a tag-document entry from the database.

    :param doc: The target document.
    :param tag: Tag to remove
    :returns Removal success response.
    """
    d = DocEntry.find_by_path(doc)
    if not d:
        raise NotExist()
    verify_manage_access(d)

    tag_obj = (
        db.session.execute(
            select(Tag).filter_by(block_id=d.id, name=tag.name, type=tag.type)
        )
        .scalars()
        .first()
    )

    if not tag_obj:
        raise RouteException("Tag not found.")
    check_tag_access(tag_obj)
    try:
        db.session.delete(tag_obj)
        db.session.commit()
    except (IntegrityError, UnmappedInstanceError):
        raise RouteException("Tag removal failed.")
    return ok_response()


@tags_blueprint.get("/getTags/<path:doc>")
def get_tags(doc: str) -> Response:
    """
    Gets the list of a document's tags.

    :param doc: The target document.
    :returns The list of document's Tag-objects converted into JSON.
    """
    d = DocEntry.find_by_path(doc)
    if not d:
        raise NotExist()
    verify_view_access(d)
    tags = d.block.tags
    return json_response(tags, date_conversion=True)


@tags_blueprint.get("/getAllTags")
def get_all_tags() -> Response:
    """
    Gets the list of all unique tags used in any document, regardless
    of expiration.
    :returns The list of all unique tag names as list of strings.
    """
    tags = db.session.execute(select(Tag)).scalars().all()

    tags_unique = set()
    for tag in tags:
        tags_unique.add(tag.name)
    return json_response(sorted(list(tags_unique)), date_conversion=True)


@tags_blueprint.get("/getDocs")
def get_tagged_documents() -> Response:
    """
    Gets a list of documents that have a certain tag.
    Options:
    - Search exact or partial words.
    - Case sensitivity
    - Get all other tags in the document as well or don't fetch them.
    :returns Response containing list of documents with the searched tag.
    """
    tag_name = request.args.get("name", "")
    exact_search = get_option(request, "exact_search", default=False, cast=bool)
    list_doc_tags = get_option(request, "list_doc_tags", default=False, cast=bool)
    case_sensitive = get_option(request, "case_sensitive", default=False, cast=bool)

    if exact_search:
        if case_sensitive:
            custom_filter = DocEntry.id.in_(
                select(Tag.block_id).filter_by(name=tag_name)
            )
        else:
            custom_filter = DocEntry.id.in_(
                select(Tag.block_id).filter(
                    func.lower(Tag.name) == func.lower(tag_name)
                )
            )

    else:
        tag_name = f"%{tag_name}%"
        if case_sensitive:
            custom_filter = DocEntry.id.in_(
                select(Tag.block_id).filter(
                    Tag.name.like(tag_name)
                    & ((Tag.expires > datetime.now()) | (Tag.expires == None))
                )
            )
        else:
            custom_filter = DocEntry.id.in_(
                select(Tag.block_id).filter(
                    Tag.name.ilike(tag_name)
                    & ((Tag.expires > datetime.now()) | (Tag.expires == None))
                )
            )

    if list_doc_tags:
        query_options = joinedload(DocEntry._block).joinedload(Block.tags)
    else:
        query_options = None

    docs = get_documents(
        filter_user=get_current_user_object(),
        custom_filter=custom_filter,
        query_options=query_options,
    )
    return json_response(docs, date_conversion=True)


@tags_blueprint.get("/getDoc/<int:doc_id>")
def get_tagged_document_by_id(doc_id: int) -> Response:
    """
    Gets a document and its tags by id.
    :param doc_id: Searched document id.
    :return: A DocEntry with tags.
    """
    docs = get_documents(
        filter_user=get_current_user_object(),
        custom_filter=DocEntry.id.in_([doc_id]),
        query_options=joinedload(DocEntry._block).joinedload(Block.tags),
    )
    if not docs:
        raise NotExist("Document not found or not accessible!")
    return json_response(docs[0], date_conversion=True)
