"""
Routes related to tags.
"""
from datetime import datetime
from typing import List

from flask import Blueprint
from flask import abort
from flask import request
from sqlalchemy import func
from sqlalchemy.exc import IntegrityError
from sqlalchemy.orm import joinedload
from sqlalchemy.orm.exc import UnmappedInstanceError, FlushError

from timApp.auth.accesshelper import verify_view_access, verify_manage_access, check_admin_access
from timApp.auth.sessioninfo import get_current_user_object
from timApp.document.docentry import DocEntry, get_documents
from timApp.document.docinfo import DocInfo
from timApp.item.block import Block
from timApp.item.tag import Tag, TagType, GROUP_TAG_PREFIX
from timApp.timdb.sqa import db
from timApp.user.groups import verify_group_view_access
from timApp.user.special_group_names import TEACHERS_GROUPNAME
from timApp.user.usergroup import UserGroup
from timApp.util.flask.requesthelper import verify_json_params, get_option
from timApp.util.flask.responsehelper import ok_response, json_response

tags_blueprint = Blueprint('tags',
                           __name__,
                           url_prefix='/tags')


@tags_blueprint.route('/add/<path:doc>', methods=["POST"])
def add_tag(doc):
    """
    Adds a tag-document entry into the database.
    :param doc The target document.
    :returns Tag adding success response.
    """

    d = DocEntry.find_by_path(doc)
    if not d:
        abort(404)
    verify_manage_access(d)
    add_tags_from_request(d)
    return commit_and_ok()


def commit_and_ok():
    try:
        db.session.commit()
    except (IntegrityError, FlushError):
        db.session.rollback()
        abort(400, "Tag name is already in use.")
    return ok_response()


def add_tags_from_request(d: DocInfo):
    tag_dict_list, = verify_json_params('tags')
    tags = []
    for tag_dict in tag_dict_list:
        tag_type = TagType(int(tag_dict["type"]))
        tag_name = tag_dict["name"]
        tag_expire = tag_dict.get("expires")
        tag = Tag(name=tag_name, expires=tag_expire, type=tag_type)
        check_tag_access(tag)
        tags.append(tag)
    for tag in tags:
        d.block.tags.append(tag)


def check_tag_access(tag: Tag, check_group=True):
    """
    Checks whether the user is allowed to make changes to the tag type.
    If not allowed, gives abort response.
    :param check_group: Whether to check group tag right. Should be false when deleting a tag.
    :param tag: The tag to check.
    """
    if tag.type != TagType.Regular:
        ug = UserGroup.get_by_name(TEACHERS_GROUPNAME)
        if ug not in get_current_user_object().groups and not check_admin_access():
            abort(400, f"Managing this tag requires admin or {TEACHERS_GROUPNAME} rights.")
    groupname = tag.get_group_name()
    if groupname and check_group:
        ug = UserGroup.get_by_name(groupname)
        if not ug:
            abort(404, f'Usergroup "{groupname}" not found.')
        verify_group_view_access(ug)


@tags_blueprint.route('/setCourseTags/<path:doc>', methods=["POST"])
def set_group_tags(doc):
    d = DocEntry.find_by_path(doc)
    if not d:
        abort(404)
    verify_manage_access(d)
    tags: List[Tag] = d.block.tags
    tags_to_remove = [t for t in tags
                      if t.get_group_name() or t.type in (TagType.CourseCode, TagType.Subject)]
    for t in tags_to_remove:
        check_tag_access(t, check_group=False)
        db.session.delete(t)

    add_tags_from_request(d)

    new_groups, = verify_json_params('groups')
    for g in new_groups:
        t = Tag(name=GROUP_TAG_PREFIX + g, type=TagType.Regular)
        check_tag_access(t)
        d.block.tags.append(t)
    return commit_and_ok()


@tags_blueprint.route('/edit/<path:doc>', methods=["POST"])
def edit_tag(doc):
    """
    Replaces a tag-document entry in the database with new one.
    :param doc The target document.
    :returns Edit success response.
    """
    d = DocEntry.find_by_path(doc)
    if not d:
        abort(404)
    verify_manage_access(d)

    new_tag_dict, = verify_json_params('newTag')
    old_tag_dict, = verify_json_params('oldTag')

    new_tag_type = TagType(int(new_tag_dict["type"]))
    old_tag_type = TagType(int(old_tag_dict["type"]))

    new_tag_name = new_tag_dict["name"]
    old_tag_name = old_tag_dict["name"]
    new_tag_expire = new_tag_dict.get("expires")

    # If commas in the name, use first part.
    new_tag_name = new_tag_name.split(",", 1)[0]

    new_tag = Tag(name=new_tag_name, expires=new_tag_expire, type=new_tag_type)
    old_tag = Tag.query.filter_by(block_id=d.id, name=old_tag_name, type=old_tag_type).first()

    if not old_tag:
        abort(400, "Tag to edit not found.")
    check_tag_access(old_tag)
    check_tag_access(new_tag)
    try:
        db.session.delete(old_tag)
        d.block.tags.append(new_tag)
        db.session.commit()
    except (IntegrityError, FlushError):
        abort(400, "Tag editing failed! New tag name may already be in use")
    return ok_response()


@tags_blueprint.route('/remove/<path:doc>', methods=["POST"])
def remove_tag(doc):
    """
    Removes a tag-document entry from the database.
    :param doc The target document.
    :returns Removal success response.
    """
    d = DocEntry.find_by_path(doc)
    if not d:
        abort(404)
    verify_manage_access(d)

    tag_dict, = verify_json_params('tagObject')

    tag_type = TagType(int(tag_dict["type"]))
    tag_name = tag_dict["name"]
    tag_obj = Tag.query.filter_by(block_id=d.id, name=tag_name, type=tag_type).first()

    if not tag_obj:
        abort(400, "Tag not found.")
    check_tag_access(tag_obj)
    try:
        db.session.delete(tag_obj)
        db.session.commit()
    except (IntegrityError, UnmappedInstanceError):
        abort(400, "Tag removal failed.")
    return ok_response()


@tags_blueprint.route('/getTags/<path:doc>', methods=["GET"])
def get_tags(doc):
    """
    Gets the list of a document's tags.
    :param doc The target document.
    :returns The list of document's Tag-objects converted into JSON.
    """
    d = DocEntry.find_by_path(doc)
    if not d:
        abort(404)
    verify_view_access(d)
    tags = d.block.tags
    return json_response(tags)


@tags_blueprint.route('/getAllTags', methods=["GET"])
def get_all_tags():
    """
    Gets the list of all unique tags used in any document, regardless
    of expiration.
    :returns The list of all unique tag names as list of strings.
    """
    tags = Tag.query.all()

    tags_unique = set()
    for tag in tags:
        tags_unique.add(tag.name)
    return json_response(sorted(list(tags_unique)))


@tags_blueprint.route('/getDocs')
def get_tagged_documents():
    """
    Gets a list of documents that have a certain tag.
    Options:
    - Search exact or partial words.
    - Case sensitivity
    - Get all other tags in the document as well or don't fetch them.
    :returns Response containing list of documents with the searched tag.
    """
    tag_name = request.args.get('name', '')
    exact_search = get_option(request, 'exact_search', default=False, cast=bool)
    list_doc_tags = get_option(request, 'list_doc_tags', default=False, cast=bool)
    case_sensitive = get_option(request, 'case_sensitive', default=False, cast=bool)

    if exact_search:
        if case_sensitive:
            custom_filter = DocEntry.id.in_(Tag.query.filter_by(name=tag_name).with_entities(Tag.block_id))
        else:
            custom_filter = DocEntry.id.in_(
                Tag.query.filter(func.lower(Tag.name) == func.lower(tag_name)).with_entities(Tag.block_id))

    else:
        tag_name = f"%{tag_name}%"
        if case_sensitive:
            custom_filter = DocEntry.id.in_(Tag.query.filter(Tag.name.like(tag_name) &
                                                             ((Tag.expires > datetime.now()) | (Tag.expires == None))).
                                            with_entities(Tag.block_id))
        else:
            custom_filter = DocEntry.id.in_(Tag.query.filter(Tag.name.ilike(tag_name) &
                                                             ((Tag.expires > datetime.now()) | (Tag.expires == None))).
                                            with_entities(Tag.block_id))

    if list_doc_tags:
        query_options = joinedload(DocEntry._block).joinedload(Block.tags)
    else:
        query_options = None

    docs = get_documents(filter_user=get_current_user_object(), custom_filter=custom_filter,
                         query_options=query_options)
    return json_response(docs)


@tags_blueprint.route("/getDoc/<int:doc_id>")
def get_tagged_document_by_id(doc_id):
    """
    Gets a document and its tags by id.
    :param doc_id: Searched document id.
    :return: A DocEntry with tags.
    """
    docs = get_documents(filter_user=get_current_user_object(),
                         custom_filter=DocEntry.id.in_([doc_id]),
                         query_options=joinedload(DocEntry._block).joinedload(Block.tags))
    if not docs:
        abort(404, "Document not found or not accessible!")
    return json_response(docs[0])
