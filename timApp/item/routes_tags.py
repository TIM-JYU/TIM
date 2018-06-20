"""
Routes related to tags.
"""

from flask import Blueprint
from flask import abort
from flask import request
from sqlalchemy.exc import IntegrityError
from sqlalchemy.orm import joinedload
from sqlalchemy.orm.exc import UnmappedInstanceError, FlushError

from timApp.auth.accesshelper import verify_view_access, verify_manage_access
from timApp.auth.sessioninfo import get_current_user_object
from timApp.document.docentry import DocEntry, get_documents
from timApp.item.block import Block
from timApp.item.tag import Tag, TagType
from timApp.timdb.sqa import db
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

    d = DocEntry.find_by_path(doc, try_translation=True)
    if not d:
        abort(404)
    verify_manage_access(d)

    tag_dict_list, = verify_json_params('tags')

    tags = []
    for tag_dict in tag_dict_list:
        tag_type = TagType(int(tag_dict["type"]))
        check_tag_access(tag_type, "teachers")
        tag_name = tag_dict["name"]
        if has_tag_special_chars(tag_name):
            abort(400, "Tags can only contain letters a-z, numbers, underscores, spaces and dashes.")
        tag_expire = tag_dict.get("expires")
        tags.append(Tag(name=tag_name, expires=tag_expire, type=tag_type))

    if not tags:
        abort(400, "Tags not found.")

    for tag in tags:
        d.block.tags.append(tag)
    try:
        db.session.commit()
    except (IntegrityError, FlushError):
        abort(400, "Tag name is already in use.")
    return ok_response()


def check_tag_access(tag_type, group):
    """
    Checks whether the user is allowed to make changes to the tag type.
    :param tag_type:
    :return:
    """
    if tag_type != TagType.Regular:
        ug = UserGroup.get_by_name(group)
        if ug in get_current_user_object().groups:
            abort(400, f"Managing this tag requires {group} rights.")


def has_tag_special_chars(item_path: str):
    return set(item_path.lower()) - set('abcdefghijklmnopqrstuvwxyzåäö0123456789/- _')


@tags_blueprint.route('/remove/<path:doc>', methods=["POST"])
def remove_tag(doc):
    """
    Removes a tag-document entry from the database.
    :param doc The target document.
    :returns Removal success response.
    """
    d = DocEntry.find_by_path(doc, try_translation=True)
    if not d:
        abort(404)
    verify_manage_access(d)

    tag_dict, = verify_json_params('tagObject')

    tag_type = TagType(int(tag_dict["type"]))
    check_tag_access(tag_type, "teachers")

    tag_name = tag_dict["name"]
    tag_block_id = tag_dict["block_id"]
    tag_obj = Tag.query.filter_by(block_id=tag_block_id, name=tag_name, type=tag_type).first()

    if not tag_obj:
        abort(400, "Tag not found.")
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
    d = DocEntry.find_by_path(doc, try_translation=True)
    if not d:
        abort(404)
    verify_view_access(d)
    tags = d.block.tags
    return json_response(tags)


@tags_blueprint.route('/getAllTags', methods=["GET"])
def get_all_tags():
    """
    Gets the list of all unique tags used in any document.
    :returns The list of all unique tag names.
    """
    tags = Tag.query.all()

    # TODO: Check if necessary to add subject-type tags here.
    tags_unique = set()
    for tag in tags:
        tags_unique.add(tag.name)
    return json_response(list(tags_unique))


@tags_blueprint.route('/getDocs')
def get_tagged_documents():
    """
    Gets a list of Tag-entries that have a certain tag.
    Options:
    - Search exact or partial words.
    - Get all other tags in the document as well or don't.
    """
    tag_name = request.args.get('name', '')
    exact_search = get_option(request, 'exact_search', default=False, cast=bool)
    list_doc_tags = get_option(request, 'list_doc_tags', default=False, cast=bool)

    if exact_search:
        custom_filter = DocEntry.id.in_(Tag.query.filter_by(name=tag_name).with_entities(Tag.block_id))
    else:
        tag_name = f"%{tag_name}%"
        custom_filter = DocEntry.id.in_(Tag.query.filter(Tag.name.like(tag_name)).with_entities(Tag.block_id))
    if list_doc_tags:
        query_options = joinedload(DocEntry._block).joinedload(Block.tags)
    else:
        query_options = None

    docs = get_documents(filter_user=get_current_user_object(), custom_filter=custom_filter,
                         query_options=query_options)
    return json_response(docs)
