"""
Routes related to tags.
"""

from flask import Blueprint
from flask import abort
from flask import request
from sqlalchemy.exc import IntegrityError
from sqlalchemy.orm import joinedload, raiseload
from sqlalchemy.orm.exc import UnmappedInstanceError, FlushError

from timApp.auth.accesshelper import verify_edit_access, verify_view_access
from timApp.auth.sessioninfo import get_current_user_object
from timApp.document.docentry import DocEntry, get_documents
from timApp.item.block import Block
from timApp.item.tag import Tag
from timApp.item.validation import has_special_chars
from timApp.timdb.sqa import db
from timApp.util.flask.requesthelper import verify_json_params, get_option
from timApp.util.flask.responsehelper import ok_response, json_response

tags_blueprint = Blueprint('tags',
                           __name__,
                           url_prefix='/tags')

# Tags restricted to certain groups:
special_tags = ["kurssi", "projekti", "gradu"]


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
    verify_edit_access(d)
    tags, = verify_json_params('tags')
    expires, = verify_json_params('expires', require=False)
    for tag in tags:
        check_special_tag_rights(tag)
        if has_special_chars(tag):
            abort(400, "Tags can only contain letters a-z, numbers, underscores and dashes.")
        d.block.tags.append(Tag(tag=tag, expires=expires))
    try:
        db.session.commit()
    except (IntegrityError, FlushError):
        abort(400, "Tag already exists for this document.")
    return ok_response()


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
    verify_edit_access(d)
    tag_dict, = verify_json_params('tagObject')
    tag_name = tag_dict["tag"]
    tag_block_id = tag_dict["block_id"]
    check_special_tag_rights(tag_name)
    tag_obj = Tag.query.filter_by(block_id=tag_block_id, tag=tag_name).first()
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
    tags_unique = set(special_tags)
    for tag in tags:
        tags_unique.add(tag.tag)

    return json_response(list(tags_unique))


@tags_blueprint.route('/getDocs')
def get_tagged_documents():
    """
    Gets a list of Tag-entries that have a certain tag.
    Options:
    - Search exact or partial words.
    - Get all other tags in the document as well or don't.
    """
    tag_name = request.args.get('tag', '')
    exact_search = get_option(request, 'exact_search', default=False, cast=bool)
    list_doc_tags = get_option(request, 'list_doc_tags', default=False, cast=bool)

    if exact_search:
        custom_filter = DocEntry.id.in_(Tag.query.filter_by(tag=tag_name).with_entities(Tag.block_id))
    else:
        tag_name = f"%{tag_name}%"
        custom_filter = DocEntry.id.in_(Tag.query.filter(Tag.tag.like(tag_name)).with_entities(Tag.block_id))
    if list_doc_tags:
        query_options = joinedload(DocEntry._block).joinedload(Block.tags)
    else:
        query_options = None
    docs = get_documents(filter_user=get_current_user_object(), custom_filter=custom_filter,
                         query_options=query_options)

    return json_response(docs)


def check_special_tag_rights(tag: str):
    if tag in special_tags:
        # TODO: Check if user belongs to a group allowed to use the tag.
        # abort(403, f"Editing tag '{tag}' requires additional rights.")
        pass
