"""
Routes related to tags.
"""
from flask import Blueprint
from flask import abort
from sqlalchemy.exc import IntegrityError
from sqlalchemy.orm.exc import UnmappedInstanceError

from timApp.accesshelper import verify_edit_access, verify_view_access
from timApp.requesthelper import verify_json_params
from timApp.responsehelper import ok_response, json_response
from timApp.timdb.models.docentry import DocEntry
from timApp.timdb.models.tag import Tag
from timApp.timdb.tim_models import db

tags_blueprint = Blueprint('tags',
                           __name__,
                           url_prefix='/tags')

# Tags restricted to certain groups:
special_tags = ["kurssi", "projekti", "gradu"]


@tags_blueprint.route('/add/<path:doc>', methods=["POST"])
def add_tag(doc):
    """
    Adds a tag-document entry into the database.
    """
    d = DocEntry.find_by_path(doc, try_translation=True)
    if not d:
        abort(404)
    verify_edit_access(d)
    tag, = verify_json_params('tag')
    expires, = verify_json_params('expires', require=False)
    check_special_tag_rights(tag)

    d.block.tags.append(Tag(tag=tag, expires=expires))
    try:
        db.session.commit()
    except IntegrityError:
        abort(400, "Tag already exists for this document.")
    return ok_response()


@tags_blueprint.route('/remove/<path:doc>', methods=["POST"])
def remove_tag(doc):
    """
    Removes a tag-document entry from the database.
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
    Gets the list of document's tags.
    """
    d = DocEntry.find_by_path(doc, try_translation=True)
    if not d:
        abort(404)
    verify_view_access(d)
    tags = Tag.query.filter_by(block_id=d.id).all()
    return json_response(tags)


def check_special_tag_rights(tag: str):
    if tag in special_tags:
        # TODO: Check if use belongs to a group allowed to use the tag.
        # abort(403, f"Editing tag '{tag}' requires additional rights.")
        pass