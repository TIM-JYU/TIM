"""
Routes related to tags.
"""
from flask import Blueprint
from flask import abort
from sqlalchemy.exc import IntegrityError

from timApp.accesshelper import verify_edit_access
from timApp.requesthelper import verify_json_params
from timApp.responsehelper import ok_response
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
    if tag in special_tags:
        # TODO: Check if use belongs to a group allowed to use the tag.
        # abort(403, f"Managing tag '{tag}' requires additional rights.")
        pass

    d.block.tags.append(Tag(tag=tag, expires=expires))
    try:
        db.session.commit()
    except IntegrityError:
        abort(400, "Tag already exists for this document.")
    return ok_response()
