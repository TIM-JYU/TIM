from flask import Blueprint, current_app, Response
from sqlalchemy.orm import selectinload

from timApp.auth.sessioninfo import get_current_user_object
from timApp.document.docentry import DocEntry, get_documents
from timApp.item.block import Block
from timApp.util.flask.responsehelper import json_response

course_blueprint = Blueprint("profile", __name__, url_prefix="/profile")


@course_blueprint.get()
def get_data_from_profile_document() -> Response:
    current_user = get_current_user_object()
    profile_picture = current_user.get_profile()
    return json_response({"test": "Hello World!"})
