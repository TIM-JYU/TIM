"""
Contains course related routes.
"""

from flask import Blueprint
from timApp.document.docentry import DocEntry
from timApp.util.flask.responsehelper import json_response

course_blueprint = Blueprint('course',
                             __name__,
                             url_prefix='/courses')


@course_blueprint.route("/settings")
def get_course_settings():
    """
    Get course settings from the designated settings document.
    :return: Settings from the course settings.
    """
    d = DocEntry.find_by_path("settings/courses")
    if not d:
        return json_response({})
    return json_response(d.document.get_settings().get_dict().values)