"""
Contains course related routes.
"""
from dataclasses import dataclass

from flask import Blueprint, current_app, Response, request
from sqlalchemy.orm import selectinload

from timApp.auth.sessioninfo import get_current_user_object
from timApp.document.docentry import DocEntry, get_documents
from timApp.folder.folder import Folder
from timApp.item.block import Block
from timApp.item.manage import do_copy_folder
from timApp.util.flask.requesthelper import load_data_from_req, NotExist
from timApp.util.flask.responsehelper import json_response
from tim_common.marshmallow_dataclass import class_schema

course_blueprint = Blueprint("course", __name__, url_prefix="/courses")


@course_blueprint.get("/settings")
def get_course_settings() -> Response:
    """
    Get course settings from the designated settings document.
    :return: Settings from the course settings.
    """
    d = DocEntry.find_by_path("settings/courses")
    if not d:
        return json_response({})
    return json_response(d.document.get_settings().get_dict().values)


@course_blueprint.get("/documents/<string:foldername>")
def get_documents_from_bookmark_folder(foldername: str) -> Response:
    """
    Gets documents and their tags based on a bookmark folder name.

    :param foldername:
    :return:
    """
    if not current_app.config["BOOKMARKS_ENABLED"]:
        return json_response([])
    folder = None
    paths = []
    bookmark_folders = get_current_user_object().bookmarks
    for bookmark_folder in bookmark_folders.as_dict():
        if bookmark_folder["name"] == foldername:
            folder = bookmark_folder
    if folder:
        for bookmark in folder["items"]:
            paths.append(bookmark["link"].replace("/view/", "", 1))
    else:
        return json_response([])
    docs = get_documents(
        filter_user=get_current_user_object(),
        custom_filter=DocEntry.name.in_(paths),
        query_options=selectinload(DocEntry._block).selectinload(Block.tags),
    )
    return json_response(docs)


@dataclass
class CourseTemplateInitModel:
    copy_to_dir_name: str
    # copy_from_id: str
    copy_from_path: str


CourseTemplateInitModelSchema = class_schema(CourseTemplateInitModel)


@course_blueprint.post("/from-template")
def create_course_from_template() -> Response:
    data: CourseTemplateInitModel = load_data_from_req(CourseTemplateInitModelSchema)
    copy_to_dir_name: str = data.copy_to_dir_name
    #  copy_from_id: str = data.copy_from_id
    copy_from_path: str = data.copy_from_path
    folder = Folder.find_by_path(copy_from_path)
    if not folder:
        raise NotExist("No template folder found.")

    # TODO: provide info, if document exist

    copied = do_copy_folder(
        folder_id=folder.id, destination=f"oscar/camps/{copy_to_dir_name}", exclude=None
    )

    return json_response({"url": copied[0].url})
