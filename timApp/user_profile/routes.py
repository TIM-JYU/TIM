from flask import Blueprint, Response

from timApp.auth.accesshelper import AccessDenied, verify_edit_access
from timApp.auth.sessioninfo import get_current_user_object, logged_in
from timApp.user.user import User
from timApp.document.docentry import DocEntry, get_documents
from timApp.util.flask.responsehelper import json_response
from dataclasses import dataclass
from flask import request
from timApp.upload.upload import upload_file, upload_image_or_file
from timApp.upload.uploadedfile import (
    UploadedFile,
)

profile_blueprint = Blueprint("profile", __name__, url_prefix="/profile")


@dataclass
class ProfileInfo:
    username: str
    realname: str
    email: str
    profile_path: str
    profile_picture_path: str
    profile_description: str
    profile_links: list[str]

    def to_json(self) -> dict[str, str | None]:
        return {
            "username": self.username,
            "realname": self.realname,
            "email": self.email,
            "profile_path": self.profile_path,
            "profile_picture_path": self.profile_picture_path,
            "profile_description": self.profile_description,
            "profile_links": self.profile_links,
        }


PROFILE_PICTURE_KEY = "profile_picture_path"
PROFILE_DESCRIPTION_KEY = "profile_description"
PROFILE_LINKS_KEY = "profile_links"


@profile_blueprint.get("/<int:userid>")
def get_data_from_profile_document(userid: int) -> Response:
    """
    Provide user profile details according requested user.
    :param userid: ID of the user
    :return: JSON data, containing user profile details
    """
    requested_user = User.get_by_id(userid) if userid else get_current_user_object()
    username = requested_user.name
    realname = requested_user.real_name
    email = requested_user.email
    personal_folder = requested_user.get_personal_folder()
    profile_path = f"/view/{personal_folder.path}/profile"

    # Find a profile picture url from document settings
    profile_settings = personal_folder.get_document("profile").document.get_settings()
    profile_picture_path = profile_settings.get(PROFILE_PICTURE_KEY, default="")
    profile_description = profile_settings.get(PROFILE_DESCRIPTION_KEY, default="")

    # As default, return at leas one item as profile link
    profile_links = profile_settings.get(PROFILE_LINKS_KEY, default=[""])

    profile_data: ProfileInfo = ProfileInfo(
        username,
        realname,
        email,
        profile_path=profile_path,
        profile_picture_path=profile_picture_path,
        profile_description=profile_description,
        profile_links=profile_links,
    )

    return json_response(profile_data.to_json())


@profile_blueprint.post("/picture/<int:document_id>/")
def upload_profile_picture(document_id: int) -> Response:
    file_to_upload = request.files.get("file")
    document_info = DocEntry.find_by_id(document_id)

    # Verify log in and edit access
    if not logged_in():
        raise AccessDenied("You have to be logged in to upload a picture.")
    verify_edit_access(document_info)

    upload_response = upload_image_or_file(document_info, file_to_upload)
    image_url_suffix = upload_response.json["image"]
    # Palauta upload reitti ennalleen
    # Käytä upload_image_or_file(d: DocInfo, file):
    document_info.document.add_setting(
        PROFILE_PICTURE_KEY, f"/images/{image_url_suffix}"
    )

    return upload_response


@profile_blueprint.post("/details")
def set_profile_details() -> Response:
    data = request.json
    links: list[str] = data.get("profile_links")
    description: str = data.get("profile_description")
    document_id: int = data.get("document_id")
    document_entry = DocEntry.find_by_id(document_id)
    document_entry.document.add_setting(PROFILE_DESCRIPTION_KEY, description)
    document_entry.document.add_setting(PROFILE_LINKS_KEY, links)

    return Response(status=200)
