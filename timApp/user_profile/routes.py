from flask import Blueprint, Response
from timApp.auth.sessioninfo import get_current_user_object
from timApp.user.user import User
from timApp.document.docentry import DocEntry, get_documents
from timApp.util.flask.responsehelper import json_response
from dataclasses import dataclass
from flask import request
from timApp.upload.upload import upload_file
from timApp.upload.uploadedfile import (
    UploadedFile,
)

profile_blueprint = Blueprint("profile", __name__, url_prefix="/profile")


@dataclass
class ProfileInfo:
    username: str
    profile_path: str
    profile_picture_path: str

    def to_json(self) -> dict[str, str | None]:
        return {
            "username": self.username,
            "profile_path": self.profile_path,
            "profile_picture_path": self.profile_picture_path,
        }


PROFILE_PICTURE_KEY = "profile_picture_path"


@profile_blueprint.get("/<int:userid>")
def get_data_from_profile_document(userid: int) -> Response:
    """
    Provide user profile details according requested user.
    :param userid: ID of the user
    :return: JSON data, containing user profile details
    """
    requested_user = User.get_by_id(userid) if userid else get_current_user_object()
    username = requested_user.name
    personal_folder = requested_user.get_personal_folder()
    profile_path = f"/view/{personal_folder.path}/profile"

    # Find a profile picture url from document settings
    profile_settings = personal_folder.get_document("profile").document.get_settings()
    profile_picture_path = profile_settings.get(PROFILE_PICTURE_KEY)

    profile_data: ProfileInfo = ProfileInfo(
        username, profile_path, profile_picture_path
    )

    return json_response(profile_data.to_json())


@profile_blueprint.post("/picture/<int:document_id>/")
def upload_profile_picture(document_id: int) -> Response:
    file_to_upload = request.files.get("file")
    upload_response = upload_file(document_id, file_to_upload)
    image_url_suffix = upload_response.json["image"]
    document_entry = DocEntry.find_by_id(document_id)
    document_entry.document.add_setting(
        PROFILE_PICTURE_KEY, f"/images/{image_url_suffix}"
    )

    return upload_response
