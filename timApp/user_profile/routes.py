import json

from flask import Blueprint, Response

from timApp.auth.accesshelper import (
    AccessDenied,
    verify_edit_access,
    verify_view_access,
)
from timApp.auth.sessioninfo import get_current_user_object, logged_in
from timApp.timdb.sqa import db
from timApp.user.user import User
from timApp.document.docentry import DocEntry, get_documents
from timApp.util.flask.requesthelper import RouteException, NotExist
from timApp.util.flask.responsehelper import json_response
from dataclasses import dataclass
from flask import request
from timApp.upload.upload import upload_file, upload_image_or_file
from timApp.upload.uploadedfile import (
    UploadedFile,
)
from timApp.auth.accesstype import AccessType

profile_blueprint = Blueprint("profile", __name__, url_prefix="/profile")


@dataclass
class ProfileData:
    username: str
    realname: str | None
    email: str
    profile_path: str
    profile_picture_path: str
    profile_description: str
    profile_links: list[str]
    edit_access: bool

    def to_json(self) -> dict[str, str | list[str] | bool | None]:
        return {
            "username": self.username,
            "realname": self.realname,
            "email": self.email,
            "profile_path": self.profile_path,
            "profile_picture_path": self.profile_picture_path,
            "profile_description": self.profile_description,
            "profile_links": self.profile_links,
            "edit_access": self.edit_access,
        }


PROFILE_PICTURE_KEY = "profile_picture_path"
PROFILE_DESCRIPTION_KEY = "profile_description"
PROFILE_LINKS_KEY = "profile_links"


@profile_blueprint.get("/<int:userid>")
@profile_blueprint.get("")
def get_data_from_profile_document(userid: int | None = None) -> Response:
    """
    Provide user profile details according requested user.
    :param userid: ID of the user
    :return: JSON data, containing user profile details
    """

    requested_user: User | None = get_current_user_object()
    edit_access = True

    if userid is not None:
        requested_user = User.get_by_id(userid)
        edit_access = False

    if requested_user is None:
        raise RouteException("No user with given ID was found.")

    profile_data = prepare_profile_data(requested_user, edit_access)

    return json_response(profile_data.to_json())


def prepare_profile_data(user: User, edit_access: bool) -> ProfileData:
    username = user.name
    realname = user.real_name
    email = user.email
    personal_folder = user.get_personal_folder()

    profile_path = f"/view/{personal_folder.path}/profile"

    # Find a profile picture url from document settings
    document_object = personal_folder.get_document("profile")

    if document_object is None:
        try:
            verify_edit_access(personal_folder)
            document_object = personal_folder.get_document(
                "profile", create_if_not_exist=True
            )
            db.session.commit()
        except AccessDenied as e:
            print(e)

    if document_object is None:
        raise NotExist("No profile document was found.")

    # When document is available, verify view privileges
    if not edit_access:
        verify_view_access(personal_folder)

    profile_settings = document_object.document.get_settings()
    profile_picture_path = profile_settings.get(PROFILE_PICTURE_KEY, default="")
    profile_description = profile_settings.get(PROFILE_DESCRIPTION_KEY, default="")

    # As default, return at leas one item as profile link
    profile_links = profile_settings.get(PROFILE_LINKS_KEY, default=[""])

    profile_data: ProfileData = ProfileData(
        username,
        realname,
        email,
        profile_path=profile_path,
        profile_picture_path=profile_picture_path,
        profile_description=profile_description,
        profile_links=profile_links,
        edit_access=edit_access,
    )
    return profile_data


@profile_blueprint.post("/picture/<int:document_id>/")
def upload_profile_picture(document_id: int) -> Response:
    file_to_upload = request.files.get("file")

    if not logged_in():
        raise AccessDenied("You have to be logged in to upload a picture.")

    document_info = DocEntry.find_by_id(int(document_id))
    if document_info is None:
        raise RouteException("No profile-document found.")
    verify_edit_access(document_info)

    upload_response = upload_image_or_file(document_info, file_to_upload)
    image_url_suffix = upload_response.json["image"]

    document_info.document.add_setting(
        PROFILE_PICTURE_KEY, f"/images/{image_url_suffix}"
    )

    return upload_response


@profile_blueprint.post("/details/<int:document_id>")
def set_profile_details(document_id: int) -> Response:
    data: ProfileData = ProfileData(**request.get_json())
    links: list[str] = data.profile_links
    description: str = data.profile_description
    document_info = DocEntry.find_by_id(document_id)

    if document_info is None:
        raise RouteException("No profile-document found.")
    verify_edit_access(document_info)

    document_info.document.add_setting(PROFILE_DESCRIPTION_KEY, description)
    document_info.document.add_setting(PROFILE_LINKS_KEY, links)

    return Response(status=200)
