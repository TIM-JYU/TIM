from flask import Blueprint, Response
from timApp.auth.sessioninfo import get_current_user_object, logged_in
from timApp.timdb.sqa import db
from timApp.user.user import User
from timApp.document.docentry import DocEntry
from timApp.util.flask.requesthelper import RouteException, NotExist, load_data_from_req
from timApp.util.flask.responsehelper import json_response
from dataclasses import dataclass
from flask import request
from timApp.upload.upload import upload_image_or_file
from tim_common.marshmallow_dataclass import class_schema
from timApp.auth.accesshelper import (
    AccessDenied,
    verify_edit_access,
)

profile_blueprint = Blueprint("profile", __name__, url_prefix="/profile")

PROFILE_PICTURE_KEY = "profile_picture_path"
PROFILE_DESCRIPTION_KEY = "profile_description"
PROFILE_LINKS_KEY = "profile_links"


@dataclass
class ProfileDataModel:
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


ProfileDataModelSchema = class_schema(ProfileDataModel)


@profile_blueprint.get("/<int:userid>")
@profile_blueprint.get("")
def get_data_from_profile_document(userid: int | None = None) -> Response:
    """
    Provide user profile details according requested user.
    :param userid: ID of the user
    :return: JSON data, containing user profile details
    """
    # When viewing somebody else's profile
    requested_user: User | None = get_current_user_object()

    # If not a specific user's profile requested, show user's own profile
    if userid is not None:
        requested_user = User.get_by_id(userid)

    if requested_user is None:
        raise RouteException("No user with given ID was found.")

    # Form the response JSON here
    profile_data = prepare_profile_data(requested_user)

    return json_response(profile_data.to_json())


def prepare_profile_data(user: User) -> ProfileDataModel:
    """
    Build a profile data from user's personal profile document
    :param user: either requested or current user
    :return: profile data instance
    """
    username = user.name
    realname = user.real_name
    email = user.email

    # Get a personal folder from user's documents, and get the profile document
    personal_folder = user.get_personal_folder()
    profile_path = f"/view/{personal_folder.path}/profile"
    document_object = personal_folder.get_document("profile")

    if document_object is None:
        try:
            verify_edit_access(personal_folder)
            document_object = personal_folder.get_document(
                "profile",
                create_if_not_exist=True,
                creator_group=user.get_personal_group(),
            )

            if document_object is None:
                raise NotExist("Document 'profile' not found.")

            document_object.document.add_paragraph(
                text=f"<tim-user-profile modify-enabled=true user-id={user.id} document-id=%%docid%% ></tim-user-profile>",
                attrs={"allowangular": "true"},
            )
            db.session.commit()
        except AccessDenied as e:
            print(e)

    if document_object is None:
        raise NotExist("No profile document was found.")

    profile_settings = document_object.document.get_settings()
    profile_picture_path = profile_settings.get(PROFILE_PICTURE_KEY, default="")
    profile_description = profile_settings.get(PROFILE_DESCRIPTION_KEY, default="")

    # As default, return at least one item as profile link
    profile_links = profile_settings.get(PROFILE_LINKS_KEY, default=[""])

    # Provide information for to show edit profile button or not
    # If requester is owner of requested profile
    edit_access = user.id == get_current_user_object().id

    profile_data: ProfileDataModel = ProfileDataModel(
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
    data: ProfileDataModel = load_data_from_req(ProfileDataModelSchema)
    links: list[str] = data.profile_links
    description: str = data.profile_description
    document_info = DocEntry.find_by_id(document_id)

    if document_info is None:
        raise RouteException("No profile-document found.")
    verify_edit_access(document_info)

    document_info.document.add_setting(PROFILE_DESCRIPTION_KEY, description)
    document_info.document.add_setting(PROFILE_LINKS_KEY, links)

    return Response(status=200)
