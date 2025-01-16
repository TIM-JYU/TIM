from dataclasses import dataclass

from flask import Response
from flask import request

from timApp.auth.accesshelper import (
    AccessDenied,
    verify_edit_access,
)
from timApp.auth.sessioninfo import get_current_user_object, logged_in
from timApp.document.docentry import DocEntry
from timApp.folder.folder import Folder
from timApp.timdb.sqa import db
from timApp.upload.upload import upload_image_or_file
from timApp.user.user import User
from timApp.util.flask.requesthelper import RouteException, NotExist
from timApp.util.flask.responsehelper import json_response, ok_response
from timApp.util.flask.typedblueprint import TypedBlueprint

profile_blueprint = TypedBlueprint("profile", __name__, url_prefix="/profile")

PROFILE_PICTURE_KEY = "profile_picture_path"
PROFILE_DESCRIPTION_KEY = "profile_description"
PROFILE_LINKS_KEY = "profile_links"
COURSE_GROUP_KEY = "course_group_name"


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
    course_group_name: str

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
            "course_group_name": self.course_group_name,
        }


@profile_blueprint.get("/<int:userid>/<string:mode>")
def get_data_from_profile_document(
    userid: int | None = None, mode: str = "SHOW"
) -> Response:
    """
    Provide user profile details according requested user.
    :param userid: ID of the user
    :return: JSON data, containing user profile details
    """
    # When viewing somebody else's profile
    requested_user: User | None = get_current_user_object()

    # If not a specific user's profile requested, show user's own profile
    if userid > 0:
        requested_user = User.get_by_id(userid)

    if requested_user is None:
        raise RouteException("No user with given ID was found.")

    # Form the response JSON here
    profile_data = prepare_profile_data(requested_user, mode)

    return json_response(profile_data.to_json())


def prepare_profile_data(user: User, mode: str) -> ProfileDataModel:
    """
    Build a profile data from user's personal profile document
    :param user: either requested or current user
    :return: profile data instance
    """
    # Get a personal folder from user's documents, and get the profile document
    personal_folder: Folder = user.get_personal_folder()
    document_object = personal_folder.get_document("profile")

    document_no_exist: bool = document_object is None
    show_mode_on: bool = mode == "SHOW"

    # Do not auto create document, if one is not trying to edit one's own profile
    if document_no_exist and show_mode_on:
        raise NotExist("No profile document was found.")

    # Access profile settings to get profile data
    profile_settings = document_object.document.get_settings()

    profile_data: ProfileDataModel = ProfileDataModel(
        user.name,
        user.real_name,
        user.email,
        profile_path=f"/view/{personal_folder.path}/profile",
        profile_picture_path=profile_settings.get(PROFILE_PICTURE_KEY, default=""),
        profile_description=profile_settings.get(PROFILE_DESCRIPTION_KEY, default=""),
        course_group_name=profile_settings.get(COURSE_GROUP_KEY, default=""),
        # As default, return at least one item as profile link
        profile_links=profile_settings.get(PROFILE_LINKS_KEY, default=[""]),
        # Provide information for to show edit profile button or not
        # If requester is owner of requested profile
        edit_access=user.id == get_current_user_object().id,
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


@profile_blueprint.post("create")
def create_profile_on_fetch() -> Response:
    user: User = get_current_user_object()
    personal_folder: Folder = user.get_personal_folder()

    create_profile(user, personal_folder)
    return json_response({})


def create_profile(user: User, personal_folder: Folder) -> Response:
    """
    Create a new profile
    :return:
    """

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
            text=f"<tim-user-profile view-mode=true user-id={user.id} document-id=%%docid%% ></tim-user-profile>",
            attrs={"allowangular": "true"},
        )
        db.session.commit()
    except AccessDenied as e:
        print(e)


@profile_blueprint.post("/details/<int:document_id>")
def set_profile_details(
    document_id: int,
    profile_links: list[str],
    profile_description: str,
    course_group_name: str,
) -> Response:
    links: list[str] = profile_links
    description: str = profile_description
    course_group: str = course_group_name
    document_info = DocEntry.find_by_id(document_id)

    if document_info is None:
        raise RouteException("No profile-document found.")
    verify_edit_access(document_info)

    document_info.document.add_setting(PROFILE_DESCRIPTION_KEY, description)
    document_info.document.add_setting(PROFILE_LINKS_KEY, links)
    document_info.document.add_setting(COURSE_GROUP_KEY, course_group)

    return ok_response()
