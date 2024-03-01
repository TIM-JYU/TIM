import datetime
from authlib.integrations.flask_oauth2 import current_token
from authlib.oauth2 import OAuth2Error
from authlib.oauth2.rfc6749 import BaseGrant, scope_to_list
from flask import Response, render_template, request


from timApp.auth.accesshelper import verify_logged_in
from timApp.auth.oauth2.models import OAuth2Client, Scope
from timApp.auth.oauth2.oauth2 import auth_server, require_oauth
from timApp.auth.sessioninfo import get_current_user_object, logged_in

from timApp.tide_support.routes import (
    user_ide_courses,
    ide_tasks_from_document,
    ide_task_by_id,
)
from timApp.tim_app import csrf
from timApp.user.user import User
from timApp.util.flask.responsehelper import json_response, safe_redirect
from timApp.util.flask.typedblueprint import TypedBlueprint

oauth = TypedBlueprint("oauth", __name__, url_prefix="/oauth")


@oauth.get("authorize")
def request_authorization() -> str | Response:
    user = get_current_user_object() if logged_in() else None
    try:
        grant: BaseGrant = auth_server.get_consent_grant(end_user=user)
    except OAuth2Error:
        return safe_redirect("/")
    client: OAuth2Client = grant.client
    scope = client.get_allowed_scope(grant.request.scope)
    return render_template(
        "oauth_authorize.jinja2", client=client, scopes=scope_to_list(scope)
    )


@oauth.post("authorize")
def authorize(confirm: bool) -> Response:
    verify_logged_in()
    return auth_server.create_authorization_response(
        grant_user=get_current_user_object() if confirm else None
    )


@oauth.post("token")
@csrf.exempt
def issue_token() -> Response:
    return auth_server.create_token_response()


@oauth.get("token-validity")
@require_oauth(Scope.profile.value)
def token_is_valid() -> Response:
    """
    Check if the token is valid
    :return: JSON response with the token status and validity time (hours:minutes:seconds)
    """

    # Convert seconds to hours:minutes:seconds
    convert = str(datetime.timedelta(seconds=current_token.expires_in))

    return json_response({"validityTime": convert})


@oauth.get("profile")
@require_oauth(Scope.profile.name)
def get_user_profile() -> Response:
    user: User = current_token.user
    # Implement profile output here to make sure not too much information is given out for this scope
    return json_response(
        {
            "id": user.id,
            "emails": [
                {  # First email is considered primary
                    "email": user.email,
                    "verified": True,
                }
            ],
            "last_name": user.last_name,
            "given_name": user.given_name,
            "real_name": user.real_name,
            "username": user.name,
        }
    )


@oauth.get("bookmarked-courses")
@require_oauth(Scope.user_courses.value)
def get_courses() -> Response:
    """
    Gets all courses that the user has bookmarked
    :return: JSON response with all courses
    """
    user: User = current_token.user
    courses = user.bookmarks.bookmark_data[2]["My courses"]

    res = [
        {"course_name": list(course.keys())[0], "path": list(course.values())[0]}
        for course in courses
    ]

    return json_response(res)


@oauth.get("bookmarked-ide-courses")
@require_oauth(Scope.user_tasks.value)
def get_all_ide_courses() -> Response:
    """
    Get all courses that the user has bookmarked and have ide task/s
    :return: JSON response with all tasks
    """
    user: User = current_token.user

    tasks = user_ide_courses(user=user, attr_name="ideTask")

    return json_response(tasks)


@oauth.get("ide-tasks-by-document-id/<int:document_id>")
@require_oauth(Scope.user_tasks.value)
def get_ide_tasks_by_doc_id(document_id: int) -> Response:
    """
    Get all ide-tasks by document id
    :return: JSON response with the task
    """
    user: User = current_token.user

    return json_response(
        ide_tasks_from_document(user=user, document_id=document_id, attr_name="ideTask")
    )


@oauth.get("ide-task-by-id/<int:doc_id>/<string:ide_task_id>")
@require_oauth(Scope.user_tasks.value)
def get_ide_task_by_id(doc_id: int, ide_task_id: str) -> Response:
    """
    Get a specific task by doc id and task id
    :return: JSON response with the task
    """
    user: User = current_token.user

    return json_response(
        ide_task_by_id(
            user=user, ide_task_id=ide_task_id, doc_id=doc_id, attr_name="ideTask"
        )
    )
