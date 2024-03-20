from authlib.integrations.flask_oauth2 import current_token
from flask import Response, request

from timApp.auth.oauth2.models import Scope
from timApp.auth.oauth2.oauth2 import require_oauth

from timApp.tide_support.utils import (
    user_ide_courses,
    demos_by_doc,
    ide_tasks,
    ide_task_by_id,
    submit_task,
)
from timApp.user.user import User
from timApp.util.flask.responsehelper import json_response
from timApp.util.flask.typedblueprint import TypedBlueprint

ide = TypedBlueprint("ide", __name__, url_prefix="/ide")


@ide.get("ideCourses")
@require_oauth(Scope.user_tasks.value)
def get_user_ide_courses() -> Response:
    """
    Get all courses that the user has bookmarked and have ideCourse tah
    :return: JSON response with all courses and their demo folders
    """

    user: User = current_token.user
    return json_response(user_ide_courses(user=user))


@ide.get("demosByDocId")
@require_oauth(Scope.user_tasks.value)
def get_ide_demos_by_doc_id() -> Response:
    """
    Get all demos by document id
    :return: JSON response with the task
    """

    doc_id = request.json.get("doc_id")
    if not doc_id:
        return json_response({"error": "No doc_id provided"})

    return json_response(demos_by_doc(doc_id=doc_id))


@ide.get("demosByDocPath")
@require_oauth(Scope.user_tasks.value)
def get_ide_demos_by_doc_path() -> Response:
    """
    Get all demos by document path
    :return: JSON response with the task
    """

    doc_path = request.json.get("doc_path")
    if not doc_path:
        return json_response({"error": "No doc_path provided"})

    return json_response(demos_by_doc(doc_path=doc_path))


@ide.get("tasksByDocPath")
@require_oauth(Scope.user_tasks.value)
def get_ide_tasks_by_doc_path() -> Response:
    """
    Get all tasks by demo folder path
    :return: JSON response with the task
    """
    doc_path = request.json.get("doc_path")
    if not doc_path:
        return json_response({"error": "No demo_path provided"})

    user: User = current_token.user

    return json_response(ide_tasks(demo_path=doc_path, user=user))


@ide.get("tasksByDocId")
@require_oauth(Scope.user_tasks.value)
def get_ide_tasks_by_doc_id() -> Response:
    """
    Get all tasks by demo folder doc_id
    :return: JSON response with the task
    """
    doc_id = request.json.get("doc_id")
    if not doc_id:
        return json_response({"error": "No doc_id provided"})

    user: User = current_token.user

    return json_response(ide_tasks(doc_id=doc_id, user=user))


@ide.get("tasksByIdeTaskId")
@require_oauth(Scope.user_tasks.value)
def get_ide_tasks_by_ide_task_id() -> Response:
    """
    Get all tasks by demo folder path
    :return: JSON response with the task
    """
    ide_task_id = request.json.get("ide_task_id")
    doc_id = request.json.get("doc_id")
    doc_path = request.json.get("doc_path")

    if not ide_task_id:
        return json_response({"error": "No ide_task_id provided"})

    if not doc_id and not doc_path:
        return json_response({"error": "No doc_id or doc_path provided"})

    user: User = current_token.user

    return json_response(
        ide_task_by_id(
            ide_task_id=ide_task_id,
            doc_id=doc_id,
            user=user,
            doc_path=doc_path,
        )
    )


@ide.get("submitTask")
@require_oauth(Scope.user_tasks.value)
def submit_ide_task() -> Response:
    """
    Submit a task

    :return: JSON response with the task
    """
    user = current_token.user
    task_id_ext = request.json.get("task_id_ext")
    code_files = request.json.get("code_files")
    code_language = request.json.get("code_language")
    if not task_id_ext or not code_files:
        return json_response({"error": "No task_id_ext or code file data provided"})

    answer = submit_task(
        code_files=code_files,
        task_id_ext=task_id_ext,
        code_language=code_language,
        user=user,
    )

    return json_response(answer)
