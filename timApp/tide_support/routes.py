from authlib.integrations.flask_oauth2 import current_token
from flask import Response, request

from timApp.auth.oauth2.models import Scope
from timApp.auth.oauth2.oauth2 import require_oauth

from timApp.tide_support.utils import (
    user_ide_courses,
    ide_tasks,
    ide_task_by_id,
    ide_task_folders_by_doc,
    ide_submit_task,
)
from timApp.user.user import User
from timApp.util.flask.responsehelper import json_response
from timApp.util.flask.typedblueprint import TypedBlueprint

ide = TypedBlueprint("ide", __name__, url_prefix="/ide")


@ide.get("ideCourses")
@require_oauth(Scope.user_tasks.value)
def get_user_ide_courses() -> Response:
    """
    Get all courses that the user has bookmarked and have ideCourse tag
    :return: JSON response with all courses and their task folders
    """

    user: User = current_token.user
    return json_response(user_ide_courses(user=user))


@ide.get("taskFoldersByDoc")
@require_oauth(Scope.user_tasks.value)
def get_ide_task_folders_by_doc() -> Response:
    """
    Get all ide tasks folders by document id
    :return: JSON response with the list of folders
    """

    doc_id = request.json.get("doc_id")
    doc_path = request.json.get("doc_path")

    if not doc_id and not doc_path:
        return json_response({"error": "No doc_id or doc_path provided"})

    return json_response(ide_task_folders_by_doc(doc_id=doc_id, doc_path=doc_path))


@ide.get("tasksByDoc")
@require_oauth(Scope.user_tasks.value)
def get_ide_tasks_by_doc_path() -> Response:
    """
    Get all tasks by task folder path or doc_id
    :return: JSON response with the task
    """
    doc_path = request.json.get("doc_path")
    doc_id = request.json.get("doc_id")

    if not doc_path and not doc_id:
        return json_response({"error": "No doc_path or doc_id provided"})

    user: User = current_token.user

    return json_response(ide_tasks(doc_path=doc_path, doc_id=doc_id, user=user))


@ide.get("tasksByIdeTaskId")
@require_oauth(Scope.user_tasks.value)
def get_ide_tasks_by_ide_task_id() -> Response:
    """
    Get all tasks by folder path or doc_id and ide_task_id
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
    user_input = request.json.get("user_input")
    user_args = request.json.get("user_args")

    if not task_id_ext or not code_files or not code_language:
        return json_response(
            {"error": "No task_id_ext, code file or code language provided"}
        )

    answer = ide_submit_task(
        code_files=code_files,
        task_id_ext=task_id_ext,
        code_language=code_language,
        user=user,
        user_input=user_input,
        user_args=user_args,
    )

    return json_response(answer)
