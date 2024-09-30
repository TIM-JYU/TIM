from authlib.integrations.flask_oauth2 import current_token
from flask import Response, request

from timApp.auth.oauth2.models import Scope
from timApp.auth.oauth2.oauth2 import require_oauth

import timApp.idesupport.utils as utils

from timApp.tim_app import csrf
from timApp.user.user import User
from timApp.util.flask.requesthelper import RouteException
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
    return json_response(utils.get_user_ide_courses(current_token.user))


@ide.get("tasksByDoc")
@require_oauth(Scope.user_tasks.value)
def get_ide_tasks_by_doc_path(doc_path: str | None, doc_id: int | None) -> Response:
    """
    Get all tasks by task set document path or doc_id
    :return: JSON response with the task
    """

    if not doc_path and not doc_id:
        raise RouteException("No doc_path or doc_id provided")

    user: User = current_token.user

    return json_response(utils.get_ide_tasks(user, doc_path=doc_path, doc_id=doc_id))


@ide.get("tasksByCourse")
@require_oauth(Scope.user_tasks.value)
def get_ide_tasks_by_course(doc_id: int | None, doc_path: str | None) -> Response:
    """
    Gets all ide tasks for a given course
    :return: JSON response with tasks in task folders
    """

    # TODO: Not yet implemented in TIDE cli

    if not doc_path and not doc_id:
        raise RouteException("No course path or doc id provided")

    user = current_token.user

    doc_paths = utils.get_ide_task_set_documents_by_doc(
        user, doc_path=doc_path, doc_id=doc_id
    )

    user = current_token.user

    tasks_by_folder = []
    for task_set_doc in doc_paths:
        tasks = [utils.get_ide_tasks(user, doc_path=task_set_doc.path)]
        tasks_by_folder.append(tasks)

    return json_response(tasks_by_folder)


@ide.get("taskByIdeTaskId")
@require_oauth(Scope.user_tasks.value)
def get_ide_task_by_ide_task_id(
    ide_task_id: str, doc_id: int | None, doc_path: str | None
) -> Response:
    """
    Get all tasks by document path or document id and ide_task_id
    :return: JSON response with the task
    """

    if not doc_id and not doc_path:
        raise RouteException("No doc_id or doc_path provided")

    user: User = current_token.user

    return json_response(
        utils.get_ide_task_by_id(
            user,
            ide_task_id=ide_task_id,
            doc_id=doc_id,
            doc_path=doc_path,
        )
    )


@ide.put("submitTask")
@csrf.exempt
@require_oauth(Scope.user_tasks.value)
def submit_ide_task() -> Response:
    """
    Submit a task

    :return: JSON response with the task
    """
    user = current_token.user

    if not request.json:
        raise RouteException("No JSON data provided")

    submit = utils.TIDESubmitFileSchema.load(request.json)

    answer = utils.ide_submit_task(submit, user)

    # Hack for Jypeli rerun when few attributes missing
    # TODO: study is thera more cases when those are missing and
    # why they are missing
    if not answer.result.get('savedNew', None):
        answer.result['savedNew'] = None
    if not answer.result.get('valid', None):
        answer.result['valid'] = True

    return json_response(answer)
