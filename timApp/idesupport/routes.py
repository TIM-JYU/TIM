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
from timApp.util.logger import log_info

ide = TypedBlueprint("ide", __name__, url_prefix="/ide")


@ide.get("ideCourses")
@require_oauth(Scope.user_tasks.value)
def get_user_ide_courses() -> Response:
    """
    Get all courses that the user has bookmarked and have ideCourse tag
    :return: JSON response with all courses and their task folders
    """
    log_info(f"tide ideCourses: {current_token.user.name}")
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

    log_info(f"tide tasksByDoc: {current_token.user.name}; {doc_id}; {doc_path}")
    return json_response(utils.get_ide_tasks(user, doc_path=doc_path, doc_id=doc_id))


@ide.get("tasksByCourse")
@require_oauth(Scope.user_tasks.value)
def get_ide_tasks_by_course(doc_id: int | None, doc_path: str | None) -> Response:
    """
    Gets all ide tasks for a given course
    :return: JSON response with tasks in task folders
    """

    # TODO: Not yet implemented in TIDE cli

    log_info(f"tide tasksByCourse: {current_token.user.name}; {doc_id}; {doc_path}")

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

    log_info(
        f"tide taskByIdeTaskId: {current_token.user.name}; {doc_id}; {doc_path}; {ide_task_id}"
    )

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

    log_info(
        f"tide submit: {current_token.user.name}; "
        + f"{request.json.get('code_files',[{'task_id_ext': '???'}])[0].get('task_id_ext', 'None')}"
    )

    submit = utils.TIDESubmitFileSchema.load(request.json)
    answer2 = None
    comtest_result = ""

    answer = utils.ide_submit_task(submit, user)
    ttype = answer.extra.get("type", "")
    if not answer.result.get("web", {}).get("error", "") and "comtest" in ttype:
        answer2 = utils.ide_submit_task(submit, user, "comtest")

    # TODO: poista seuraavat sitten kun tidecli osaa käsitellä pisteitä
    points = answer.extra.get("points")
    if answer2:
        points = answer2.extra.get("points")
        comtest_result = answer2.result.get("web", {}).get("console", "")

    console = answer.result.get("web", {}).get("console", "")
    if points:
        console = "Points: " + str(points) + "\n" + console
    else:
        console = "\n" + console  # TODO: can be removed when tidecli formats better
    answer.result.get("web", {})["console"] = console + "\n" + comtest_result

    # Hack for Jypeli rerun when few attributes missing
    # TODO: study if there is more cases when those are missing and
    # why they are missing
    if not answer.result.get("savedNew", None):
        answer.result["savedNew"] = None
    if not answer.result.get("valid", None):
        answer.result["valid"] = True

    answer.extra = {}  # älä päästä liikaa tietoa eteenpäin
    # TODO: Onko pakko palauttaa koko answer, eikö result riitä?
    return json_response(answer)


@ide.get("taskPoints")
@require_oauth(Scope.user_tasks.value)
def get_task_points(doc_path: str, ide_task_id: str) -> Response:
    """
    Get points data for a task
    :return: JSON response with the points data
    """
    user: User = current_token.user

    task = utils.get_ide_task_by_id(user, ide_task_id=ide_task_id, doc_path=doc_path)

    points = utils.get_task_points(task.doc_id, task.task_id, user)

    return json_response(points)
