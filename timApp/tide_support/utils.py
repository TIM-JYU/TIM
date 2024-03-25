import base64
import json
from dataclasses import dataclass
from typing import List

from bs4 import BeautifulSoup
from marshmallow import EXCLUDE
from flask import current_app

from timApp.answer.routes import post_answer_impl
from timApp.auth.accesshelper import verify_ip_ok, verify_access
from timApp.auth.accesstype import AccessType
from timApp.bookmark.bookmarks import MY_COURSES_GROUP, HIDDEN_COURSES_GROUP
from timApp.document.docentry import DocEntry
from timApp.document.docinfo import DocInfo
from timApp.document.usercontext import UserContext
from timApp.document.viewcontext import default_view_ctx
from timApp.plugin.containerLink import render_plugin_multi
from timApp.plugin.plugin import Plugin, PluginRenderOptions, PluginWrap
from timApp.plugin.pluginOutputFormat import PluginOutputFormat
from timApp.plugin.taskid import TaskId
from timApp.printing.printsettings import PrintFormat
from timApp.user.user import User
from timApp.util.flask.requesthelper import NotExist
from tim_common.marshmallow_dataclass import class_schema


@dataclass
class IdeFile:
    """
    File that contains the code and path for one TIDE-task
    """

    by: str | None = None
    """ Code of the file when plugin has only one file """

    byCode: str | None = None
    """ Code of the file when plugin has multiple files """

    path: str | None = None
    """ Path of the file for folder structure """

    # Convert to json and set code based on 'by' or 'byCode'
    def to_json(self):
        return {
            "content": self.by or self.byCode,
            "path": self.path,
        }


IdeFileSchema = class_schema(IdeFile)()


@dataclass
class TIDETaskInfo:
    """
    Information about the TIDE-task
    """

    header: str | None = None
    """
    Header of the plugin
    """

    stem: str | None = None
    """
    Stem of the plugin
    """

    answer_count: int | None = None
    """
    Number of answers for the task
    """

    # Might be needed to require this
    type: str | None = None
    """ Type of the file """


TIDETaskInfoSchema = class_schema(TIDETaskInfo)()


@dataclass
class TIDEPluginData:
    """
    Data for the TIDE-task
    """

    task_files: List[IdeFile] | None = None
    """
    Files for the TIDE-task
    """

    header: str | None = None
    """
    Header of the plugin
    """

    stem: str | None = None
    """
    Stem of the plugin
    """

    type: str | None = None
    """
    Type of the file
    """

    task_id: str | None = None
    """
    Task id
    """

    doc_id: int | None = None
    """
    Document id
    """

    par_id: int | None = None
    """
    Paragraph id
    """

    ide_task_id: str | None = None
    """
    TIDE-task id
    """


IDE_TASK_TAG = "ideTask"  # Identification tag for the TIDE-task


@dataclass
class TIDEError:
    """
    Error message for the TIDE-task
    """

    error: str


def user_ide_courses(user: User) -> list[json] | TIDEError:
    """
    Gets all courses that have parameter for Ide course in course settings and are bookmarked by the user
    :param user: Logged-in user
    :return: List JSON with all TIDE-tasks from the courses user has bookmarked
    """

    if not user.bookmarks.bookmark_data and len(user.bookmarks.bookmark_data) < 2:
        raise NotExist()

    user_courses = user.bookmarks.bookmark_data[2].get(MY_COURSES_GROUP)

    # Add hidden courses to the list
    if user.bookmarks.bookmark_data[2].get(HIDDEN_COURSES_GROUP):
        user_courses.extend(user.bookmarks.bookmark_data[2][HIDDEN_COURSES_GROUP])

    if not user_courses:
        return TIDEError("No courses found")

    ide_courses = []

    for course_dict in user_courses:
        for course_name, course_path in course_dict.items():
            # Get the document by path, remove /view/ from the beginning of path
            doc = DocInfo.find_by_path(course_path.split("view/")[1:][0])

            if doc is None:
                continue

            task_paths = doc.document.get_settings().ide_course()
            paths = []
            for path in task_paths:
                paths.append(path.path)

            if task_paths is None:
                continue

            ide_courses.append(
                {
                    "name": course_name,
                    "id": doc.document.doc_id,
                    "path": course_path,
                    "task_paths": paths,
                }
            )

    if not ide_courses:
        return TIDEError("No courses found")

    return ide_courses


def ide_task_folders_by_doc(
        doc_id: int = None, doc_path: str = None
) -> list[json] | TIDEError:
    """
    Find all TIDE-task folders from the document
    :param doc_path:
    :param doc_id: Document id

    :return: List JSON with all TIDE-task folders from the document
    """

    if doc_id is None and doc_path is None:
        return TIDEError("No document id or path given")

    if doc_id is None:
        doc = DocInfo.find_by_path(path=doc_path.lower())
    else:
        doc = DocEntry.find_by_id(doc_id=doc_id)

    if doc is None:
        return TIDEError("Document not found")

    task_paths = doc.document.get_settings().ide_course()

    if task_paths is None:
        return TIDEError("Document not found")

    paths = []
    for path in task_paths:
        paths.append(path.path)

    return paths


def ide_tasks(
        user: User, doc_id: int = None, doc_path: str = None
) -> list[TIDEPluginData] | TIDEError:
    """
    Get all TIDE-tasks from the task folder
    :param user: Logged-in user
    :param doc_id:  Document id
    :param doc_path: Path to the task folder
    :return: List JSON with all TIDE-tasks from the task folder
    """

    if doc_id is None and doc_path is None:
        raise NotExist()

    if doc_id is None:
        doc = DocInfo.find_by_path(path=doc_path.lower())
    else:
        doc = DocInfo.find_by_id(item_id=doc_id)

    if doc is None:
        return TIDEError("No document found")

    # Check if the user has edit access to the document
    verify_access(
        b=doc,
        access_type=AccessType(2),
        require=True,
        message="You do not have access to this document",
        user=user,
    )

    user_ctx = UserContext.from_one_user(u=user)

    pars = doc.document.get_paragraphs()

    tasks = []

    for p in pars:
        if p.attrs.get(IDE_TASK_TAG) is not None:
            task = ide_user_plugin_data(
                doc=doc, par=p, user_ctx=user_ctx, ide_task_id=p.attrs.get(IDE_TASK_TAG)
            )
            if task:
                tasks.append(task)

    if len(tasks) == 0:
        return TIDEError("No tasks found")

    return tasks


def ide_task_by_id(
        user: User,
        ide_task_id: str,
        doc_id: int = None,
        doc_path: str = None,
) -> TIDEPluginData | TIDEError:
    """
    Get TIDE-task from the document by document id and paragraph id
    :param ide_task_id:  TIDE-task id
    :param user: Logged-in user
    :param doc_id:  Document id
    :param doc_path:  Document path
    :return: JSON with TIDE-task ide-files, task info and task id
    """
    if doc_id is None and doc_path is None:
        return TIDEError(error="No document id or path given")

    if doc_path is None:
        doc = DocInfo.find_by_id(item_id=doc_id)
    else:
        doc = DocInfo.find_by_path(path=doc_path.lower())

    # If the document does not exist, raise NotExist
    if doc is None:
        return TIDEError("No document found")

    # Check if the user has edit access to the document

    verify_access(
        b=doc,
        access_type=AccessType(2),
        require=True,
        message="You do not have access to this document",
        user=user,
    )

    user_ctx = UserContext.from_one_user(u=user)

    pars = doc.document.get_paragraphs()

    if pars is None:
        return TIDEError("No paragraphs found")

    tasks = []

    for p in pars:
        if p.attrs.get(IDE_TASK_TAG) == ide_task_id:
            task = ide_user_plugin_data(
                doc=doc, par=p, user_ctx=user_ctx, ide_task_id=ide_task_id
            )
            if task:
                tasks.append(task)

    if len(tasks) == 0:
        return TIDEError("No tasks found")

    if len(tasks) == 1:
        return tasks[0]

    # TODO: case where files are saved based on language base folders eg path is taken from the language package

    return TIDEError("Multiple tasks found, support not implemented yet")


def ide_user_plugin_data(
        doc: DocInfo,
        par,
        ide_task_id: str,
        user_ctx: UserContext,
) -> TIDEPluginData | TIDEError:
    """
    Get the TIDE-task information from the plugin
    :param ide_task_id:  TIDE-task id
    :param doc: TIM document
    :param par: Paragraph from the document
    :param user_ctx: User context
    :return: JSON with TIDE-task ide-files, task info and task id
    """

    view_ctx = default_view_ctx

    plugin = Plugin.from_paragraph(par, view_ctx, user_ctx)

    if plugin.type != "csPlugin":
        return TIDEError(error="Not a csPlugin plugin")

    # Plugin render options
    plugin_opts = PluginRenderOptions(
        do_lazy=False,
        user_print=False,
        preview=view_ctx.preview,
        target_format=PrintFormat.JSON,
        output_format=PluginOutputFormat.HTML,
        user_ctx=user_ctx,
        review=False,
        wraptype=PluginWrap.Nothing,
        viewmode=view_ctx.viewmode,
    )

    plugin.set_render_options(None, plugin_opts)
    res = render_plugin_multi(doc.document.get_settings(), "csPlugin", [plugin])
    plugin_htmls = json.loads(res)
    plugin_html = BeautifulSoup(plugin_htmls[0], features="lxml")
    element = plugin_html.select_one("cs-runner")
    plugin_json = json.loads(base64.b64decode(element.attrs["json"]).decode("utf-8"))

    task_info: TIDETaskInfo = TIDETaskInfoSchema.load(plugin.values, unknown=EXCLUDE)

    task_id: TaskId | None = plugin.task_id

    # If plugin does not have task_id, return empty list
    if task_id is None:
        return TIDEError(error="No task id found")

    # If the plugin has files attribute
    if plugin_json["markup"].get("files"):
        ide_files = IdeFileSchema.load(
            plugin_json["markup"]["files"], many=True, unknown=EXCLUDE
        )
        json_ide_files = [file.to_json() for file in ide_files]

    # If the plugin has only one file
    else:
        ide_file = IdeFileSchema.load(plugin_json, unknown=EXCLUDE)
        # Give the file main.<type> name if the file has no path and the type is not None
        if ide_file.path is None and task_info.type is not None:
            ide_file.path = "main." + task_info.type
        json_ide_files = [ide_file.to_json()]

    return TIDEPluginData(
        task_files=json_ide_files,
        header=task_info.header,
        stem=task_info.stem,
        type=task_info.type,
        task_id=task_id.task_name,
        doc_id=doc.id,
        par_id=par.id,
        ide_task_id=ide_task_id,
    )


def ide_submit_task(
        code_files: dict[str], task_id_ext: str, code_language: str, user: User
):
    """
    Submit the TIDE-task
    :param user: Current user
    :param code_language: Language of the code
    :param code_files: Code files for the TIDE-task
    :param task_id_ext:  "<doc_id>.<task_id>"
    :return: True if the task was submitted successfully
    """

    submitted_files = []
    # If the return has only one file
    if len(code_files) == 1:
        user_code = code_files[0].get("content")
    else:
        # If the return has multiple files
        user_code = code_files[-1].get(
            "content"
        )  # Apparently the last file should be here
        for file in code_files:
            submitted_files.append(file)

    answer_data = {
        "isInput": False,
        "nosave": False,
        "type": code_language,
        "uploadedFiles": [],
        "submittedFiles": submitted_files,
        "userargs": "",
        "usercode": user_code,
        "userinput": "",
    }

    brow_data = {
        "answer_id": None,
        "answernr": None,
        "giveCustomPoints": False,
        "points": None,
        "saveAnswer": True,
        "saveTeacher": False,
        "teacher": False,
        "userId": user.id,
    }

    # Check if the answer from user IP is allowed
    blocked_msg = (
            current_app.config["IP_BLOCK_ROUTE_MESSAGE"]
            or "Answering is not allowed from this IP address."
    )
    allowed = verify_ip_ok(user=user, msg=blocked_msg)

    return post_answer_impl(
        task_id_ext=task_id_ext,
        answerdata=answer_data,
        answer_browser_data=brow_data,
        answer_options={},
        curr_user=user,
        urlmacros=(),
        other_session_users=[],
        origin=None,
        error=blocked_msg if not allowed else None,
    )
