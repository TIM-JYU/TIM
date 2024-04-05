import base64
import json
from dataclasses import dataclass
from typing import List

from bs4 import BeautifulSoup
from marshmallow import EXCLUDE

from timApp.answer.routes import post_answer_impl, verify_ip_address, AnswerRouteResult
from timApp.auth.accesshelper import (
    verify_view_access,
)
from timApp.bookmark.bookmarks import MY_COURSES_GROUP, HIDDEN_COURSES_GROUP
from timApp.document.docentry import DocEntry
from timApp.document.docinfo import DocInfo
from timApp.document.docparagraph import DocParagraph
from timApp.document.usercontext import UserContext
from timApp.document.viewcontext import default_view_ctx
from timApp.plugin.containerLink import render_plugin_multi
from timApp.plugin.plugin import Plugin, PluginRenderOptions, PluginWrap
from timApp.plugin.pluginOutputFormat import PluginOutputFormat
from timApp.plugin.taskid import TaskId
from timApp.printing.printsettings import PrintFormat
from timApp.user.user import User
from timApp.util.flask.requesthelper import NotExist, RouteException
from tim_common.marshmallow_dataclass import class_schema

IDE_TASK_TAG = "ideTask"  # Identification tag for the TIDE-task


@dataclass
class IdeFile:
    """
    File that contains the code and path for one TIDE-task
    """

    taskIDExt: str | None = None
    """ Task id extension """

    by: str | None = None
    """ Code of the file when plugin has only one file """

    byCode: str | None = None
    """ Code of the file when plugin has multiple files """

    filename: str | None = None
    """ Name of the file """

    userinput: str | None = ""
    """ User input for the file """

    userargs: str | None = ""
    """ User arguments for the file """

    # Convert to json and set code based on 'by' or 'byCode'
    def to_json(self) -> dict[str, str | None]:
        return {
            "task_id_ext": self.taskIDExt,
            "content": self.by or self.byCode,
            "file_name": self.filename,
            "user_input": self.userinput,
            "user_args": self.userargs or "",
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
    """Stem of the plugin"""

    answer_count: int | None = None
    """
    Number of answers for the task
    """

    # Might be needed to require this
    type: str | None = None
    """ Type of the file """


TIDETaskInfoSchema = class_schema(TIDETaskInfo)()


@dataclass
class SubmitCodeFile:
    content: str
    file_name: str | None = None
    source: str = "editor"
    task_id_ext: str | None = None  # "<doc_id>.<task_id>"
    user_input: str | None = ""
    user_args: str | None = ""


@dataclass
class TIDESubmitFile:
    """
    Submittable code files
    """

    code_files: list[SubmitCodeFile]
    code_language: str
    user_input: str = ""
    user_args: str = ""


TIDESubmitFileSchema = class_schema(TIDESubmitFile)()


@dataclass
class TIDEPluginData:
    """
    Data for the TIDE-task
    """

    task_files: List[IdeFile] | None = None
    """
    Files for the TIDE-task
    """

    path: str | None = None
    """
    Path to the task
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

    par_id: str | None = None
    """
    Paragraph id
    """

    ide_task_id: str | None = None
    """
    TIDE-task id
    """


@dataclass
class TIDETaskSetDoc:
    """
    Document where the TIDE-task is located
    """

    name: str
    """
    Name of the task set document
    """

    path: str
    """
    Path to the task set document
    """

    doc_id: int
    """
    Id of the task set document
    """


@dataclass
class TIDETaskSetDocument:
    """
    Document that has TIDE task sets
    """

    path: str


@dataclass
class TIDECourse:
    """
    Course information for the TIDE-task
    """

    name: str
    """
    Name of the course
    """

    id: int
    """
    Document id
    """

    path: str
    """
    Path to the course
    """

    tasks: list[TIDETaskSetDoc]
    """
    Paths to the tasks
    """


def get_user_ide_courses(user: User) -> list[TIDECourse]:
    """
    Gets all courses that have parameter for Ide course in course settings and are bookmarked by the user
    :param user: Logged-in user
    :return: List of TIDECourse from the courses user has bookmarked
    """

    user_courses = user.bookmarks.get_bookmarks_in_group(MY_COURSES_GROUP) or []
    hidden_user_courses = user.bookmarks.get_bookmarks_in_group(HIDDEN_COURSES_GROUP)

    if hidden_user_courses is not None:
        user_courses.extend(hidden_user_courses)

    if not user_courses:
        raise NotExist("No courses found")

    ide_courses = []

    for course_dict in user_courses:
        for course_name, course_path in course_dict.items():
            # Get the document by path, remove /view/ from the beginning of path
            view_str = "/view/"
            if not course_path.startswith(view_str):
                continue

            course_path = course_path[len(view_str) :]
            doc = DocEntry.find_by_path(course_path)

            if doc is None:
                continue

            task_paths = doc.document.get_settings().ide_course()

            if task_paths == [] or task_paths is None:
                continue

            paths = []

            for path in task_paths:
                # Get the document by path for doc id
                task_doc = DocEntry.find_by_path(path.path)

                if task_doc is None:
                    continue

                paths.append(
                    TIDETaskSetDoc(
                        path=path.path,
                        doc_id=task_doc.document.doc_id,
                        name=task_doc.title,
                    )
                )

            if paths == []:
                continue

            course = TIDECourse(
                name=course_name,
                id=doc.document.doc_id,
                path=course_path,
                tasks=paths,
            )

            ide_courses.append(course)

    if not ide_courses:
        raise NotExist("No courses found")

    return ide_courses


def get_ide_task_set_documents_by_doc(
    user: User, doc_id: int | None = None, doc_path: str | None = None
) -> list[TIDETaskSetDocument]:
    """
    Find all TIDE-task set documents from the document
    :param user: Current user
    :param doc_path: Document path
    :param doc_id: Document id

    :return: List TIDETaskFolders with all TIDE-task documents from the document
    """

    if doc_id is None and doc_path is None:
        raise RouteException("No document id or path given")

    if doc_id is None:
        path = doc_path
        if path is None:
            raise RouteException("No document path given")
        doc = DocEntry.find_by_path(path=path)
    else:
        doc = DocEntry.find_by_id(doc_id=doc_id)

    if doc is None:
        raise RouteException("Document not found")

    # Check if the user has view access to the document
    verify_view_access(doc, user=user)

    task_paths = doc.document.get_settings().ide_course()

    if task_paths is None:
        raise NotExist("Document not found")

    paths = []
    for p in task_paths:
        paths.append(TIDETaskSetDocument(path=p.path))

    return paths


def get_ide_tasks(
    user: User, doc_id: int | None = None, doc_path: str | None = None
) -> list[TIDEPluginData]:
    """
    Get all TIDE-tasks from the task set document
    :param user: Logged-in user
    :param doc_id:  Document id
    :param doc_path: Path to the task set document
    :return: List of TIDEPluginData from the task set document or RouteException
    :raise In case of an error raises RouteException
    """

    if doc_path is not None:
        doc = DocEntry.find_by_path(path=doc_path)

    elif doc_id is not None:
        doc = DocEntry.find_by_id(doc_id=doc_id)
    else:
        raise RouteException("No document id or path given")

    if doc is None:
        raise NotExist("Document not found")

    # Check if the user has edit access to the document
    verify_view_access(doc, user=user)

    user_ctx = UserContext.from_one_user(u=user)

    pars = doc.document.get_paragraphs()

    tasks = []

    for p in pars:
        if p.attrs is not None:
            tag = p.attrs.get(IDE_TASK_TAG)
            if tag is not None:
                task = get_ide_user_plugin_data(
                    doc=doc, par=p, user_ctx=user_ctx, ide_task_id=tag
                )
                if task:
                    tasks.append(task)

    if len(tasks) == 0:
        raise NotExist(
            "No valid IDE tasks found. A valid task must be a csPlugin and have been marked as an IDE task"
        )

    return tasks


def get_ide_task_by_id(
    user: User,
    ide_task_id: str,
    doc_id: int | None = None,
    doc_path: str | None = None,
) -> TIDEPluginData:
    """
    Get TIDE-task from the document by document id and paragraph id
    :param ide_task_id:  TIDE-task id
    :param user: Logged-in user
    :param doc_id:  Document id
    :param doc_path:  Document path
    :return: TIDEPluginData or TIDEError
    :raises In case of an error raises RouteException
    """

    if doc_id is not None:
        doc = DocEntry.find_by_id(doc_id=doc_id)
    elif doc_path is not None:
        doc = DocEntry.find_by_path(path=doc_path)
    else:
        raise RouteException("No document id or path given")
    # If the document does not exist, raise NotExist
    if doc is None:
        raise NotExist("No document found")

    # Check if the user has edit access to the document
    verify_view_access(doc, user=user)

    user_ctx = UserContext.from_one_user(u=user)

    pars = doc.document.get_paragraphs()

    if pars is None:
        raise NotExist("No paragraphs found")

    tasks = []

    for p in pars:
        if p.attrs is not None:
            if p.attrs.get(IDE_TASK_TAG) == ide_task_id:
                task = get_ide_user_plugin_data(
                    doc=doc, par=p, user_ctx=user_ctx, ide_task_id=ide_task_id
                )
                if task:
                    tasks.append(task)

    if len(tasks) == 0:
        raise RouteException(
            "No valid IDE tasks found. A valid task must be a csPlugin and have been marked as an IDE task"
        )

    if len(tasks) == 1:
        return tasks[0]

    # TODO: case where files are saved based on language base folders eg path is taken from the language package

    raise RouteException("Multiple tasks found, support not implemented yet")


def get_ide_user_plugin_data(
    doc: DocInfo,
    par: DocParagraph,
    ide_task_id: str,
    user_ctx: UserContext,
) -> TIDEPluginData | None:
    """
    Get the TIDE-task information from the plugin
    :param ide_task_id:  TIDE-task id
    :param doc: TIM document
    :param par: Paragraph from the document
    :param user_ctx: User context
    :return: TIDEPluginData or None if the plugin is not a TIDE-task
    """

    view_ctx = default_view_ctx

    plugin = Plugin.from_paragraph(par, view_ctx, user_ctx)

    if plugin.type != "csPlugin":
        return None

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
        return None

    # If the plugin has files attribute
    if plugin_json["markup"].get("files"):
        # TODO: Implement multiple files
        # ide_files = IdeFileSchema.load(
        #     plugin_json["markup"]["files"], many=True, unknown=EXCLUDE
        # )
        # json_ide_files = [file.to_json() for file in ide_files]
        #
        # if ide_files.taskIDExt is None:
        #     if plugin_json.get("taskIDExt"):
        #         ide_files.taskIDExt = plugin_json["taskIDExt"]
        #     else:
        #         raise RouteException("No taskIDExt found in the plugin")
        return None

    # If the plugin has only one file
    else:
        # If the plugin has only one file, load the file based on 'by' or 'byCode'
        ide_file = IdeFileSchema.load(plugin_json, unknown=EXCLUDE)

        # if the plugin has no code, look from markup
        if ide_file.by is None and ide_file.byCode is None:
            ide_file = IdeFileSchema.load(plugin_json["markup"], unknown=EXCLUDE)
            if ide_file.taskIDExt is None:
                if plugin_json.get("taskIDExt"):
                    ide_file.taskIDExt = plugin_json["taskIDExt"]
                else:
                    return None

        # If the plugin still has no code, return error
        if ide_file.by is None and ide_file.byCode is None:
            raise RouteException("No code found in the plugin")

        # if the ide_file has no filename, try to look it from the markup
        if ide_file.filename is None:
            ide_file.filename = plugin_json["markup"].get("filename")

        # Give the file main.<type> name if the file has no filename and the type is not None
        if ide_file.filename is None and task_info.type is not None:
            if "c++" in task_info.type:
                ide_file.filename = "main.cpp"
            elif "cc" in task_info.type:
                ide_file.filename = "main.c"
            else:
                ide_file.filename = "main." + task_info.type

        json_ide_files = [ide_file.to_json()]

    return TIDEPluginData(
        task_files=json_ide_files,
        header=task_info.header,
        stem=task_info.stem,
        type=task_info.type,
        path=doc.path,
        task_id=task_id.task_name,
        doc_id=doc.id,
        par_id=par.id,
        ide_task_id=ide_task_id,
    )


def ide_submit_task(submit: TIDESubmitFile, user: User) -> AnswerRouteResult:
    """
    Submit the TIDE-task
    :param submit: TIDESubmitFile
    :param user: Current user
    :return: True if the task was submitted successfully
    """

    submitted_files = []
    # If the return has only one file
    if len(submit.code_files) == 1:
        file_index = 0
    else:
        # If the return has multiple files, files parameter is used
        file_index = -1
        # Apparently the last file should be here
        for file in submit.code_files:
            submitted_files.append(file)

    answer_data = {
        "isInput": False,
        "nosave": False,
        "type": submit.code_language,
        "uploadedFiles": [],
        "submittedFiles": submitted_files,
        "userargs": submit.code_files[file_index].user_args,
        "usercode": submit.code_files[file_index].content,
        "userinput": submit.code_files[file_index].user_input,
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
    task_id_ext = submit.code_files[file_index].task_id_ext

    if task_id_ext is None:
        raise RouteException("No task id extension found in the plugin")

    return post_answer_impl(
        task_id_ext=task_id_ext,
        answerdata=answer_data,
        answer_browser_data=brow_data,
        answer_options={},
        curr_user=user,
        urlmacros=(),
        other_session_users=[],
        origin=None,
        error=verify_ip_address(user),  # Check if the answer from user IP is allowed
    )
