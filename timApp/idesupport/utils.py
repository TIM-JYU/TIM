import base64
import json
import os
import re
from dataclasses import dataclass, field
from typing import List

from bs4 import BeautifulSoup
from marshmallow import EXCLUDE
from sqlalchemy import select

from timApp.answer.answer import Answer
from timApp.answer.answer_models import UserAnswer
from timApp.answer.answers import valid_answers_query
from timApp.answer.routes import (
    post_answer_impl,
    verify_ip_address,
    AnswerRouteResult,
    InputAnswer,
)
from timApp.auth.accesshelper import (
    verify_view_access,
)
from timApp.bookmark.bookmarks import MY_COURSES_GROUP, HIDDEN_COURSES_GROUP
from timApp.document.docentry import DocEntry
from timApp.document.docinfo import DocInfo
from timApp.document.docparagraph import DocParagraph
from timApp.document.usercontext import UserContext
from timApp.document.viewcontext import default_view_ctx
from timApp.idesupport.files import SupplementaryFile, get_task_language
from timApp.idesupport.ide_languages import Language
from timApp.plugin.containerLink import render_plugin_multi
from timApp.plugin.plugin import Plugin, PluginRenderOptions, PluginWrap, find_task_ids
from timApp.plugin.pluginControl import get_answers, AnswerMap
from timApp.plugin.pluginOutputFormat import PluginOutputFormat
from timApp.plugin.taskid import TaskId
from timApp.printing.printsettings import PrintFormat
from timApp.timdb.sqa import run_sql
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

    program: str | None = None
    """ Code, when the task is meant to be fully editable """

    filename: str | None = None
    """ Name of the file """

    task_directory: str | None = None
    """ Directory of the task """

    task_type: str | None = None
    """ type of the file """

    userinput: str | None = ""
    """ User input for the file """

    userargs: str | None = ""
    """ User arguments for the file """

    # usercode: str | None = ""
    # """ User code for the file """

    content: str | None = field(init=False)
    """File contents provided for IDE"""

    # TODO: Maybe should be field(default_factory=Language)
    language: Language = field(default_factory=Language)

    def __post_init__(self) -> None:
        self.content = ""

    def set_combined_code(self, usercode: str) -> None:
        """Combine the code blocks and set the combined code to the
        content var.
        :param usercode: None or user last answer
        """
        # Program can be considered as a boilerplate
        boilerplate = self.program
        user_code = None
        filename = self.filename

        # Both "by" or "byCode" attributes are considered as user editable code
        if self.by is not None:
            user_code = self.by

        if self.byCode is not None:
            user_code = self.byCode

        if usercode is not None:
            user_code = usercode

        # TODO: think if there is need for code?  Why empty file is not ok?
        # if boilerplate is None and user_code is None:
        #    raise RouteException("No code found in the plugin")

        if boilerplate is None:
            self.content = user_code or ""
            return

        # If no dedicated user editable code, the whole boilerplate is editable
        if user_code is None:
            self.content = boilerplate
            return

        if filename is None:
            raise RouteException("File name not provided")

        # Find the "comment line" characters based on the file extension and create messages.
        comment_line_characters = self.language.find_comment_line_characters()

        user_code_begins_message = (
            comment_line_characters + " --- Write your code below this line. ---"
        )
        user_code_ends_message = (
            comment_line_characters + " --- Write your code above this line. ---"
        )

        # If the program is not meant to be fully editable, add the messages to the code.
        if boilerplate != "REPLACEBYCODE":
            annotated_block = (
                user_code_begins_message + "\nREPLACEBYCODE\n" + user_code_ends_message
            )
            boilerplate = boilerplate.replace("REPLACEBYCODE", annotated_block)
        boilerplate = boilerplate.replace("REPLACEBYCODE", user_code)
        self.content = boilerplate

    def generate_file_extension(self, task_type: str) -> None:
        """Ensure that the filename with extension will be set for the
        ide_file.

        :param task_type: Type of task, containing the file extension
        :return: None
        """

        if self.filename is None:
            self.filename = "main"

        _, predefined_file_extension = os.path.splitext(self.filename)
        if predefined_file_extension != "":
            return

        if task_type is None:
            raise RouteException("File extension cannot be generated")

        # PLEASE NOTE: task_info.type could be fancy like c++/input/comtest
        if self.language is None:
            raise RouteException("Misconfigured task language")
        self.filename += "." + str(self.language.fileext)

    # Convert to json and set code based on 'by' or 'byCode'
    def to_json(self) -> dict[str, str | None]:
        return {
            "task_id_ext": self.taskIDExt,
            "content": self.content,
            "file_name": self.filename,
            "task_directory": self.task_directory,
            "task_type": self.task_type,
            "user_input": self.userinput,
            "user_args": self.userargs or "",
        }


IdeFileSchema = class_schema(IdeFile)()
SupplementaryFileSchema = class_schema(SupplementaryFile)()


@dataclass
class TIDETaskInfo:
    """
    Information about the TIDE-task
    """

    header: str | None = None
    """
    Header of the plugin
    """

    description: str | None = None
    """
    Description of the plugin. Works the same as the header, but only shown in the IDE.
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

    supplementary_files: List[SupplementaryFile] | None = None
    """
    Supplementary files for the task
    """

    path: str | None = None
    """
    Path to the task
    """

    task_directory: str | None = None
    """
    Directory of the task
    """

    task_type: str | None = None
    """
    Type of the file
    """

    header: str | None = None
    """
    Header of the plugin
    """

    stem: str | None = None
    """
    Stem of the plugin
    """

    max_points: float | None = None
    """
    Maximum total points for the task
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

    deadline: str | None = None
    """
    ISO8601 date string for the task deadline. Server time is UTC+0, so users of this property will need to account for the local offset.
    """

    answer_limit: int | None = None
    """
    Maximum number of answers that the user can save in the task.
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

            if not task_paths:
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

            if not paths:
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


def get_ide_tasks_implementation(
    user: User,
    doc_id: int | None = None,
    doc_path: str | None = None,
    ide_task_id: str | None = None,
) -> list[TIDEPluginData]:
    """
    Find ide tasks from pars and get user answers
    :param user: Logged-in user
    :param doc_id:  Document id
    :param doc_path: Path to the task set document
    :param ide_task_id: ideTask to find or get all if None
    :return: list of ideTasks
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
        tag: str | None = "???"
        try:
            if p.attrs is not None:
                tag = p.attrs.get(IDE_TASK_TAG)
                if tag is not None:
                    if tag == "":
                        tag = p.attrs.get("taskId")
                    if tag is None:
                        raise RouteException("Missing taskID!")
                    if ide_task_id and ide_task_id != tag:
                        continue
                    task = get_ide_user_plugin_data(
                        doc=doc, par=p, user_ctx=user_ctx, ide_task_id=tag
                    )
                    if task:
                        tasks.append(task)
        except Exception as e:
            task = TIDEPluginData(
                task_files=[],
                supplementary_files=[
                    SupplementaryFile(
                        filename="error.txt",
                        content="Error: " + str(e),
                    )
                ],
                header="Error: " + str(e),
                stem="Error: " + str(e),
                type="error",
                path=doc.path,
                task_id=(p.attrs or {}).get("taskId"),
                doc_id=doc.id,
                par_id=p.id,
                ide_task_id="error_" + (tag or "???"),
            )
            tasks.append(task)

    return tasks


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

    tasks = get_ide_tasks_implementation(user, doc_id, doc_path)

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

    tasks = get_ide_tasks_implementation(user, doc_id, doc_path, ide_task_id)

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

    if plugin.type not in ["csPlugin", "taunoPlugin"]:
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

    # Get user answer for the task
    task_ids, _, _ = find_task_ids(
        [par], view_ctx, user_ctx, check_access=user_ctx.is_different
    )
    answer_map: AnswerMap = {}
    answer = None
    get_answers(user_ctx.user, task_ids, answer_map)
    if answer_map:  # If there was an answer
        first_key = next(iter(answer_map))
        answer = answer_map[first_key][0]

    plugin.set_render_options(None, plugin_opts)
    plugin.answer = answer
    res = render_plugin_multi(doc.document.get_settings(), "csPlugin", [plugin])
    plugin_htmls = json.loads(res)
    plugin_html = BeautifulSoup(plugin_htmls[0], features="lxml")
    element = plugin_html.select_one("cs-runner")
    plugin_json = json.loads(base64.b64decode(element.attrs["json"]).decode("utf-8"))

    task_info: TIDETaskInfo = TIDETaskInfoSchema.load(plugin.values, unknown=EXCLUDE)
    selected_language = plugin_json.get("markup").get("selectedLanguage")
    if selected_language:
        task_language = get_task_language(selected_language)
    else:
        task_language = get_task_language(task_info.type)
    if task_language is None:
        raise RouteException("Misconfigured task language")
    language = Language.make_language(task_language, plugin_json, ide_task_id)

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
        ide_file.language = language

        # if the plugin has no code, look from markup
        if ide_file.by is None and ide_file.byCode is None and ide_file.program is None:
            ide_file = IdeFileSchema.load(plugin_json["markup"], unknown=EXCLUDE)
            ide_file.language = language
            if ide_file.taskIDExt is None:
                if plugin_json.get("taskIDExt"):
                    ide_file.taskIDExt = plugin_json["taskIDExt"]
                else:
                    return None

        # TODO: Think if error is needed?
        # If the plugin still has no code, return error
        # if ide_file.by is None and ide_file.byCode is None and ide_file.program is None:
        #     raise RouteException("No code found in the plugin")

        # if the ide_file has no filename, try to look it from the markup
        if ide_file.filename is None:
            ide_file.filename = language.get_filename()

        if ide_file.task_directory is None:
            ide_file.task_directory = language.get_task_directory()

        if ide_file.task_type is None:
            ide_file.task_type = task_info.type

        # If the task type is defined, try to generate file extension.
        if task_info.type is not None:
            ide_file.generate_file_extension(task_info.type)

        ide_file.set_combined_code(plugin_json.get("usercode"))
        json_ide_files = [ide_file.to_json()]

    ide_extra_files = plugin_json["markup"].get("ide_extra_files") or []

    supplementary_files = language.generate_supplementary_files(ide_extra_files)

    # If both content and source are provided, content is used (see tidecli)
    # If neither is provided, no supplementary file will be created
    task_extra_directory = plugin_json["markup"].get("task_extra_directory", None)

    for extra_file in ide_extra_files:
        if "task_directory" not in extra_file:
            extra_file["task_directory"] = task_extra_directory
        if "content" in extra_file:
            supplementary_files.append(SupplementaryFileSchema.load(extra_file))
        elif "source" in extra_file:
            supplementary_files.append(SupplementaryFileSchema.load(extra_file))

    max_points = None
    try:
        max_points = float(plugin.max_points())
    except (ValueError, TypeError):
        pass

    task_deadline = plugin.deadline()
    task_answer_limit = plugin.answer_limit()

    return TIDEPluginData(
        task_files=json_ide_files,
        supplementary_files=supplementary_files,
        header=task_info.description or task_info.header,
        stem=task_info.stem,
        type=task_info.type,
        path=doc.path,
        task_id=task_id.task_name,
        task_directory=ide_file.task_directory,
        doc_id=doc.id,
        par_id=par.id,
        ide_task_id=ide_task_id,
        max_points=max_points,
        deadline=task_deadline,
        answer_limit=task_answer_limit,
    )


def ide_submit_task(
    submit: TIDESubmitFile, user: User, runtype: str | None = None
) -> AnswerRouteResult:
    """
    Submit the TIDE-task
    :param submit: TIDESubmitFile
    :param user: Current user
    :param runtype: Optional forced runtype, for example comtest
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

    answer_data: InputAnswer = {
        "isInput": False,
        "nosave": False,
        "type": runtype or re.split("[,;/]", submit.code_language)[0],
        "uploadedFiles": [],
        "submittedFiles": submitted_files,
        "userargs": submit.code_files[file_index].user_args,
        "usercode": submit.code_files[file_index].content,
        "userinput": submit.code_files[file_index].user_input,
    }

    brow_data = {
        "giveCustomPoints": False,
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


def get_task_points(doc_id: int, task_id: str, user: User) -> float | None:
    """
    Get the current points for the latest valid answer in the task

    :param doc_id: Document id
    :param task_id:  Task id
    :param user:  Current user
    :return:  Current points for the task or None
    """
    return run_sql(
        select(Answer.points)
        .join(UserAnswer)
        .filter(
            (Answer.task_id == f"{doc_id}.{task_id}")
            & (Answer.valid == True)
            & (UserAnswer.user_id == user.id)
        )
        .order_by(Answer.id.desc())
        .limit(1)
    ).scalar()
