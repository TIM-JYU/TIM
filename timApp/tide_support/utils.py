import base64
import json
from dataclasses import dataclass

from bs4 import BeautifulSoup
from marshmallow import EXCLUDE

from timApp.answer.routes import post_answer_impl
from timApp.bookmark.bookmarks import MY_COURSES_GROUP
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

IDE_TASK_TAG = "ideTask"  # Identification tag for the TIDE-task


def user_ide_courses(user: User) -> list[json]:
    """
    Gets all courses that have parameter for Ide course in course settings and are bookmarked by the user
    :param user: Logged-in user
    :return: List JSON with all TIDE-tasks from the courses user has bookmarked
    """
    user_courses = user.bookmarks.bookmark_data[2][MY_COURSES_GROUP]
    ide_courses = []

    for course_dict in user_courses:
        for course_name, course_path in course_dict.items():
            # Get the document by path, remove /view/ from the beginning of path
            doc = DocInfo.find_by_path(course_path.split("view/")[1:][0])

            if doc is None:
                continue

            demo_paths = doc.document.get_settings().ide_course()
            paths = []
            for path in demo_paths:
                paths.append(path.path)

            if demo_paths is None:
                continue

            ide_courses.append(
                {
                    "name": course_name,
                    "id": doc.document.doc_id,
                    "path": course_path,
                    "demo_paths": paths,
                }
            )

    return ide_courses


def demos_by_doc(doc_id: int = None, doc_path: str = None):
    """
    Find all TIDE-demo folders from the document
    :param doc_path:
    :param doc_id: Document id

    :return: List JSON with all TIDE-demo folders from the document
    """

    doc_path = doc_path.lower()

    if doc_id is None and doc_path is None:
        return []

    if doc_id is None:
        doc = DocInfo.find_by_path(path=doc_path)
    else:
        doc = DocEntry.find_by_id(doc_id=doc_id)

    if doc is None:
        return []

    demo_paths = doc.document.get_settings().ide_course()

    if demo_paths is None:
        return []

    return demo_paths


def ide_tasks(user: User, doc_id: int = None, demo_path: str = None) -> json:
    """
    Get all TIDE-tasks from the demo folder
    :param user: Logged-in user
    :param doc_id:  Document id
    :param demo_path: Path to the demo folder
    :return: List JSON with all TIDE-tasks from the demo folder
    """

    if doc_id is None and demo_path is None:
        return []

    if doc_id is None:
        doc = DocInfo.find_by_path(path=demo_path.lower())
    else:
        doc = DocInfo.find_by_id(item_id=doc_id)

    if doc is None:
        return []

    user_ctx = UserContext.from_one_user(u=user)

    pars = doc.document.get_paragraphs()

    tasks = []

    for p in pars:
        if p.attrs.get(IDE_TASK_TAG) is not None:
            task = user_plugin_data(
                doc=doc, par=p, user_ctx=user_ctx, ide_task_id=p.attrs.get(IDE_TASK_TAG)
            )
            if task:
                tasks.append(task)
    return tasks


def ide_task_by_id(
        user: User,
        ide_task_id: str,
        doc_id: int = None,
        doc_path: str = None,
) -> json:
    """
    Get TIDE-task from the document by document id and paragraph id
    :param ide_task_id:  TIDE-task id
    :param user: Logged-in user
    :param doc_id:  Document id
    :param doc_path:  Document path
    :return: JSON with TIDE-task ide-files, task info and task id
    """
    if doc_id is None and doc_path is None:
        return []

    if doc_path is None:
        doc = DocInfo.find_by_id(item_id=doc_id)
    else:
        doc = DocInfo.find_by_path(path=doc_path.lower())

    if doc is None:
        return []

    user_ctx = UserContext.from_one_user(u=user)

    pars = doc.document.get_paragraphs()

    if pars is None:
        return []

    tasks = []

    for p in pars:
        if p.attrs.get(IDE_TASK_TAG) == ide_task_id:
            tasks.append(
                user_plugin_data(
                    doc=doc, par=p, user_ctx=user_ctx, ide_task_id=ide_task_id
                )
            )

    if len(tasks) == 0:
        return []

    if len(tasks) == 1:
        return tasks[0]

    # TODO: case where files are saved based on language base folders eg path is taken from the language package,

    # Combine all ide_files to one list
    all_ide_files = [task["ide_files"] for task in tasks]

    # Change the path of the files to main<index>.<type>, dummy solution for now
    for i in range(len(all_ide_files)):
        all_ide_files[i]["path"] = (
                "main" + str(i) + "." + all_ide_files[i]["path"].split(".")[-1]
        )

    total = tasks[0]
    total["ide_files"] = all_ide_files

    return total


def user_plugin_data(
        doc: DocInfo,
        par,
        ide_task_id: str,
        user_ctx: UserContext,
        plugin: Plugin = None,
) -> json:
    """
    Get the TIDE-task information from the plugin
    :param ide_task_id:  TIDE-task id
    :param plugin: Tim plugin
    :param doc: TIM document
    :param par: Paragraph from the document
    :param user_ctx: User context
    :return: JSON with TIDE-task ide-files, task info and task id
    """

    view_ctx = default_view_ctx

    if plugin is None:
        plugin = Plugin.from_paragraph(par, view_ctx, user_ctx)

    if plugin.type != "csPlugin":
        return []

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
        return []

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

    return {
        "task_files": json_ide_files,
        "header": task_info.header,
        "stem": task_info.stem,
        "type": task_info.type,
        "task_id": task_id.task_name,
        "doc_id": doc.id,
        "par_id": par.id,
        "ide_task_id": ide_task_id,
    }


def submit_task(
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

    return post_answer_impl(
        task_id_ext=task_id_ext,
        answerdata=answer_data,
        answer_browser_data=brow_data,
        answer_options={},
        curr_user=user,
        urlmacros=(),
        other_session_users=[],
        origin=None,
    )
