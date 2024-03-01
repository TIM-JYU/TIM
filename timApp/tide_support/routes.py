import base64
import json
from dataclasses import dataclass

from bs4 import BeautifulSoup
from marshmallow import EXCLUDE

from timApp.document.docentry import DocEntry
from timApp.document.docinfo import DocInfo
from timApp.document.usercontext import UserContext
from timApp.document.viewcontext import default_view_ctx
from timApp.plugin.containerLink import render_plugin_multi
from timApp.plugin.plugin import Plugin, PluginRenderOptions, PluginWrap
from timApp.plugin.pluginOutputFormat import PluginOutputFormat
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
            "code": self.by or self.byCode,
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


TIDETaskInfoSchema = class_schema(TIDETaskInfo)()


def user_ide_courses(user: User, attr_name) -> list[json]:
    """
    Gets all courses that have a tasks with TIDE-parameter and are bookmarked by the user
    :param attr_name: Name of the attribute that identifies the TIDE-task
    :param user: Logged-in user
    :return: List JSON with all TIDE-tasks from the courses user has bookmarked
    """
    user_courses = user.bookmarks.bookmark_data[2]["My courses"]

    ide_courses = []

    for course_dict in user_courses:
        for course_name, course_path in course_dict.items():
            # Get the document by path, remove /view/ from the beginning of path
            doc = DocEntry.find_by_path(course_path.split("view/")[1:])
            # TODO: Does this work with multiple subfolders
            # Get all paragraphs from the document
            pars = doc.document.get_paragraphs()
            if any(p.attrs.get(attr_name) is not None for p in pars):
                ide_courses.append(
                    {"course_name": course_name, "course_id": doc.document.doc_id}
                )

    return ide_courses


def ide_tasks_from_document(document_id: int, attr_name, user) -> json:
    """
    Get all TIDE-tasks from the courses user has bookmarked
    :param attr_name:
    :param document_id: Document id
    :param user: Logged-in user
    :return: List JSON with all TIDE-tasks from the courses user has bookmarked
    """
    doc = DocEntry.find_by_id(doc_id=document_id)
    # TODO: Should here be a test if the user has right to access the document?
    pars = doc.document.get_paragraphs()
    ide_task_ids = []

    for p in pars:
        if p.attrs.get(attr_name) is not None:
            ide_task_ids.append(p.attrs.get(attr_name))

    return {"ide_task_ids": ide_task_ids, "doc.id": doc.id}


def ide_task_by_id(user: User, ide_task_id: str, doc_id: int, attr_name: str) -> json:
    """
    Get the TIDE-task by task id
    :param attr_name: Name of the attribute that identifies the ide-task
    :param ide_task_id: Ide-task id
    :param user: Authenticated user
    :param doc_id: Document id
    :return:
    """

    user_ctx = UserContext.from_one_user(u=user)
    doc = DocEntry.find_by_id(doc_id=doc_id)
    pars = doc.document.get_paragraphs()

    tasks = []

    for par in pars:
        if par.attrs.get(attr_name) == ide_task_id:
            tasks.append(user_plugin_data(doc=doc, par=par, user_ctx=user_ctx))

    # TODO: Multiple tasks with the same id -> Combine code files for the same task
    # TODO: Paths according the language
    return tasks


def user_plugin_data(
        doc: DocInfo, par, user_ctx: UserContext, plugin: Plugin = None
) -> json:
    """
    Get the TIDE-task information from the plugin
    :param plugin: Tim plugin
    :param doc: TIM document
    :param par: Paragraph from the document
    :param user_ctx: User context
    :return: JSON with TIDE-task ide-files, task info and task id
    """

    view_ctx = default_view_ctx

    # Get the plugin from the paragraph
    if plugin is None:
        plugin = Plugin.from_paragraph(par, view_ctx, user_ctx)

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

    task_id = plugin_json["taskID"]

    # If the plugin has files attribute
    if plugin_json["markup"].get("files"):
        ide_files = IdeFileSchema.load(
            plugin_json["markup"]["files"], many=True, unknown=EXCLUDE
        )
    # If the plugin has only one file TODO: check if this is correct
    else:
        ide_files = IdeFileSchema.load(plugin_json, unknown=EXCLUDE)

    return {
        "ide_files": ide_files.to_json(),
        "task_info": task_info,
        "task_id": task_id,
        "document_id": doc.id,
        "paragraph_id": par.id,
    }
