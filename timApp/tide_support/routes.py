import base64
import json
from dataclasses import dataclass

from bs4 import BeautifulSoup
from marshmallow import EXCLUDE

from timApp.bookmark.bookmarks import MY_COURSES_GROUP
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

            if demo_paths is None:
                continue

            ide_courses.append(
                {
                    "course_name": course_name,
                    "course_id": doc.document.doc_id,
                    "course_path": course_path,
                    "demo_paths": demo_paths,
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


def ide_tasks(
        user: User, ide_task_tag: str, doc_id: int = None, demo_path: str = None
) -> json:
    """
    Get all TIDE-tasks from the demo folder
    :param user: Logged-in user
    :param doc_id:  Document id
    :param ide_task_tag: Tag that identifies the TIDE-task
    :param demo_path: Path to the demo folder
    :return: List JSON with all TIDE-tasks from the demo folder
    """
    # TODO: Should here be a test if the user has right to access the document?

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
        if p.attrs.get(ide_task_tag) is not None:
            tasks.append(
                user_plugin_data(
                    doc=doc,
                    par=p,
                    user_ctx=user_ctx,
                    ide_task_id=p.attrs.get(ide_task_tag),
                )
            )

    return tasks


def ide_task_by_id(
        user: User,
        ide_task_id: str,
        ide_task_tag: str,
        doc_id: int = None,
        doc_path: str = None,
) -> json:
    """
    Get TIDE-task from the document by document id and paragraph id
    :param ide_task_tag: Tag that identifies the TIDE-task
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

    if len(pars) == 1:
        if pars[0].attrs.get(ide_task_tag) == ide_task_id:
            return user_plugin_data(doc=doc, par=pars[0], user_ctx=user_ctx)
        else:
            return []

    task = []

    for par_id, par in pars:
        if par.attrs.get(ide_task_tag) == ide_task_id:
            task.append(user_plugin_data(doc=doc, par=par, user_ctx=user_ctx))

    # TODO: support if there are multiple tasks with the same id

    return task


def user_plugin_data(
        doc: DocInfo,
        par,
        user_ctx: UserContext,
        plugin: Plugin = None,
        ide_task_id: str = None,
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
        "ide_task_id": ide_task_id,
    }


def is_ide_course_by_tag(tag_name: str, doc: DocInfo):
    """
    :param tag_name: Name of the tag that identifies the TIDE-course
    :param doc:  Document that is checked
    :return: True if the document has the tag that identifies the TIDE-course
    TODO: This is probably not needed
    """
    # Get the tags from the document
    tags = doc.block.tags

    # If the document has the tag that identifies the TIDE-task
    return any(tag_name in str(tag) for tag in tags)
