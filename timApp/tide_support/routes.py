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


def get_user_tasks_by_bookmarks(user: User) -> list[json]:
    """
    Get all TIDE-tasks from the courses user has bookmarked
    :param user: Logged-in user
    :return: List JSON with all TIDE-tasks from the courses user has bookmarked
    """
    user_courses = user.bookmarks.bookmark_data[2]["My courses"]

    user_plugin_datas = []

    for course_dict in user_courses:
        for course_name, course_path in course_dict.items():
            # Get the document by path, remove /view/ from the beginning of path
            doc = DocEntry.find_by_path(course_path.split("view/")[1:])

            # Get all paragraphs from the document
            pars = doc.document.get_paragraphs()

            for p in pars:
                if p.attrs["plugin"] == "csPlugin" and p.attrs.get("tide") == "true":
                    # Checks the plugin type to be csPlugin TODO: additional check for TIDE-task, now only with tide attribute
                    user_plugin_datas.append(
                        get_user_plugin_data(
                            doc,
                            p,
                            UserContext.from_one_user(user),
                        )
                    )

    return user_plugin_datas


def get_task_by_id(doc_id: int, par_id: str, user: User) -> json:
    """
    Get the TIDE-task by task id
    :param user: Authenticated user
    :param par_id: id of the paragraph
    :param doc_id: Document id
    :return:
    """

    user_ctx = UserContext.from_one_user(user)
    doc = DocEntry.find_by_id(doc_id=doc_id)
    par = doc.document.get_paragraph(par_id=par_id)

    return get_user_plugin_data(doc=doc, par=par, user_ctx=user_ctx)


def get_user_plugin_data(
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

    # If the plugin doesn't have files
    if plugin_json["markup"].get("files"):
        ide_files = IdeFileSchema.load(
            plugin_json["markup"]["files"], many=True, unknown=EXCLUDE
        )
    # If the plugin has only one file TODO: check if this is correct
    else:
        ide_files = IdeFileSchema.load(plugin_json, unknown=EXCLUDE)

    return {
        "ide_files": ide_files,
        "task_info": task_info,
        "task_id": task_id,
        "document_id": doc.id,
        "paragraph_id": par.id,
    }
