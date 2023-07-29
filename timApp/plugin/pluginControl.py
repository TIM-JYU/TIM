"""Functions for dealing with plugin paragraphs."""
import json
from collections import OrderedDict, defaultdict
from dataclasses import dataclass
from itertools import chain
from typing import Optional, Union, DefaultDict
from xml.sax.saxutils import quoteattr

import attr
import yaml
import yaml.parser
from sqlalchemy import func, select

from timApp.answer.answer import Answer
from timApp.answer.answers import valid_answers_query, valid_taskid_filter
from timApp.auth.accesshelper import has_edit_access, verify_view_access
from timApp.document.docentry import DocEntry
from timApp.document.docparagraph import DocParagraph
from timApp.document.docsettings import DocSettings
from timApp.document.document import Document
from timApp.document.macroinfo import MacroInfo
from timApp.document.randutils import hashfunc
from timApp.document.usercontext import UserContext
from timApp.document.viewcontext import ViewContext
from timApp.document.yamlblock import YamlBlock
from timApp.plugin.containerLink import plugin_reqs, get_plugin
from timApp.plugin.containerLink import render_plugin_multi, render_plugin, get_plugins
from timApp.plugin.plugin import (
    Plugin,
    PluginRenderOptions,
    load_markup_from_yaml,
    expand_macros_for_plugin,
    find_inline_plugins,
    InlinePlugin,
    finalize_inline_yaml,
    PluginWrap,
    WANT_FIELDS,
    find_task_ids,
    get_simple_hash_from_par_and_user,
    expand_macros_for_plugin_attrs,
)
from timApp.plugin.pluginOutputFormat import PluginOutputFormat
from timApp.plugin.pluginexception import PluginException
from timApp.plugin.taskid import TaskId
from timApp.printing.printsettings import PrintFormat
from timApp.timdb.sqa import db
from timApp.user.user import User
from timApp.util.get_fields import (
    get_fields_and_users,
    RequestedGroups,
    GetFieldsAccess,
)
from timApp.util.rndutils import SeedClass
from timApp.util.timtiming import taketime
from timApp.util.utils import (
    get_error_tex,
    Range,
    get_error_html_block,
    get_error_html,
)
from tim_common.dumboclient import call_dumbo
from tim_common.html_sanitize import sanitize_html


def get_error_plugin(
    plugin_name,
    message,
    response=None,
    plugin_output_format: PluginOutputFormat = PluginOutputFormat.HTML,
    inline=False,
):
    """

    :param response:
    :type message: str
    :type plugin_name: str
    """
    error_message = "Plugin " + (f"{plugin_name} " if plugin_name else "") + "error:"
    if plugin_output_format == PluginOutputFormat.MD:
        return get_error_tex(error_message, message)

    return (
        get_error_html_block(error_message, message, response)
        if not inline
        else get_error_html(f"{error_message} {message}", response)
    )


PluginOrError = Union[Plugin, str]  # str represent HTML markup of error
AnswerMap = dict[str, tuple[Answer, int]]
ErrorMap = dict[Range, tuple[str, str]]


@attr.s
class PluginPlacement:
    """Represents the position(s) of plugin(s) in a block.

    Can be either:
     * a block-level (traditional) plugin, or
     * one or more inlineplugins.

    In case of a block-level plugin, the range spans the entire block's expanded markdown.
    """

    plugins: dict[Range, Plugin] = attr.ib(kw_only=True)  # ordered

    errors: ErrorMap = attr.ib(kw_only=True)  # ordered
    block: DocParagraph = attr.ib(kw_only=True)
    """The block where the plugins are."""

    expanded_md: str = attr.ib(kw_only=True)
    """Expanded markdown of the containing block."""

    is_block_plugin: bool = attr.ib(kw_only=True)
    """Whether this is a block-level plugin."""

    output_format: PluginOutputFormat = attr.ib(kw_only=True)

    def get_block_plugin(self):
        if not self.is_block_plugin:
            return None
        try:
            return next(iter(self.plugins.values()))
        except StopIteration:
            return None

    def set_error(self, r: Range, err: str):
        p = self.plugins.pop(r)
        self.errors[r] = err, p.type

    def set_output(self, r: Range, out: str):
        self.plugins[r].set_output(out)

    def get_block_output(
        self, extract_plugins: bool = False
    ) -> tuple[str, dict[str, str]]:
        """TODO: this did not help very much
        if self.is_block_plugin:
            idx = next(iter(self.plugins))
            p = self.plugins[idx]
            return p.get_final_output().strip()
        """
        sorted_ranges = sorted(
            chain(self.plugins.keys(), self.errors.keys()),
            key=lambda r: r[0],
            reverse=True,
        )
        block_codes: dict[str, str] = {}
        out_md = self.expanded_md
        for sr in sorted_ranges:
            p = self.plugins.get(sr)
            if not p:
                err, name = self.errors[sr]
                h = get_error_plugin(
                    name,
                    err,
                    plugin_output_format=self.output_format,
                    inline=extract_plugins,
                )
            else:
                h = (
                    p.get_final_output().strip()
                )  # allow inlineplugins to come close each other
            if extract_plugins:
                # We can't pass plugin HTML directly into Dumbo
                # because Pandoc does not know how to parse custom HTML elements
                # Instead we temporarily encode the plugin HTML into
                # a special code that is passed through Pandoc unchanged
                # We pick a special prefix that is far enough from
                # anything that Pandoc could process in a special manner
                h_code = f"§§plugin_html_{hashfunc(h)}"
                block_codes[h_code] = h
                h = h_code
            start, end = sr
            out_md = out_md[:start] + h + out_md[end:]
        return out_md, block_codes

    @staticmethod
    def from_par(
        block: DocParagraph,
        load_states: bool,
        macroinfo: MacroInfo,
        plugin_opts: PluginRenderOptions,
        user_ctx: UserContext,
        view_ctx: ViewContext,
        settings: DocSettings,
        answer_map: AnswerMap,
        custom_answer: Answer | None,
        output_format: PluginOutputFormat,
    ) -> Optional["PluginPlacement"]:
        plugin_name = block.get_attr("plugin")
        defaultplugin = block.get_attr("defaultplugin")
        if not plugin_name and not defaultplugin:
            return None
        new_seed = False
        rnd_seed = None
        answer_and_cnt = None
        ask_next = False

        if rnd_seed is None:
            rnd_seed = get_simple_hash_from_par_and_user(
                block,
                user_ctx,
            )  # TODO: RND_SEED: get users seed for this plugin

            # TODO: if possible to look from markup newtask: true, this is not needed
            if block.is_new_task():
                if block.answer_nr is not None and not block.ask_new:
                    rnd_seed = SeedClass(rnd_seed, block.answer_nr)
                else:  # try with length of answers
                    task_id = block.get_attr("taskId")
                    doc_id = str(block.doc.doc_id)
                    if task_id:
                        answer_and_cnt = answer_map.get(doc_id + "." + task_id, None)
                        if answer_and_cnt:
                            cnt = answer_and_cnt[1]
                            if cnt > 0:
                                rnd_seed = SeedClass(rnd_seed, cnt)
                                ask_next = True
            new_seed = True

        rnd_error = None
        try:
            if (
                block.insert_rnds(rnd_seed) and new_seed
            ):  # do not change order!  inserts must be done
                # TODO: RND_SEED save rnd_seed to user data
                pass
        except ValueError as e:
            rnd_error = str(e)

        errs = OrderedDict()
        plugs = OrderedDict()
        is_block_plugin = bool(plugin_name)
        if rnd_error:
            md = block.get_expanded_markdown(macroinfo)
            errs[0, len(md)] = rnd_error, plugin_name or defaultplugin
        elif plugin_name:
            # We want the expanded markdown here, so can't call Plugin.from_paragraph[_macros] directly.
            macros = macroinfo.get_macros()
            md = expand_macros_for_plugin(block, macros, macroinfo.jinja_env)
            p_range = 0, len(md)
            try:
                vals = load_markup_from_yaml(
                    md, settings.global_plugin_attrs(), block.get_attr("plugin")
                )
                if ask_next:
                    block.ask_new = True
                    if vals.get("initNewAnswer", None) == "":
                        load_states = False

                if plugin_name in WANT_FIELDS and "fields" in vals and user_ctx:
                    data, aliases, field_names, _ = get_fields_and_users(
                        vals["fields"],
                        RequestedGroups([user_ctx.user.get_personal_group()]),
                        block.doc.docinfo,
                        user_ctx.logged_user,
                        view_ctx,
                        add_missing_fields=True,
                        access_option=GetFieldsAccess.from_bool(
                            True
                        ),  # TODO: the user selected from User list
                    )
                    df = data[0]["fields"]
                    da = []
                    labels = []
                    for fn in field_names:
                        da.append(df.get(fn, 0))
                        labels.append(fn)
                    vals["fielddata"] = {
                        "data": data[0]["fields"],
                        "aliases": aliases,
                        "fieldnames": field_names,
                        "graphdata": {"data": da, "labels": labels},
                    }

            except PluginException as e:
                errs[p_range] = str(e), plugin_name
            else:
                taskid = block.get_attr("taskId")
                try:
                    tid = (
                        TaskId.parse(
                            taskid, require_doc_id=False, allow_block_hint=False
                        )
                        if taskid
                        else None
                    )
                except PluginException as e:
                    errs[p_range] = str(e), plugin_name
                else:
                    if check_task_access(errs, p_range, plugin_name, tid):
                        try:
                            plugs[p_range] = Plugin(
                                tid,
                                vals,
                                plugin_name,
                                par=block,
                            )
                        except PluginException as e:
                            errs[p_range] = str(e), plugin_name
        else:
            md = None
            for task_id, p_yaml, p_range, md in find_inline_plugins(block, macroinfo):
                plugin_type = ""
                try:
                    task_id = task_id.validate()
                    plugin_type = task_id.plugin_type or defaultplugin
                    y = load_markup_from_yaml(
                        finalize_inline_yaml(p_yaml),
                        settings.global_plugin_attrs(),
                        plugin_type,
                    )
                except PluginException as e:
                    errs[p_range] = str(e), plugin_type
                    continue
                if not check_task_access(errs, p_range, plugin_type, task_id):
                    continue
                try:
                    plug = InlinePlugin(
                        task_id=task_id,
                        values=y,
                        plugin_type=plugin_type,
                        p_range=p_range,
                        par=block,
                    )
                except PluginException as e:
                    errs[p_range] = str(e), plugin_type
                    continue
                plugs[p_range] = plug
        if md is None:
            # Can happen if inline plugin block has no plugins.
            md = block.get_expanded_markdown(macroinfo)
        for p in plugs.values():
            if p.type == "qst":
                p.values["isTask"] = not block.is_question()

            if load_states:
                if (
                    custom_answer is not None
                    and custom_answer.task_id == p.task_id.doc_task
                ):
                    answer_and_cnt = custom_answer, custom_answer.get_answer_number()
                elif p.task_id:
                    answer_and_cnt = answer_map.get(p.task_id.doc_task, None)

            p.set_render_options(
                answer_and_cnt if load_states and answer_and_cnt is not None else None,
                plugin_opts,
            )
        return PluginPlacement(
            block=block,
            errors=errs,
            expanded_md=md,
            plugins=plugs,
            is_block_plugin=is_block_plugin,
            output_format=output_format,
        )


def check_task_access(errs: ErrorMap, p_range: Range, plugin_name: str, tid: TaskId):
    if tid and tid.doc_id:
        b = DocEntry.find_by_id(tid.doc_id)
        if b:
            has_access = verify_view_access(b, require=False)
            if not has_access:
                errs[p_range] = (
                    "Task id refers to another document, "
                    "but you do not have access to that document."
                ), plugin_name
                return False
        else:
            errs[p_range] = "Task id refers to a non-existent document.", plugin_name
            return False
    return True


KeyType = tuple[int, Range]


def get_answers(user: User, task_ids, answer_map):
    col = func.max(Answer.id).label("col")
    cnt = func.count(Answer.id).label("cnt")
    if user is None:
        sub = (
            valid_answers_query(task_ids)
            .add_columns(col, cnt)
            .with_only_columns(col, cnt)
            .group_by(Answer.task_id)
            .subquery()
        )
    else:
        sub = (
            user.answers.filter(valid_taskid_filter(task_ids))
            .add_columns(col, cnt)
            .with_entities(col, cnt)
            .group_by(Answer.task_id)
            .subquery()
        )
    answers: list[tuple[Answer, int]] = db.session.execute(
        select(Answer)
        .join(sub, Answer.id == sub.c.col)
        .with_only_columns(Answer, sub.c.cnt)
    ).all()
    for answer, cnt in answers:
        answer_map[answer.task_id] = answer, cnt
    return cnt, answers


@dataclass
class PluginifyResult:
    pars: list[DocParagraph]
    js_paths: list[str]
    css_paths: list[str]
    custom_answer_plugin: Plugin | None
    all_plugins: list[Plugin]
    has_errors: bool


def pluginify(
    doc: Document,
    pars: list[DocParagraph],
    user_ctx: UserContext,
    view_ctx: ViewContext,
    custom_answer: Answer | None = None,
    task_id: TaskId | None = None,
    sanitize=True,
    do_lazy=False,
    load_states=True,
    review=False,
    pluginwrap=PluginWrap.Full,
    output_format: PluginOutputFormat = PluginOutputFormat.HTML,
    user_print: bool = False,
    target_format: PrintFormat = PrintFormat.LATEX,
    protect_raw_inline_plugins: bool = False,
) -> PluginifyResult:
    """
    "Pluginifies" the specified DocParagraphs by calling the corresponding plugin route for each plugin
    paragraph.

    :param view_ctx: The view context.
    :param doc: Document / DocumentVersion object.
    :param pars: A list of DocParagraphs to be processed.
    :param user_ctx: The user context.
    :param custom_answer: Optional answer that will used as the state for the plugin instead of answer database.
    :param task_id: Optional taskId for plugin which will load it's current state (returned as custom_answer_plugin)
        If custom_answer or task_id is specified, the expression len(blocks) MUST be 1.
    :param sanitize: Whether the blocks should be sanitized before processing.
    :param do_lazy: Whether to use lazy versions of the plugins.
    :param output_format: Desired output format (html/md) for plugins
    :param user_print: Whether the plugins should output the original values or user's input (when exporting markdown).
    :param target_format: for MD-print what exact format to use
    :param protect_raw_inline_plugins: If true, protect inline plugins from being processed by macros
                                       by wrapping them into a raw block.
    :return: Processed HTML blocks along with JavaScript and CSS stylesheet dependencies.
    """

    taketime("answ", "start")
    if not view_ctx.preview and has_edit_access(doc.get_docinfo()):
        for p in pars:
            if p.is_translation_out_of_date():
                p.add_class("troutofdate")
            else:
                if p.is_translation_unchecked():
                    p.add_class("checktr")
    if sanitize:
        for par in pars:
            par.sanitize_html()

    # init these for performance as they stay the same for all pars
    md_out = output_format == PluginOutputFormat.MD
    html_out = False if md_out else (output_format == PluginOutputFormat.HTML)

    # We need to expand macros for plugin paragraph/block attributes before calling DocParagraph.prepare,
    # otherwise some (s)css classes will not be set and the plugins will not render correctly
    mi = doc.get_settings().get_macroinfo(view_ctx, user_ctx)
    for par in pars:
        if par.is_plugin():
            expand_macros_for_plugin_attrs(par, mi.get_macros(), mi.jinja_env)

    html_pars = [par.prepare(view_ctx, use_md=md_out) for par in pars]

    if custom_answer is not None or task_id is not None:
        if len(pars) != 1:
            raise PluginException("len(blocks) must be 1 if custom state is specified")
    plugins: DefaultDict[str, dict[KeyType, Plugin]] = defaultdict(OrderedDict)

    answer_map: AnswerMap = {}
    plugin_opts = PluginRenderOptions(
        do_lazy=do_lazy,
        user_print=user_print,
        preview=view_ctx.preview,
        target_format=target_format,
        output_format=output_format,
        user_ctx=user_ctx,
        review=review,
        wraptype=pluginwrap,
        viewmode=view_ctx.viewmode,
    )

    if load_states and custom_answer is None and user_ctx.user.logged_in:
        # TODO: could this return also the plugins, then there is no need for other iteration
        task_ids, _, _ = find_task_ids(
            pars, view_ctx, user_ctx, check_access=user_ctx.is_different
        )
        get_answers(user_ctx.user, task_ids, answer_map)
        # get_answers(User.get_by_id(user_ctx.user.id), task_ids, answer_map)
        # db.session.close()
        # TODO: RND_SEED get all users rand_seeds for this doc's tasks. New table?

    placements = {}
    dumbo_opts = OrderedDict()
    custom_answer_plugin = None
    has_errors = False
    for idx, block in enumerate(pars):
        is_gamified = block.get_attr("gamification")
        is_gamified = not not is_gamified
        settings = block.doc.get_settings()
        macroinfo = settings.get_macroinfo(view_ctx, user_ctx=user_ctx)

        if is_gamified:
            md = block.get_expanded_markdown(macroinfo=macroinfo)
            try:
                # TODO: Gamification map should be its own plugin
                gd = YamlBlock.from_markdown(md).values
                runner = "gamification-map"
                html_pars[
                    idx
                ].output = f"<{runner} data={quoteattr(json.dumps(gd))}></{runner}>"
            except yaml.YAMLError as e:
                has_errors = True
                html_pars[idx].output = (
                    '<div class="error"><p>Gamification error:</p><pre>'
                    + str(e)
                    + "</pre><p>From block:</p><pre>"
                    + md
                    + "</pre></div>"
                )
        pplace = PluginPlacement.from_par(
            block=block,
            load_states=load_states,
            macroinfo=macroinfo,
            plugin_opts=plugin_opts,
            user_ctx=user_ctx,
            view_ctx=view_ctx,
            settings=settings,
            answer_map=answer_map,
            custom_answer=custom_answer,
            output_format=output_format,
        )
        if pplace:
            placements[idx] = pplace
            for r, p in pplace.plugins.items():
                plugins[p.type][idx, r] = p
                if (custom_answer and p.task_id.doc_task == custom_answer.task_id) or (
                    task_id and p.task_id.doc_task == task_id
                ):
                    custom_answer_plugin = p
            if not pplace.is_block_plugin:
                dumbo_opts[idx] = block.get_dumbo_options(
                    base_opts=settings.get_dumbo_options()
                )
        else:
            if block.nocache and not is_gamified:  # get_nocache():
                # if block.get_nocache():
                texts = [block.get_expanded_markdown(macroinfo)]
                htmls = call_dumbo(
                    texts,
                    options=block.get_dumbo_options(
                        base_opts=settings.get_dumbo_options()
                    ),
                )
                html_pars[idx].output = sanitize_html(
                    htmls[0]
                )  # to collect all together before dumbo

                # taketime("answ", "markup", len(plugins))

    js_paths = []
    css_paths = []

    # TODO: Get plugin values before 1st answer query and loop for special cases
    #  (these tasks could have been omitted from 1st answer query)
    glb_task_ids = []
    glb_plugins_to_change = []
    curruser_task_ids = []
    curruser_plugins_to_change = []
    taketime("glb/ucu", "GLO/currUser")
    for plugin_name, plugin_block_map in plugins.items():
        for _, plugin in plugin_block_map.items():
            plugin.values.pop("postprogram", None)
            plugin.values.pop("preprogram", None)
            plugin.values.pop("postProgram", None)  # Used by some plugins
            plugin.values.pop("modelAnswer", None)
            if not plugin.task_id:
                continue
            if plugin.task_id.is_global and not custom_answer:
                glb_task_ids.append(plugin.task_id)
                glb_plugins_to_change.append(plugin)
            elif plugin.known.useCurrentUser and user_ctx.is_different:
                curruser_task_ids.append(plugin.task_id)
                curruser_plugins_to_change.append(plugin)
    if glb_task_ids:
        get_answers(None, glb_task_ids, answer_map)
        for p in glb_plugins_to_change:
            a = answer_map.get(p.task_id.doc_task, None)
            if not a:
                continue
            p.answer = a[0]
            p.answer_count = a[1]
    if curruser_task_ids:
        for tid in curruser_task_ids:
            answer_map.pop(tid.doc_task, None)
        get_answers(user_ctx.logged_user, curruser_task_ids, answer_map)
        for p in curruser_plugins_to_change:
            p.options.user_ctx = UserContext.from_one_user(user_ctx.logged_user)
            a = answer_map.get(p.task_id.doc_task, None)
            if not a:
                p.answer = None
                p.answer_count = None
                continue
            p.answer = a[0]
            p.answer_count = a[1]
            # p.options.__setattr__("user", current_user)

    taketime("glb/ucu", "done")
    settings = doc.get_settings()
    all_plugins = []
    for plugin_name, plugin_block_map in plugins.items():
        taketime("plg", plugin_name)
        try:
            plugin = get_plugin(plugin_name)
            plugin_lazy = plugin.lazy

            plugin_block_map_vals = [*plugin_block_map.values()]
            for p in plugin_block_map_vals:
                all_plugins.append(p)

            resp = plugin_reqs(plugin_name)
        except PluginException as e:
            has_errors = True
            for idx, r in plugin_block_map.keys():
                placements[idx].set_error(r, str(e))
            continue
        # taketime("plg e", plugin_name)
        try:
            reqs = json.loads(resp)
            plugin.can_give_task = reqs.get("canGiveTask", False)
            if plugin_name == "mmcq" or plugin_name == "mcq":
                reqs["multihtml"] = True
                reqs["multimd"] = True
        except ValueError as e:
            has_errors = True
            for idx, r in plugin_block_map.keys():
                placements[idx].set_error(
                    r, f"Failed to parse JSON from plugin reqs route: {e}"
                )
            continue
        plugin_js_files, plugin_css_files = plugin_deps(reqs)
        for src in plugin_js_files:
            if src.startswith("http") or src.startswith("/"):  # absolute URL
                js_paths.append(src)
            elif src.endswith(".js"):  # relative JS URL
                js_paths.append(f"/{plugin_name}/{src}")
            else:  # module name
                js_paths.append(src)
        for src in plugin_css_files:
            if src.startswith("http") or src.startswith("/"):
                css_paths.append(src)
            else:
                css_paths.append(f"/{plugin_name}/{src}")

        # Remove duplicates, preserving order TODO: could this be done out of the loop?
        # taketime("rmv", "Remove dupl")
        js_paths = list(OrderedDict.fromkeys(js_paths))
        css_paths = list(OrderedDict.fromkeys(css_paths))

        default_auto_md = reqs.get("default_automd", False)

        if (html_out and reqs.get("multihtml")) or (md_out and reqs.get("multimd")):
            try:
                # taketime("plg m", plugin_name)
                response = render_plugin_multi(
                    settings,
                    plugin_name,
                    list(plugin_block_map.values()),
                    plugin_output_format=output_format,
                    default_auto_md=default_auto_md,
                )
                taketime("plg e", plugin_name)
            except PluginException as e:
                has_errors = True
                for idx, r in plugin_block_map.keys():
                    placements[idx].set_error(r, str(e))
                continue
            try:
                plugin_htmls = json.loads(response)
            except ValueError as e:
                has_errors = True
                for idx, r in plugin_block_map.keys():
                    placements[idx].set_error(
                        r, f"Failed to parse plugin response from multihtml route: {e}"
                    )
                continue
            if not isinstance(plugin_htmls, list):
                for (idx, r), plugin in plugin_block_map.items():
                    plugin.plugin_lazy = plugin_lazy
                    placements[idx].set_error(
                        r,
                        f"Multihtml response of {plugin_name} was not a list: {plugin_htmls}",
                    )
            else:
                for ((idx, r), plugin), html in zip(
                    plugin_block_map.items(), plugin_htmls
                ):
                    plugin.plugin_lazy = plugin_lazy
                    placements[idx].set_output(r, html)
        else:
            for (idx, r), plugin in plugin_block_map.items():
                if md_out:
                    err_msg_md = (
                        "Plugin does not support printing yet. "
                        "Please refer to TIM help pages if you want to learn how you can manually "
                        "define what to print here."
                    )
                    placements[idx].set_error(r, err_msg_md)
                else:
                    try:
                        html = render_plugin(
                            docsettings=settings,
                            plugin=plugin,
                            output_format=output_format,
                        )
                    except PluginException as e:
                        has_errors = True
                        placements[idx].set_error(r, str(e))
                        continue
                    placements[idx].set_output(r, html)
    taketime("plg m", "Plugins done")

    taketime("plc", "Placement start")

    for idx, place in placements.items():
        par = html_pars[idx]
        pass_to_dumbo = idx in dumbo_opts
        output, plugin_htmls = place.get_block_output(extract_plugins=pass_to_dumbo)
        par.output = output
        if pass_to_dumbo:
            par.plugin_htmls = plugin_htmls

    taketime("plc", "Placement done")

    # inline plugin blocks need to go through Dumbo to process MD
    if output_format == PluginOutputFormat.HTML:
        htmls_to_dumbo = []
        settings_to_dumbo = []
        taketime("dumbo", "start 1")
        for k, v in dumbo_opts.items():
            htmls_to_dumbo.append({"content": html_pars[k].output, **v.dict()})
            settings_to_dumbo.append(v)
        taketime("dumbo", "start 2")
        for h, (idx, s) in zip(
            call_dumbo(htmls_to_dumbo, options=doc.get_settings().get_dumbo_options()),
            dumbo_opts.items(),
        ):
            par = html_pars[idx]
            for plugin_key, plugin_html in par.plugin_htmls.items():
                h = h.replace(plugin_key, plugin_html)
            par.plugin_htmls = None
            par.output = sanitize_html(h)
    elif output_format == PluginOutputFormat.MD:
        # No dumbo, just insert raw MD
        for par in html_pars:
            if par.plugin_htmls:
                for plugin_key, plugin_html in par.plugin_htmls.items():
                    if protect_raw_inline_plugins:
                        plugin_html = f"{{% raw %}}{plugin_html}{{% endraw %}}"
                    par.output = par.output.replace(plugin_key, plugin_html)

    taketime("phtml done")

    return PluginifyResult(
        pars=pars,
        js_paths=js_paths,
        css_paths=css_paths,
        custom_answer_plugin=custom_answer_plugin,
        all_plugins=all_plugins,
        has_errors=has_errors,
    )


def get_all_reqs():
    allreqs = {}
    for plugin, vals in get_plugins().items():
        if vals.skip_reqs:
            continue
        try:
            resp = plugin_reqs(plugin)
        except PluginException:
            continue
        try:
            reqs = json.loads(resp)
            allreqs[plugin] = reqs
        except ValueError:
            continue
    return allreqs


def plugin_deps(p: dict) -> tuple[list[str], list[str]]:
    """

    :param p: is json of plugin requirements of the form:
              {"js": ["js.js"], "css":["css.css"]}
    """
    js_files = []
    css_files = []
    if "css" in p:
        for cssF in p["css"]:
            css_files.append(cssF)
    if "js" in p:
        for jsF in p["js"]:
            js_files.append(jsF)
    return js_files, css_files
