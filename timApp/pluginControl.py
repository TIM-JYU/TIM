# -*- coding: utf-8 -*-
"""Functions for dealing with plugin paragraphs."""
import json
from collections import OrderedDict
from typing import List, Tuple, Optional, Dict

import yaml
import yaml.parser
from flask import render_template

from timApp.containerLink import PLUGINS, render_plugin_multi, render_plugin
from timApp.containerLink import get_plugin_needs_browser
from timApp.containerLink import get_plugin_tim_url
from timApp.containerLink import plugin_reqs
from timApp.documentmodel.docparagraph import DocParagraph
from timApp.documentmodel.document import dereference_pars, Document
from timApp.plugin import PluginException, Plugin
from timApp.pluginOutputFormat import PluginOutputFormat
from timApp.timdb import gamificationdata
from timApp.timdb.models.user import User
from timApp.utils import get_error_html, get_error_md
from timApp.rndutils import get_simple_hash_from_par_and_user
from timApp.timdb.printsettings import PrintFormat
from timApp.dumboclient import call_dumbo
from timApp.timtiming import taketime

LAZYSTART = "<!--lazy "
LAZYEND = " lazy-->"
NOLAZY = "<!--nolazy-->"
NEVERLAZY = "NEVERLAZY"


def get_error_plugin(plugin_name, message, response=None,
                     plugin_output_format: PluginOutputFormat = PluginOutputFormat.HTML):
    """

    :param response:
    :type message: str
    :type plugin_name: str
    """
    if plugin_output_format == PluginOutputFormat.MD:
        return get_error_md('Plugin {} error:'.format(plugin_name), message, response)

    return get_error_html('Plugin {} error: {}'.format(plugin_name, message), response)


def find_task_ids(blocks: List[DocParagraph]) -> Tuple[List[str], int]:
    """Finds all task plugins from the given list of paragraphs and returns their ids."""
    task_ids = []
    plugin_count = 0
    for block in blocks:
        task_id = block.get_attr('taskId')
        plugin = block.get_attr('plugin')
        if plugin:
            plugin_count += 1
        # Need "and plugin" to ignore e.g. manual heading ids
        if task_id and plugin:
            task_ids.append("{}.{}".format(block.doc.doc_id, task_id))
    return task_ids, plugin_count


def try_load_json(json_str: str):
    """"""
    try:
        if json_str is not None:
            return json.loads(json_str)
        return None
    except ValueError:
        return json_str


def pluginify(doc: Document,
              pars,
              user: Optional[User],
              timdb,
              custom_answer=None,
              sanitize=True,
              do_lazy=False,
              edit_window=False,
              load_states=True,
              plugin_params=None,
              wrap_in_div=True,
              output_format: PluginOutputFormat = PluginOutputFormat.HTML,
              user_print: bool = False,
              target_format: str='latex',
              dereference=True):
    """"Pluginifies" or sanitizes the specified DocParagraphs by calling the corresponding plugin route for each plugin
    paragraph.

    :param doc Document / DocumentVersion object.
    :param pars: A list of DocParagraphs to be processed.
    :param user: The current user object.
    :param timdb: A reference to the database.
    :param custom_answer: Optional answer that will used as the state for the plugin instead of answer database.
    If this parameter is specified, the expression len(blocks) MUST be 1.
    :param sanitize: Whether the blocks should be sanitized before processing.
    :param do_lazy Whether to use lazy versions of the plugins.
    :param edit_window Whether the method is called from the edit window or not.
    :param output_format: Desired output format (html/md) for plugins
    :param user_print: Whether the plugins should output the original values or user's input (when exporting markdown).
    :param target_format: for MD-print what exact format to use
    :param dereference: should pars be checked id dereference is needed
    :return: Processed HTML blocks along with JavaScript, CSS stylesheet and AngularJS module dependencies.

    :type pars: list[DocParagraph]

    """

    taketime("answ", "start")
    if dereference:
        pars = dereference_pars(pars, source_doc=doc.get_original_document())
    if sanitize:
        for par in pars:
            par.sanitize_html()

    # init these for performance as they stay the same for all pars
    md_out = (output_format == PluginOutputFormat.MD)
    html_out = False if md_out else (output_format == PluginOutputFormat.HTML)

    html_pars = [par.html_dict(use_md=md_out) for par in pars]

    # taketime("answ", "sansitize")

    if custom_answer is not None:
        if len(pars) != 1:
            raise PluginException('len(blocks) must be 1 if custom state is specified')
    plugins = {}
    state_map = {}
    task_ids = []

    global_attrs = doc.get_settings().global_plugin_attrs()
    macroinfo = doc.get_settings().get_macroinfo(user)
    macros = macroinfo.get_macros()
    macro_delimiter = macroinfo.get_macro_delimiter()

    answer_map = {}
    # enum_pars = enumerate(pars)

    if load_states and custom_answer is None and user is not None:
        for idx, block in enumerate(pars):  # find taskid's
            attr_taskid = block.get_attr('taskId')
            plugin_name = block.get_attr('plugin')
            if plugin_name and not block.is_question():  # show also question in preview
                task_id = "{}.{}".format(block.get_doc_id(), attr_taskid or '')
                if not task_id.endswith('.'):
                    task_ids.append(task_id)
        answers = timdb.answers.get_newest_answers(user.id, task_ids)
        # TODO: RND_SEED get all users rand_seeds for this doc's tasks. New table?
        # Close database here because we won't need it for a while
        timdb.close()
        for answer in answers:
            answer_map[answer['task_id']] = answer

    for idx, block in enumerate(pars):
        attr_taskid = block.get_attr('taskId')
        plugin_name = block.get_attr('plugin')
        is_gamified = block.get_attr('gamification')

        if is_gamified:
            md = block.get_markdown()
            try:
                gamified_data = gamificationdata.gamify(md)
                html_pars[idx][output_format.value] = render_template('partials/gamification_map.html',
                                                                      gamified_data=gamified_data)
            except yaml.parser.ParserError as e:
                html_pars[idx][output_format.value] = '<div class="error"><p>Gamification error:</p><pre>' + \
                                                      str(e) + \
                                                      '</pre><p>From block:</p><pre>' + \
                                                      md + \
                                                      '</pre></div>'

        if plugin_name and not block.is_question():  # show also question in preview
            task_id = "{}.{}".format(block.get_doc_id(), attr_taskid or '')
            info = None
            state_ok = False
            new_seed = False
            rnd_seed = None
            answer = {}

            if load_states:
                if custom_answer is not None:
                    answer = custom_answer
                else:
                    answer = answer_map.get(task_id, None)
                if answer is not None:
                    state = try_load_json(answer['content'])
                    rnd_seed = answer.get('rndseed', None)
                    state_ok = True

            if rnd_seed is None:
                rnd_seed = get_simple_hash_from_par_and_user(block, user) # TODO: RND_SEED: get users seed for this plugin
                new_seed = True

            if block.insert_rnds(rnd_seed) and new_seed:  # do not change order!  inserts must be done
                # TODO: RND_SEED save rnd_seed to user data
                pass

            try:
                # plugin = Plugin.from_paragraph(block, user)
                joint_macros = macros
                rands = block.get_rands()
                if rands:
                    joint_macros = {**macros, **rands}
                plugin = Plugin.from_paragraph_macros(block, global_attrs, joint_macros, macro_delimiter)
                plugin.values['isQuestion'] = block.get_attr('isQuestion', '')
            except PluginException as e:
                html_pars[idx][output_format.value] = get_error_plugin(plugin_name, str(e),
                                                                       plugin_output_format=output_format)
                continue
            vals = plugin.values
            if plugin_name not in plugins:
                plugins[plugin_name] = OrderedDict()
            vals["user_id"] = user.name if user is not None else 'Anonymous'

            if state_ok:
                info = plugin.get_info([user], old_answers=answer.get('cnt'), valid=answer['valid'])
            else:
                if not task_id.endswith('.'):
                    state_map[task_id] = {'plugin_name': plugin_name, 'idx': idx, 'obj': plugin}
                state = None

            plugins[plugin_name][idx] = {"markup": vals,
                                         "state": state,
                                         "taskID": task_id,
                                         "taskIDExt": task_id + '.' + block.get_id(),
                                         "doLazy": do_lazy,
                                         "userPrint": user_print,
                                         # added preview here so that whether or not the window is in preview can be
                                         # checked in python so that decisions on what data is sent can be made.
                                         "preview": edit_window,
                                         "anonymous": user is not None,
                                         "info": info,
                                         "targetFormat": target_format}
        else:
            if block.nocache:  # get_nocache():
                # if block.get_nocache():
                texts = [block.get_expanded_markdown(macroinfo)]
                htmls = call_dumbo(texts)
                html_pars[idx][output_format.value] = htmls[0]  # to collect all together before dumbo

                # taketime("answ", "markup", len(plugins))
    '''
    if load_states and custom_answer is None and user is not None:
        answers = timdb.answers.get_newest_answers(user.id, list(state_map.keys()))
        # Close database here because we won't need it for a while
        timdb.close()
        for answer in answers:
            state = try_load_json(answer['content'])
            p = state_map[answer['task_id']]
            plugins[p['plugin_name']][p['idx']]['state'] = state
            plugins[p['plugin_name']][p['idx']]['info'] = p['obj'].get_info([user],
                                                                            old_answers=answer['cnt'],
                                                                            valid=answer['valid'])
    '''
    js_paths = []
    css_paths = []
    modules = []

    taketime("answ", "done", len(answers))

    for plugin_name, plugin_block_map in plugins.items():
        taketime("plg", plugin_name)
        try:
            resp = plugin_reqs(plugin_name)
        except PluginException as e:
            for idx in plugin_block_map.keys():
                html_pars[idx][output_format.value] = get_error_plugin(plugin_name, str(e),
                                                                       plugin_output_format=output_format)
            continue
        # taketime("plg e", plugin_name)
        try:
            reqs = json.loads(resp)
            if plugin_name == 'mmcq' or plugin_name == 'mcq':
                reqs['multihtml'] = True
                reqs['multimd'] = True
        except ValueError as e:
            for idx in plugin_block_map.keys():
                html_pars[idx][output_format.value] = get_error_plugin(
                    plugin_name, 'Failed to parse JSON from plugin reqs route: {}'.format(e), resp,
                    plugin_output_format=output_format)
            continue
        plugin_js_files, plugin_css_files, plugin_modules = plugin_deps(reqs)
        for src in plugin_js_files:
            if src.startswith("http") or src.startswith("/"):  # absolute URL
                js_paths.append(src)
            elif src.endswith('.js'):  # relative JS URL
                path = get_plugin_tim_url(plugin_name) + "/" + src
                js_paths.append(path)
            else:  # module name
                js_paths.append(src)
        for src in plugin_css_files:
            if src.startswith("http") or src.startswith("/"):
                css_paths.append(src)
            else:
                path = get_plugin_tim_url(plugin_name) + "/" + src
                css_paths.append(path)
        for mod in plugin_modules:
            modules.append(mod)

        # Remove duplicates, preserving order
        js_paths = list(OrderedDict.fromkeys(js_paths))
        css_paths = list(OrderedDict.fromkeys(css_paths))
        modules = list(OrderedDict.fromkeys(modules))

        plugin_url = get_plugin_tim_url(plugin_name)
        needs_browser = get_plugin_needs_browser(plugin_name)
        if (html_out and 'multihtml' in reqs and reqs['multihtml']) or \
                (md_out and 'multimd' in reqs and reqs['multimd']):
            try:
                # taketime("plg m", plugin_name)
                response = render_plugin_multi(
                    doc,
                    plugin_name,
                    [val for _, val in plugin_block_map.items()],
                    plugin_params,
                    plugin_output_format=(output_format))
                # taketime("plg e", plugin_name)
            except PluginException as e:
                for idx in plugin_block_map.keys():
                    html_pars[idx][output_format.value] = get_error_plugin(plugin_name, str(e),
                                                                           plugin_output_format=output_format)
                continue
            try:
                plugin_htmls = json.loads(response)
            except ValueError as e:
                for idx in plugin_block_map.keys():
                    html_pars[idx][output_format.value] = \
                        get_error_plugin(plugin_name,
                                         'Failed to parse plugin response from multihtml route: {}'.format(e),
                                         response, plugin_output_format=output_format)
                continue

            for idx, markup, html in zip(plugin_block_map.keys(), plugin_block_map.values(), plugin_htmls):
                html, is_lazy = make_lazy(html, markup, do_lazy)

                html_pars[idx]['needs_browser'] = needs_browser or is_lazy
                html_pars[idx][output_format.value] = ("<div id='{}' data-plugin='{}'>{}</div>"
                                                       .format(markup['taskIDExt'],
                                                               plugin_url,
                                                               html)) if wrap_in_div else html
        else:
            for idx, val in plugin_block_map.items():
                if md_out:
                    err_msg_md = "Plugin does not support printing yet. " \
                                 "Please refer to TIM help pages if you want to learn how you can manually " \
                                 "define what to print here."
                    html_pars[idx][output_format.value] = get_error_plugin(plugin_name,
                                                                           err_msg_md,
                                                                           plugin_output_format=output_format)
                else:
                    try:
                        html = render_plugin(doc=doc,
                                             plugin=plugin_name,
                                             plugin_data=val,
                                             params=plugin_params,
                                             output_format=output_format)
                    except PluginException as e:
                        html_pars[idx][output_format.value] = get_error_plugin(plugin_name, str(e),
                                                                               plugin_output_format=output_format)
                        continue

                    html, is_lazy = make_lazy(html, val, do_lazy)
                    html_pars[idx]['needs_browser'] = needs_browser or is_lazy
                    html_pars[idx]['html'] = ("<div id='{}' data-plugin='{}'>{}</div>"
                                              .format(val['taskIDExt'],
                                                      plugin_url,
                                                      html)) if wrap_in_div else html

    # taketime("phtml done")

    return html_pars, js_paths, css_paths, modules


def get_markup_value(markup, key, default):
    if key not in markup["markup"]:
        return default
    return markup["markup"][key]


def make_lazy(html, markup, do_lazy):
    if do_lazy == NEVERLAZY:
        return html, False
    markup_lazy = get_markup_value(markup, "lazy", "")
    if markup_lazy == False:
        return html, False  # user do not want lazy
    if not do_lazy and markup_lazy != True:
        return html, False
    if html.find(NOLAZY) >= 0:
        return html, False  # not allowed to make lazy
    if html.find(LAZYSTART) >= 0:
        return html, True  # allredy lazy
    header = str(get_markup_value(markup, "header", get_markup_value(markup, "headerText", "")))
    stem = str(get_markup_value(markup, "stem", "Open plugin"))
    html = html.replace("<!--", "<!-LAZY-").replace("-->", "-LAZY->")
    # print(header, stem)
    return LAZYSTART + html + LAZYEND + '<span style="font-weight:bold">' + header + '</span>' + \
           "<div><p>" + stem + "</p></div>", True


def get_all_reqs():
    allreqs = {}
    for plugin, vals in PLUGINS.items():
        if vals.get('skip_reqs', False):
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


def plugin_deps(p: Dict) -> Tuple[List[str], List[str], List[str]]:
    """

    :param p: is json of plugin requirements of the form:
              {"js": ["js.js"], "css":["css.css"], "angularModule":["module"]}
    """
    js_files = []
    modules = []
    css_files = []
    if "css" in p:
        for cssF in p['css']:
            css_files.append(cssF)
    if "js" in p:
        for jsF in p['js']:
            js_files.append(jsF)
    if "angularModule" in p:
        for ng in p['angularModule']:
            modules.append(ng)
    return js_files, css_files, modules
