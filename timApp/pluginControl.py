# -*- coding: utf-8 -*-
"""Functions for dealing with plugin paragraphs."""
import json
import time
from collections import OrderedDict

from containerLink import call_plugin_html, call_plugin_multihtml, PLUGINS
from containerLink import get_plugin_needs_browser
from containerLink import get_plugin_tim_url
from containerLink import plugin_reqs
from documentmodel.docparagraph import DocParagraph
from documentmodel.document import dereference_pars
from plugin import PluginException, parse_plugin_values
from utils import get_error_html

LAZYSTART= "<!--lazy "
LAZYEND = " lazy-->"
NOLAZY = "<!--nolazy-->"
NEVERLAZY = "NEVERLAZY"


def get_error_html_plugin(plugin_name, message):
    """

    :type message: str
    :type plugin_name: str
    """
    return get_error_html('Plugin {} error: {}'.format(plugin_name, message))


def find_task_ids(blocks, doc_id):
    """

    :rtype: list[str]
    :type doc_id: int
    :type blocks: list[DocParagraph]
    """
    task_ids = []
    for block in blocks:
        task_id = block.get_attr('taskId')
        if task_id:
            task_ids.append("{}.{}".format(doc_id, task_id))
    return task_ids


def try_load_json(json_str):
    """

    :rtype : dict|list
    :type json_str: str
    """
    try:
        if json_str is not None:
            return json.loads(json_str)
        return None
    except ValueError:
        return json_str


def pluginify(doc,
              pars,
              user,
              answer_db,
              custom_state=None,
              sanitize=True,
              do_lazy=False,
              edit_window=False):
    """ "Pluginifies" or sanitizes the specified DocParagraphs by calling the corresponding
        plugin route for each plugin paragraph.

    :param doc Document / DocumentVersion object.
    :param pars: A list of DocParagraphs to be processed.
    :param user: The current user object.
    :param answer_db: A reference to the answer database.
    :param custom_state: Optional state that will used as the state for the plugin instead of answer database.
                         If this parameter is specified, the expression len(blocks) MUST be 1.
    :param sanitize: Whether the blocks should be sanitized before processing.
    :param do_lazy Whether to use lazy versions of the plugins.
    :param edit_window Whether the method is called from the edit window or not.
    :return: Processed HTML blocks along with JavaScript, CSS stylesheet and AngularJS module dependencies.

    :type pars: list[DocParagraph]
    """

    settings = doc.get_settings()
    pars = dereference_pars(pars, edit_window, source_doc=doc.get_original_document())
    if sanitize:
        for par in pars:
            par.sanitize_html()

    html_pars = [par.html_dict() for par in pars]

    if custom_state is not None:
        if len(pars) != 1:
            raise PluginException('len(blocks) must be 1 if custom state is specified')
    plugins = {}
    state_map = {}
    for idx, block in enumerate(pars):
        attr_taskId = block.get_attr('taskId')
        plugin_name = block.get_attr('plugin')

        if attr_taskId and plugin_name:
            vals = parse_plugin_values(block, global_attrs=settings.global_plugin_attrs(),
                                       macros=settings.get_macros(),
                                       macro_delimiter=settings.get_macro_delimiter())
            if 'error' in vals:
                html_pars[idx]['html'] = get_error_html_plugin(plugin_name, vals['error'])
                continue

            if plugin_name not in plugins:
                plugins[plugin_name] = OrderedDict()
            vals['markup']["user_id"] = user['name'] if user is not None else 'Anonymous'
            task_id = "{}.{}".format(block.get_doc_id(), attr_taskId)

            if custom_state is not None:
                state = try_load_json(custom_state)
            else:
                state_map[task_id] = {'plugin_name': plugin_name, 'idx': idx}
                state = None
            plugins[plugin_name][idx] = {"markup": vals['markup'],
                                         "state": state,
                                         "taskID":task_id,
                                         "doLazy": do_lazy,
                                         "anonymous": user is not None}

    if custom_state is None and user is not None:
        answers = answer_db.get_newest_answers(user['id'], list(state_map.keys()))
        for answer in answers:
            state = try_load_json(answer['content'])
            map_entry = state_map[answer['task_id']]
            plugins[map_entry['plugin_name']][map_entry['idx']]['state'] = state

    js_paths = []
    css_paths = []
    modules = []

    t22 = time.clock()
    # print("%-15s %-10s %6d - %7.4f" % ("answ done", " ", len(answers), (t22-t12)))
    t12 = t22

    for plugin_name, plugin_block_map in plugins.items():
        try:
            resp = plugin_reqs(plugin_name)
        except PluginException as e:
            for idx in plugin_block_map.keys():
                html_pars[idx]['html'] = get_error_html_plugin(plugin_name, str(e))
            continue
        try:
            reqs = json.loads(resp)
        except ValueError:
            for idx in plugin_block_map.keys():
                html_pars[idx]['html'] = get_error_html_plugin(plugin_name, 'Failed to parse JSON from plugin reqs route.')
            continue
        plugin_js_files, plugin_css_files, plugin_modules = plugin_deps(reqs)
        for src in plugin_js_files:
            if src.startswith("http") or src.startswith("/"):
                js_paths.append(src)
            else:
                path = get_plugin_tim_url(plugin_name) + "/" + src
                js_paths.append(path)
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

        if 'multihtml' in reqs and reqs['multihtml']:
            try:
                response = call_plugin_multihtml(plugin_name, [val for _, val in plugin_block_map.items()])
            except PluginException as e:
                for idx in plugin_block_map.keys():
                    html_pars[idx]['html'] = get_error_html_plugin(plugin_name, str(e))
                continue
            try:
                plugin_htmls = json.loads(response)
            except ValueError:
                for idx in plugin_block_map.keys():
                    html_pars[idx]['html'] = get_error_html_plugin(plugin_name, 'Failed to parse plugin response from reqs route.')
                continue

            needs_browser = get_plugin_needs_browser(plugin_name)
            for idx, markup, html in zip(plugin_block_map.keys(), plugin_block_map.values(), plugin_htmls):
                html, is_lazy = make_lazy(html, markup, do_lazy)

                html_pars[idx]['needs_browser'] = needs_browser or is_lazy
                html_pars[idx]['html'] = "<div id='{}' data-plugin='{}'>{}</div>".format(markup['taskID'],
                                                                             plugin_url,
                                                                             html)
        else:
            for idx, val in plugin_block_map.items():
                try:
                    html = call_plugin_html(plugin_name, val['markup'], val['state'], val['taskID'])
                except PluginException as e:
                    html_pars[idx]['html'] = get_error_html_plugin(plugin_name, str(e))
                    continue
                html, is_lazy = make_lazy(html, val, do_lazy)
                needs_browser = get_plugin_needs_browser(plugin_name)
                html_pars[idx]['needs_browser'] = needs_browser or is_lazy
                html_pars[idx]['html'] = "<div id='{}' data-plugin='{}'>{}</div>".format(val['taskID'],
                                                                             plugin_url,
                                                                             html)

    return html_pars, js_paths, css_paths, modules

def get_markup_value(markup, key, default):
    if key not in markup["markup"]: return default
    return markup["markup"][key]


def make_lazy(html, markup, do_lazy):
    if do_lazy == NEVERLAZY: return html, False
    markup_lazy = get_markup_value(markup,"lazy", "")
    if markup_lazy == False: return html, False # user do not want lazy
    if not do_lazy and markup_lazy != True: return html, False
    if html.find(NOLAZY) >= 0: return html, False  # not allowed to make lazy
    if html.find(LAZYSTART) >= 0: return html, True # allredy lazy
    header = str(get_markup_value(markup, "header", "Check your understanding"))
    stem = str(get_markup_value(markup, "stem", "Open plugin"))
    html = html.replace("<!--", "<!-LAZY-").replace("-->", "-LAZY->")
    # print(header, stem)
    return LAZYSTART + html + LAZYEND + '<span style="font-weight:bold">' + header + '</span>' + "<div><p>" + stem + "</p></div>", True


def get_all_reqs():
    allreqs = {}
    for plugin in PLUGINS.keys():
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


def plugin_deps(p):
    """

    :param p: is json of plugin requirements of the form:
              {"js": ["js.js"], "css":["css.css"], "angularModule":["module"]}
    :rtype : tuple[list,list,list]
    :type p: dict
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
