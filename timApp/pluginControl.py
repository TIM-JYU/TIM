# -*- coding: utf-8 -*-
"""Functions for dealing with plugin paragraphs."""
from collections import OrderedDict
from copy import deepcopy
import json
import re

import yaml
from yaml import CLoader
import yaml.parser
import yaml.scanner
import time

from containerLink import call_plugin_html, call_plugin_multihtml, PluginException, PLUGINS
from containerLink import plugin_reqs
from containerLink import get_plugin_tim_url
from containerLink import get_plugin_needs_browser
from documentmodel.docparagraph import DocParagraph
from htmlSanitize import sanitize_html
from timdb.timdbbase import TimDbException
import utils

LAZYSTART= "<!--lazy "
LAZYEND = " lazy-->"
NOLAZY = "<!--nolazy-->"
NEVERLAZY = "NEVERLAZY"


def correct_yaml(text):
    """
      Inserts missing spaces after : Like  width:20 => width: 20
      Also gives an other way to write multiline attributes, by starting
      the multiline like: program: |!!  (!! could be any number and any non a-z,A-Z chars 
      and ending it by !! in first column

    :param text: text to convert proper yaml
    :return: text that is proper yaml
    :type text: str
    """
    lines = text.splitlines()
    s = ""
    p = re.compile("^[^ :]*:[^ ]")  # kissa:istuu
    pm = re.compile("^[^ :]+:[ ]*\|[ ]*[^ ]+[ ]*$")  # program: ||| or  program: |!!!
    multiline = False
    end_str = ''
    for line in lines:
        line = line.rstrip()
        if p.match(line) and not multiline:
            line = line.replace(':', ': ', 1)
        if pm.match(line):
            multiline = True
            line, end_str = line.split("|", 1)
            end_str = end_str.rstrip()
            s = s + line + "|\n"
            continue
        if multiline:
            if line == end_str:
                multiline = False
                continue
            line = " " + line
        s = s + line + "\n"
    return s


def parse_yaml(text):
    """

    :type text: str
    :return:
    """

    if len(text) == 0:
        return False
    try:
        text = correct_yaml(text)
        values = yaml.load(text, Loader=CLoader)
    except yaml.parser.ParserError as e:
        return str(e)
    except yaml.scanner.ScannerError as e:
        return str(e)
    try:
        if type(values) is str:
            return values
        else:
            return values
    except KeyError:
        return "Missing identifier"


def parse_plugin_values(par, global_attrs=None):
    """

    :type par: DocParagraph
    :return:
    :rtype: dict
    """
    try:
        # We get the yaml str by removing the first and last lines of the paragraph markup
        par_md = par.get_markdown()
        yaml_str = par_md[par_md.index('\n') + 1:par_md.rindex('\n')]
        values = parse_yaml(yaml_str)
        if type(values) is str:
            return {'error': "YAML is malformed: " + values}
        else:
            if global_attrs:
                if type(global_attrs) is str:
                    return {'error': 'global_plugin_attrs should be a dict, not str'}
                global_attrs = deepcopy(global_attrs)
                final_values = global_attrs.get('all', {})
                utils.merge(final_values, global_attrs.get(par.get_attrs()['plugin'], {}))
                utils.merge(final_values, values)
                values = final_values
            return {"markup": values}
    except Exception as e:
        return {'error': "Unknown error: " + str(e)}


def get_error_html(plugin_name, message):
    """

    :type message: str
    :type plugin_name: str
    """
    return '<div class="pluginError">Plugin {} error: {}</div>'.format(plugin_name, message)


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


def dereference_pars(pars, edit_window=False):
    """Resolves references in the given paragraphs.

    :type pars: list[DocParagraph]
    :param pars: The DocParagraphs to be processed.
    """
    new_pars = []
    for par in pars:
        if par.is_reference():
            try:
                new_pars += par.get_referenced_pars(edit_window=edit_window)
            except TimDbException as e:
                err_par = DocParagraph.create(
                    par.doc_id,
                    md=str(e),
                    html='<div class="pluginError">' + sanitize_html(str(e)) + '</div>')

                new_pars.append(err_par)
        else:
            new_pars.append(par)

    return new_pars


def pluginify(pars,
              user,
              answer_db,
              user_id,
              custom_state=None,
              sanitize=True,
              do_lazy=False,
              edit_window=False,
              settings=None):
    """ "Pluginifies" or sanitizes the specified DocParagraphs by calling the corresponding
        plugin route for each plugin paragraph.

    :param sanitize: Whether the blocks should be sanitized before processing.
    :param pars: A list of DocParagraphs to be processed.
    :param user: The current user's username.
    :param answer_db: A reference to the answer database.
    :param user_id: The user id.
    :param custom_state: Optional state that will used as the state for the plugin instead of answer database.
                         If this parameter is specified, the expression len(blocks) MUST be 1.
    :return: Processed HTML blocks along with JavaScript, CSS stylesheet and AngularJS module dependencies.

    :type pars: list[DocParagraph]
    """

    pars = dereference_pars(pars, edit_window)
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
            vals = parse_plugin_values(block, global_attrs=settings.global_plugin_attrs())
            if 'error' in vals:
                html_pars[idx]['html'] = get_error_html(plugin_name, vals['error'])
                continue

            if plugin_name not in plugins:
                plugins[plugin_name] = OrderedDict()
            vals['markup']["user_id"] = user
            task_id = "{}.{}".format(block.get_doc_id(), attr_taskId)

            if custom_state is not None:
                state = try_load_json(custom_state)
            else:
                state_map[task_id] = {'plugin_name': plugin_name, 'idx': idx}
                state = None
            plugins[plugin_name][idx] = {"markup": vals['markup'], "state": state, "taskID": task_id, "doLazy": do_lazy}

    if custom_state is None and user_id != 0:
        answers = answer_db.get_newest_answers(user_id, list(state_map.keys()))
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
                html_pars[idx]['html'] = get_error_html(plugin_name, str(e))
            continue
        try:
            reqs = json.loads(resp)
        except ValueError:
            for idx in plugin_block_map.keys():
                html_pars[idx]['html'] = get_error_html(plugin_name, 'Failed to parse JSON from plugin reqs route.')
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
                response = call_plugin_multihtml(plugin_name, json.dumps([val for _, val in plugin_block_map.items()]))
            except PluginException as e:
                for idx in plugin_block_map.keys():
                    html_pars[idx]['html'] = get_error_html(plugin_name, str(e))
                continue
            try:
                plugin_htmls = json.loads(response)
            except ValueError:
                for idx in plugin_block_map.keys():
                    html_pars[idx]['html'] = get_error_html(plugin_name, 'Failed to parse plugin response from reqs route.')
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
                    html_pars[idx]['html'] = get_error_html(plugin_name, str(e))
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
