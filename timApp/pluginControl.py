# -*- coding: utf-8 -*-
from bs4 import BeautifulSoup
from containerLink import call_plugin_html, call_plugin_multihtml, PluginException
from containerLink import plugin_reqs
from containerLink import get_plugin_tim_url
from collections import OrderedDict
import yaml
from yaml import CLoader
import yaml.parser
import yaml.scanner
from htmlSanitize import sanitize_html
import json
import html


def parse_plugin_values(nodes):
    plugins = []
    for node in nodes:
        values = {}
        name = node['plugin']

        if len(node.text) > 0:
            try:
                values = yaml.load(node.text, Loader=CLoader)
            except (yaml.parser.ParserError, yaml.scanner.ScannerError):
                plugins.append({"plugin": name, 'error': "YAML is malformed"})
        try:
            if type(values) is str:
                plugins.append({"plugin": name, 'error': "YAML is malformed"})
            else:
                plugins.append({"plugin": name, "markup": values, "taskId": node['id']})
        except KeyError:
            plugins.append({"plugin": name, 'error': "Missing identifier"})
    return plugins


def find_plugins(html_str):
    if not html_str.startswith('<pre id="'):
        return []
    if not ' plugin="' in html_str:
        return []

    tree = BeautifulSoup(html_str, "lxml")
    pres = tree.find_all('pre')
    plugins = []
    for pre in pres:
        if pre.has_attr('plugin'):
            plugins.append(pre)
    return plugins


def get_block_yaml(block):
    tree = BeautifulSoup(block)
    values = None
    for node in tree.find_all('pre'):
        if len(node.text) > 0:
            try:
                values = yaml.load(node.text, Loader=CLoader)
            except (yaml.parser.ParserError, yaml.scanner.ScannerError):
                print("Malformed yaml string")
                return "YAMLERROR: Malformed string"
    return values


def get_error_html(plugin_name, message):
    return '<div class="pluginError">Plugin {} error: {}</div>'.format(plugin_name, message)

def find_task_ids(blocks, doc_id):
    task_ids = []
    final_html_blocks = []
    plugins = {}
    for idx, block in enumerate(blocks):
        found_plugins = find_plugins(block)
        if len(found_plugins) > 0:
            plugin_info = parse_plugin_values(found_plugins)
            error_messages = []
            for vals in plugin_info:
                plugin_name = vals['plugin']
                if 'error' in vals:
                    error_messages.append(get_error_html(plugin_name, vals['error']))
                    continue

                if not plugin_name in plugins:
                    plugins[plugin_name] = OrderedDict()
                task_ids.append("{}.{}".format(doc_id, vals['taskId']))
    
    return task_ids


def pluginify(blocks, user, answer_db, doc_id, user_id, custom_state=None):
    """ "Pluginifies" or sanitizes the specified HTML blocks by inspecting each block
    for plugin markers, calling the corresponding plugin route if such is found. Sanitizes HTML
    for each non-plugin block.

    :param blocks: A list of HTML blocks to be processed.
    :param user: The current user's username.
    :param answer_db: A reference to the answer database.
    :param doc_id: The document id.
    :param user_id: The user id.
    :param custom_state: Optional state that will used as the state for the plugin instead of answer database.
                         If this parameter is specified, the expression len(blocks) MUST be 1.
    :return: Processed HTML blocks along with JavaScript, CSS stylesheet and AngularJS module dependencies.
    """

    if custom_state is not None:
        if len(blocks) != 1:
            raise PluginException('len(blocks) must be 1 if custom state is specified')
    final_html_blocks = []
    plugins = {}
    for idx, block in enumerate(blocks):
        found_plugins = find_plugins(block)
        if len(found_plugins) > 0:
            assert len(found_plugins) == 1
            plugin_info = parse_plugin_values(found_plugins)
            error_messages = []
            for vals in plugin_info:
                plugin_name = vals['plugin']
                if 'error' in vals:
                    error_messages.append(get_error_html(plugin_name, vals['error']))
                    continue

                if plugin_name not in plugins:
                    plugins[plugin_name] = OrderedDict()
                vals['markup']["user_id"] = user
                task_id = "{}.{}".format(doc_id, vals['taskId'])
                states = answer_db.getAnswers(user_id, task_id)

                if custom_state is not None:
                    state = custom_state
                else:
                    # Don't show state for anonymous users.
                    state = None if user_id == 0 or len(states) == 0 else states[0]['content']
                try:
                    if state is not None:
                        state = json.loads(state)
                except ValueError:
                    pass
                plugins[plugin_name][idx] = {"markup": vals['markup'], "state": state, "taskID": task_id}

                final_html_blocks.append({'html': '<div class="pluginError">'
                                                  'Error(s) occurred while rendering plugin.'
                                                  '</div>'
                                                  ''.join(error_messages),
                                          'task_id': task_id})
                break  # Assuming there is at most one plugin per paragraph
        else:
            final_html_blocks.append({'html': sanitize_html(block)})

    js_paths = []
    css_paths = []
    modules = []

    for plugin_name, plugin_block_map in plugins.items():
        try:
            resp = plugin_reqs(plugin_name)
        except PluginException as e:
            for idx in plugin_block_map.keys():
                final_html_blocks[idx]['html'] = get_error_html(plugin_name, str(e))
            continue
        try:
            reqs = json.loads(resp)
        except ValueError:
            for idx in plugin_block_map.keys():
                final_html_blocks[idx]['html'] = get_error_html(plugin_name,
                                                                'Failed to parse JSON from plugin reqs route.')
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
                    final_html_blocks[idx]['html'] = get_error_html(plugin_name, str(e))
                continue
            plugin_htmls = json.loads(response)
            for idx, markup, html in zip(plugin_block_map.keys(), plugin_block_map.values(), plugin_htmls):
                final_html_blocks[idx]['html'] = "<div id='{}' data-plugin='{}'>{}</div>".format(markup['taskID'],
                                                                                         plugin_url,
                                                                                         html)
        else:
            for idx, val in plugin_block_map.items():
                try:
                    html = call_plugin_html(plugin_name, val['markup'], val['state'], val['taskID'])
                except PluginException as e:
                    final_html_blocks[idx]['html'] = get_error_html(plugin_name, str(e))
                    continue
                final_html_blocks[idx]['html'] = "<div id='{}' data-plugin='{}'>{}</div>".format(val['taskID'],
                                                                                         plugin_url,
                                                                                         html)

    return final_html_blocks, js_paths, css_paths, modules


def make_browse_buttons(user_id, task_id, answer_db):
    states = answer_db.getAnswers(user_id, task_id)
    if len(states) > 1:
        formatted = ""
        content_obj = json.loads(states[len(states)-1]["content"])
        if isinstance(content_obj, dict):
            for key, val in content_obj.items():
                formatted += key + "\n---------------\n" + str(val) + "\n\n"
        elif isinstance(content_obj, list):
            for v in content_obj:
                formatted += "List element:" + "\n---------------\n" + str(v) + "\n\n"
        else:
            formatted = str(content_obj)
        first = "<br/>First answer:<br/><pre>{}</pre>".format(html.escape(formatted))
    else:
        first = ""
    return """
       <div class="answerbuttons">
           <input type="button" value="<-">
           {} / {}
           <input type="button" value="->">
           {}
       </div>
    """.format(len(states), len(states), first)

# p is json of plugin requirements of the form:
# {"js": ["js.js"], "css":["css.css"], "angularModule":["module"]}
def plugin_deps(p):
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
