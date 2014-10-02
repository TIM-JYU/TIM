# -*- coding: utf-8 -*-
from bs4 import BeautifulSoup
from containerLink import callPlugin, callPluginMultiHtml, PluginException
from containerLink import pluginReqs
from containerLink import getPluginTimUrl
from collections import OrderedDict
import yaml
from htmlSanitize import sanitize_html
import json


def parse_plugin_values(nodes):
    plugins = []
    for node in nodes:
        values = {}
        name = node['plugin']

        if len(node.text) > 0:
            try:
                values = yaml.load(node.text)
            except (yaml.parser.ParserError, yaml.scanner.ScannerError):
                plugins.append({"plugin": name, 'error': "YAML is malformed"})
        try:
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


def getBlockYaml(block):
    tree = BeautifulSoup(block)
    for node in tree.find_all('pre'):
        values = {}
        if len(node.text) > 0:
            try:
                values = yaml.load(node.text)
            except (yaml.parser.ParserError, yaml.scanner.ScannerError):
                print("Malformed yaml string")
                return "YAMLERROR: Malformed string"
    return values


def getPluginErrorHtml(pluginName, message):
    return '<div class="pluginError">Plugin {} error: {}</div>'.format(pluginName, message)


# Take a set of blocks and search for plugin markers,
# replace contents with plugin.
def pluginify(blocks, user, answer_db, doc_id, user_id):
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
                    error_messages.append(getPluginErrorHtml(plugin_name, vals['error']))
                    continue
                try:
                    if not plugin_name in plugins:
                        plugins[plugin_name] = OrderedDict()
                    vals['markup']["user_id"] = user
                    task_id = "{}.{}".format(doc_id, vals['taskId'])
                    states = answer_db.getAnswers(user_id, task_id)

                    # Don't show state for anonymous users.
                    state = None if user_id == 0 or len(states) == 0 else states[0]['content']
                    try:
                        if state is not None:
                            state = json.loads(state)
                    except ValueError:
                        pass
                    plugins[plugin_name][idx] = {"markup": vals['markup'], "state": state, "taskID": task_id}
                except TypeError:
                    error_messages.append(getPluginErrorHtml(plugin_name, "Unexpected error occurred while constructing plugin html. Please contact TIM-development team."))
                    continue
            final_html_blocks.append('<div>Error(s) occurred while rendering plugin. </div>' + ''.join(error_messages))  # Add empty block at this point
        else:
            final_html_blocks.append(sanitize_html(block))

    js_paths = []
    css_paths = []
    modules = []

    for plugin_name, plugin_block_map in plugins.items():
        try:
            resp = pluginReqs(plugin_name)
        except PluginException:
            continue
        reqs = json.loads(resp)
        plugin_js_files, plugin_css_files, plugin_modules = pluginDeps(reqs)
        for src in plugin_js_files:
            # TODO: Better check for absolute URL.
            if "http" in src:
                js_paths.append(src)
            else:
                path = getPluginTimUrl(plugin_name) + "/" + src
                js_paths.append(path)
        for cssSrc in plugin_css_files:
            if "http" in src:
                css_paths.append(cssSrc)
            else:
                path = getPluginTimUrl(plugin_name) + "/" + src
                css_paths.append(path)
        for mod in plugin_modules:
            modules.append(mod)

        # Remove duplicates, preserving order
        js_paths = list(OrderedDict.fromkeys(js_paths))
        css_paths = list(OrderedDict.fromkeys(css_paths))
        modules = list(OrderedDict.fromkeys(modules))

        plugin_url = getPluginTimUrl(plugin_name)

        if 'multihtml' in reqs and reqs['multihtml']:
            response = callPluginMultiHtml(plugin_name, json.dumps([val for _, val in plugin_block_map.items()]))
            plugin_htmls = json.loads(response)
            for idx, markup, html in zip(plugin_block_map.keys(), plugin_block_map.values(), plugin_htmls):
                final_html_blocks[idx] = "<div id='{}' data-plugin='{}'>".format(markup['taskID'], plugin_url) + html + "</div>"
        else:
            for idx, val in plugin_block_map.items():
                html = callPlugin(plugin_name, val['markup'], val['state'], val['taskID'])
                final_html_blocks[idx] = "<div id='{}' data-plugin='{}'>".format(val['taskID'], plugin_url) + html + "</div>"

    return final_html_blocks, js_paths, css_paths, modules


# p is json of plugin requirements in form:
# {"js": ["js.js"], "css":["css.css"], "angularModule":["module"]}
def pluginDeps(p):
    js = []
    jsMods = []
    css = [] 
    if "css" in p:
        for cssF in p['css']:
            css.append(cssF)
    if "js" in p:
        for jsF in p['js']:
            js.append(jsF)
    if "angularModule" in p:
        for ng in p['angularModule']:
            jsMods.append(ng)
    return js, css, jsMods
