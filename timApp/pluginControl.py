# -*- coding: utf-8 -*-
from bs4 import BeautifulSoup
from containerLink import callPlugin
from containerLink import pluginReqs
from containerLink import getPluginTimUrl
import yaml
from htmlSanitize import sanitize_html
import json


def prepPluginCall(nodes):
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


def getPlugins(htmlStr):
    tree = BeautifulSoup(htmlStr, "html.parser")
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
def pluginify(blocks, user, answerDb, doc_id, user_id):
    preparedBlocks = []
    plugins = []
    for block in blocks:
        checkPlugin = getPlugins(block)
        if len(checkPlugin) > 0:
            pluginInfo = prepPluginCall(checkPlugin)
            pluginHtmls = []
            for vals in pluginInfo:
                if 'error' in vals:
                    pluginHtmls.append(getPluginErrorHtml(vals['plugin'], vals['error']))
                    continue
                try:
                    plugins.append(vals['plugin'])
                    vals['markup']["user_id"] = user
                    taskId = "{}.{}".format(doc_id, vals['taskId'])
                    states = answerDb.getAnswers(user_id, taskId)

                    # Don't show state for anonymous users.
                    state = None if user_id == 0 or len(states) == 0 else states[0]['content']
                    try:
                        if state is not None:
                            state = json.loads(state)
                    except ValueError:
                        pass
                    pluginHtml = callPlugin(vals['plugin'], vals['markup'], state, taskId)
                    pluginUrl = getPluginTimUrl(vals['plugin'])
                    pluginHtmls.append("<div id='{}' data-plugin='{}'>".format(taskId, pluginUrl) + pluginHtml + "</div>")
                except TypeError:
                    pluginHtmls.append(getPluginErrorHtml(vals['plugin'], "Unexpected error occurred while constructing plugin html. Please contact TIM-development team."))
                    continue
            preparedBlocks.append(''.join(pluginHtmls))
        else:
            preparedBlocks.append(sanitize_html(block))

    return plugins, preparedBlocks


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
    return (js,css, jsMods)


def removeDups(xs):
    us = []
    for x in xs:
        if x not in us:
            us.append(x)
    return us


def getPluginDatas(plugins):
    jsPaths = []
    cssPaths = []
    modules = []

    plugins = removeDups(plugins)
    for p in plugins:
        try:
            (rawJs,rawCss,modsList) = pluginDeps(json.loads(pluginReqs(p)))
            for src in rawJs:
                if( "http" in src):
                    jsPaths.append(src)
                else:
                    path = getPluginTimUrl(p) + "/" + src
                    if not path in jsPaths:
                        jsPaths.append(path)
            for cssSrc in rawCss:
                if( "http" in src):
                    if not cssSrc in cssPaths:
                        cssPaths.append(cssSrc)
                else:
                    path = getPluginTimUrl(p) + "/" + src
                    if not path in cssPaths:
                        cssPaths.append(path)
            for mod in modsList:
                modules.append(mod)
        except:
            print("Failed plugin call in plugincontrol getPluginDatas ")
            continue
    return (jsPaths, cssPaths, modules)





