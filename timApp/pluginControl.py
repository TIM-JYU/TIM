from bs4 import BeautifulSoup
from containerLink import callPlugin
from containerLink import pluginReqs
from containerLink import getPlugin
import yaml
import re
import json

# Receive html-string with plugin in it, 
# extract values from contents
#def prepPluginCall(htmlStr):
#    tree = BeautifulSoup(htmlStr)
#    plugins = []
#    for node in tree.find_all('pre'):
#        values = {}
#        name = node['plugin']
#        try:
#            values["identifier"] = node['id']
#        except KeyError:
#            values['identifier'] = " "
#        if(len(node.text) > 0):
#            multiLineId = ""
#            multiLineVal = ""
#            multiLineCont = False
#            for value in node.string.strip().split('\n'):
#                if("=====" in value):
#                    values[multiLineId] = multiLineVal
#                    multiLineId = ""
#                    multiLineVal = ""
#                    multiLineCont = False
#                elif(multiLineCont):
#                    multiLineVal = multiLineVal + "\n" + value
#                elif(":" in value):  # If line does not contain valid value pair, discard it.
#                    pair = value.strip().split(':',1)
#                    values[pair[0].strip()] = pair[1].strip()
#                elif("=" in value):
#                    multiLineCont = True
#                    pair = value.split("=", 1)
#                    multiLineId = pair[0]
#                    multiLineVal = multiLineVal + pair[1]
#                    
#        plugins.append({"plugin": name, "values": values})
#    return plugins

def prepPluginCall(htmlStr):
    tree = BeautifulSoup(htmlStr)
    plugins = []
    for node in tree.find_all('pre'):
        try: 
            values = {}
            name = node['plugin']
            if(len(node.text) > 0):
                values = yaml.load(node.text)

            try:
                if(type(values) != str):
                    values["identifier"] = node['id']
            except KeyError:
                values['identifier'] = " "
        except (yaml.parser.ParserError, yaml.scanner.ScannerError):
                print("Malformed yaml string")
                return "YAMLERROR: Malformed string"
        plugins.append({"plugin":name, "markup":values})
    return plugins


# Take a set of blocks and search for plugin markers,
# replace contents with plugin.
def pluginify(blocks,user): 
    preparedBlocks = []
    plugins = []
    for block in blocks:
        if("plugin=" in block and "<code>" in block):
            pluginInfo = prepPluginCall(block)
            if(pluginInfo == "YAMLERROR: Malformed string"):
                preparedBlocks.append("Malformed yaml string")
            else:
                for pair in pluginInfo:
                    try:
                        plugins.append(pair['plugin'])
                        print(pair)
                        pair['markup']["user_id"] =  user
                        pluginHtml = callPlugin(pair['plugin'], pair['markup'])
                        rx = re.compile('<code>.*</code>')
                        block = rx.sub(block, pluginHtml)
                        preparedBlocks.append(block)
                    except TypeError:
                        continue
        else:
            preparedBlocks.append(block)
    return (plugins,preparedBlocks)

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


def getPluginDatas(plugins):
    jsPaths = []
    cssPaths = []
    modules = []
    i = 0
    for p in plugins:
        try:
            (rawJs,rawCss,modsList) = pluginDeps(json.loads(pluginReqs(p)))     
            for src in rawJs:
                if( "http" in src):
                    jsPaths.append(src)
                else:
                    x = getPlugin(p)['host']
                    jsPaths.append(x + src)
            for cssSrc in rawCss:
                if( "http" in src):
                    cssPaths.append(cssSrc)
                else:
                    x = getPlugin(p)['host']
                    cssPaths.append(x + src)
            for mod in modsList:
                modules.append(mod)
        except:
            print("Failed plugin call in plugincontrol getPluginDatas ")
            continue
    print(jsPaths)
    print(cssPaths)
    print(modules)
    return (jsPaths, cssPaths, modules)





