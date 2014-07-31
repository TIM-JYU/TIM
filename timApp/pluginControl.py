from bs4 import BeautifulSoup
from containerLink import callPlugin
import re

# Receive html-string with plugin in it, 
# extract values from contents
def prepPluginCall(htmlStr):
    tree = BeautifulSoup(htmlStr)
    plugins = []
    for node in tree.find_all('pre'):
        values = []
        name = node['plugin']
        if(len(node['id']) > 0):
            values.append(("identifier" , node['id']))
        if(len(node.text) > 0):
            multiLineId = ""
            multiLineVal = ""
            multiLineCont = False
            for value in node.string.strip().split('\n'):
                if("=====" in value):
                    values.append((multiLineId, multiLineVal))
                    multiLineId = ""
                    multiLineVal = ""
                    multiLineCont = False
                elif(multiLineCont):
                    multiLineVal = multiLineVal + "\n" + value
                elif(":" in value):  # If line does not contain valid value pair, discard it.
                    pair = value.strip().split(':',1)
                    values.append((pair[0].strip(), pair[1].strip()))
                elif("=" in value):
                    multiLineCont = True
                    pair = value.split("=", 1)[0]
                    multiLineId = pair[0]
                    multilineVal = multiLineVal + pair[1]
                    
        plugins.append({"plugin": name, "values": values})
    return plugins


# Take a set of blocks and search for plugin markers,
# replace contents with plugin.
def pluginify(blocks,user): 
    preparedBlocks = []
    for block in blocks:
        if("plugin=" in block and "<code>" in block):
            pluginInfo = prepPluginCall(block)
            for pair in pluginInfo:
                pair['values'].append(("user_id", user))
                pluginHtml = callPlugin(pair['plugin'], pair['values'])
                rx = re.compile('<code>.*</code>')
                block = rx.sub(block, pluginHtml)
                preparedBlocks.append(block)
        else:
            preparedBlocks.append(block)
    return preparedBlocks


