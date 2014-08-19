import urllib.request
import urllib.parse
import urllib
import requests
import sys

PLUGINS = [
        {"host" : "http://tim-beta.it.jyu.fi/cs/", "name" : "csPlugin"},
        {"host" : "http://172.17.42.1:50005/", "name" : "helloExample"},
        {"host" : "http://localhost:8080/", "name" : "exampleServ"}
        ]

# plugin html call, plugin must match one of the specified plugins in 
# PLUGINS
def callPlugin(plugin, info):
    try:
        for x in PLUGINS:
            if(x['name'] == plugin):
                plug = getPlugin(plugin)
                request = urllib.request.urlopen(plug['host'] + "html/", urllib.parse.urlencode(info).encode('utf-8'), timeout=5)
                return request.read().decode(encoding="UTF-8")
        return "Unregistered plugin"
    except:
        return "Could not connect to plugin" 


# plugin answer call, plugin must match one of the specified plugins in 
# PLUGINS
def callPluginAnswer(plugin, answerData):
#    try:
    for x in PLUGINS:
        if(x['name'] == plugin):
            plug = getPlugin(plugin)
            request = requests.put( url=plug['host'] + "answer/", data=answerData, timeout=5)
            return request.text
    return "Unregistered plugin or plugin not answering. Contact document administrator for details"
#    except:
#        return "Could not connect to plugin" 


def pluginReqs(plugin):
    try:
        plug = getPlugin(plugin)
        request = urllib.request.urlopen(plug['host'] + "reqs/" , timeout=5)

        return request.read().decode(encoding="UTF-8")
    except:
        return "Could not connect to plugin" 

def getPlugin(plug):
    for p in PLUGINS:
        if plug == p["name"]:
            return p
    return "ERROR: Requested plugin not specified, please check PLUGINS and verify the plugin is registered to the system"


