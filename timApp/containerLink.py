# -*- coding: utf-8 -*-
import urllib.request
import urllib.parse
import urllib
import requests
import sys
import json

PLUGINS = [
        {"host" : "http://tim-beta.it.jyu.fi/cs/", "name" : "csPlugin"},
        {"host" : "http://tim-beta.it.jyu.fi/cs/rikki/", "name" : "csPluginRikki"}, # rikkinäisen demonstroimiseksi
        {"host" : "http://tim-beta.it.jyu.fi/svn/", "name" : "showCode"},
        {"host" : "http://tim-beta.it.jyu.fi/svn/image/", "name" : "showImage"},
        {"host" : "http://tim-beta.it.jyu.fi/svn/video/", "name" : "showVideo"},
#        {"host" : "http://172.17.42.1:57000/", "name" : "mmcq"},
        ]

# plugin html call, plugin must match one of the specified plugins in 
# PLUGINS
def callPlugin(plugin, info, state):
    try:
        for x in PLUGINS:
            if(x['name'] == plugin):
                plug = getPlugin(plugin)
                headers = {'content-type': 'application/json'}
                request = requests.post(plug['host'] + "html/", data=json.dumps({"markup" : info, "state": None}),headers=headers, timeout=5)
                return request.text
        return "Unregistered plugin"
    except:
        return "Could not connect to plugin" 


# plugin answer call, plugin must match one of the specified plugins in 
# PLUGINS
def callPluginAnswer(plugin, answerData):
#    try:
    for x in PLUGINS:
        if(x['name'] == plugin):
            request = requests.put( url=x['host'] + "answer/", data=answerData, headers=headers,timeout=5)
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


