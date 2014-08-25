# -*- coding: utf-8 -*-
import urllib.request
import urllib.parse
import urllib
import requests
import sys
import json

TIM_URL = "http://tim-beta.it.jyu.fi"

PLUGINS = [
        {"host" : "http://tim-beta.it.jyu.fi/cs/", "name" : "csPlugin"},
        {"host" : "http://tim-beta.it.jyu.fi/cs/rikki/", "name" : "csPluginRikki"}, # rikkin�isen demonstroimiseksi
        {"host" : "http://tim-beta.it.jyu.fi/svn/", "name" : "showCode"},
        {"host" : "http://tim-beta.it.jyu.fi/svn/image/", "name" : "showImage"},
        {"host" : "http://tim-beta.it.jyu.fi/svn/video/", "name" : "showVideo"},
        {"host" : "http://tim-beta.it.jyu.fi/cs/tauno/", "name" : "taunoPlugin"},
        {"host" : "http://172.17.42.1:57000/", "name" : "mcq"},

        ] 

# plugin html call, plugin must match one of the specified plugins in 
# PLUGINS
def callPlugin(plugin, info, state):
    try:
        for x in PLUGINS:
            if(x['name'] == plugin):
                plug = getPlugin(plugin)
                headers = {'Content-type': 'application/json'}
                request = requests.post(plug['host'] + "html/", data={"markup" : info, "state": state}, timeout=5, headers=headers)
                return request.text
        return "Unregistered plugin"
    except:
        return "Could not connect to plugin" 


# plugin html call, plugin must match one of the specified plugins in 
# PLUGINS
def callPluginResource(plugin, fileName):
    try:
        for x in PLUGINS:
            if(x['name'] == plugin):
                plug = getPlugin(plugin)
                request = requests.get(plug['host'] + fileName, timeout=5)
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
            headers = {'Content-type': 'application/json'}
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

def getPluginTimUrl(plug):
    for p in PLUGINS:
        if plug == p["name"]:
            return TIM_URL + "/" + p['name']
    return "ERROR: Requested plugin not specified, please check PLUGINS and verify the plugin is registered to the system"
