import requests

PLUGINS = [
        {"host" : "http://tim-beta.it.jyu.fi/cs/", "name" : "csPlugin"},
        {"host" : "http://tim-beta.it.jyu.fi/cs/rikki/", "name" : "csPluginRikki"}, # rikkinäisen demonstroimiseksi
        {"host" : "http://tim-beta.it.jyu.fi/svn/", "name" : "showCode"},
        {"host" : "http://tim-beta.it.jyu.fi/svn/image/", "name" : "showImage"},
        {"host" : "http://tim-beta.it.jyu.fi/svn/video/", "name" : "showVideo"},
        {"host" : "http://172.17.42.1:50005/", "name" : "helloExample"},
        {"host" : "http://localhost:8080/", "name" : "exampleServ"}
        ]

# plugin html call, plugin must match one of the specified plugins in 
# PLUGINS
def callPlugin(plugin, info):
    return callPluginRoute(plugin, "html/", "post", info)

def callPluginRoute(plugin, route, callMethod="get", params=None):
    try:
        for x in PLUGINS:
            if(x['name'] == plugin):
                plug = getPlugin(plugin)
                request = requests.request(url=plug['host'] + route, method=callMethod, data=params, timeout=5)
                request.encoding = 'utf-8'
                return request.text
        return "Unregistered plugin"
    except:
        return "Could not connect to plugin"

def callPluginAnswer(plugin, params):
    return callPluginRoute(plugin, "answer/", "put", params)

def pluginReqs(plugin):
    return callPluginRoute(plugin, "reqs/")

def getPlugin(plug):
    for p in PLUGINS:
        if plug == p["name"]:
            return p
    return "ERROR: Requested plugin not specified, please check PLUGINS and verify the plugin is registered to the system"
