import requests

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
                request = requests.post(url=plug['host'] + "html/", data=info, timeout=5)
                request.encoding = 'utf-8'
                return request.text
        return "Unregistered plugin"
    except:
        return "Could not connect to plugin" 

def pluginReqs(plugin):
    try:
        plug = getPlugin(plugin)
        request = requests.get(url=plug['host'] + "reqs/", timeout=5)
        request.encoding = 'utf-8'
        return request.text
    except:
        return "Could not connect to plugin" 

def getPlugin(plug):
    for p in PLUGINS:
        if plug == p["name"]:
            return p
    return "ERROR: Requested plugin not specified, please check PLUGINS and verify the plugin is registered to the system"
