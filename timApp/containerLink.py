import urllib.request
import urllib

PLUGINS = [
        {"host" : "http://localhost:50003/1/1", "name" : "typeGame"}
        ]

# Main plugin call, plugin must match one of the specified plugins in 
# PLUGINS
def callPlugin(plugin, params=""):
    plug = getPlugin(plugin)
    if(params != ""):
        data = urllib.urlencode(params)
        request = urllib.request.urlopen(plug['host'], data=data)
    else:
        request = urllib.request.urlopen(plug['host'])
    return request.read()

def getPlugin(plug):
    for p in PLUGINS:
        if plug == p["name"]:
            return p
    return "ERROR: Requested plugin not specified, please check PLUGINS and verify the plugin is registered to the system"


