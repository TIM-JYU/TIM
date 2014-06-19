import urllib.request

PLUGINS = [
        {"host" : "http://localhost:50002/1/1", "name" : "typeGame"}
        ]

# Main plugin call, plugin must match one of the specified plugins in 
# PLUGINS
def callPlugin(plugin, user=""):
    plug = getPlugin(plugin)
    request = urllib.request.urlopen(plug['host'])
    return request.read()

def getPlugin(plug):
    for p in PLUGINS:
        if plug == p["name"]:
            return p
    return "ERROR: Requested plugin not specified, please check PLUGINS and verify the plugin is registered to the system"


