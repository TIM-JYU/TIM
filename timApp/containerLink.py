import urllib.request
import urllib

PLUGINS = [
        {"host" : "http://localhost:8080/", "name" : "csPlugin"},
        {"host" : "http://localhost:8000/", "name" : "typeGame"}
        ]

# Main plugin call, plugin must match one of the specified plugins in 
# PLUGINS
def callPlugin(plugin, params=""):
    for x in PLUGINS:
        if(x['name'] == plugin): 
            plug = getPlugin(plugin)
            if(params != ""):
                request = urllib.request.urlopen(urllib.parse.urljoin(plug['host'], params))
            else:
                request = urllib.request.urlopen(plug['host'])
            return request.read()
        else:
            return "Unregistered plugin"

def getPlugin(plug):
    for p in PLUGINS:
        if plug == p["name"]:
            return p
    return "ERROR: Requested plugin not specified, please check PLUGINS and verify the plugin is registered to the system"


