import urllib.request
import urllib.parse
import urllib

PLUGINS = [
        {"host" : "http://tim-beta.it.jyu.fi/cs/", "name" : "csPlugin"},
        {"host" : "http://localhost:50005/", "name" : "helloExample"}
        ]

# Main plugin call, plugin must match one of the specified plugins in 
# PLUGINS
def callPlugin(plugin, info):
    for x in PLUGINS:
        if(x['name'] == plugin):
            plug = getPlugin(plugin)
            request = urllib.request.urlopen(plug['host'], urllib.parse.urlencode(info).encode('utf-8'))
            return request.read().decode(encoding="UTF-8")
    return "Unregistered plugin"

def getPlugin(plug):
    for p in PLUGINS:
        if plug == p["name"]:
            return p
    return "ERROR: Requested plugin not specified, please check PLUGINS and verify the plugin is registered to the system"


