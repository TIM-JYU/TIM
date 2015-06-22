# -*- coding: utf-8 -*-
import requests
from requests.exceptions import Timeout
import json


class PluginException(Exception):
    """The exception that is thrown when an error occurs during a plugin call."""
    pass


TIM_URL = ""

# Most plugins are currently running on tim-beta.it.jyu.fi
# Plugins with numeric IP-address are running there as well, they just don't have routes
# defined in nginx, and as such must be accessed through tim-beta localhost. However,
# as TIM is run from a docker container, pointing to tim-beta's localhost
# must be made through a special bridge, which for docker containers is
# by default 172.17.42.1
PLUGINS = {
    "csPlugin":      {"host": "http://172.17.42.1:56000/cs/"},
    "taunoPlugin":   {"host": "http://172.17.42.1:56000/cs/tauno/"},
    "csPluginRikki": {"host": "http://172.17.42.1:56000/cs/rikki/"},  # demonstrates a broken plugin
    "showCode":      {"host": "http://172.17.42.1:55000/svn/"},
    "showImage":     {"host": "http://172.17.42.1:55000/svn/image/"},
    "showVideo":     {"host": "http://172.17.42.1:55000/svn/video/"},
    "mcq":           {"host": "http://172.17.42.1:57000/"},
    "mmcq":          {"host": "http://172.17.42.1:58000/"},
    "shortNote":     {"host": "http://172.17.42.1:59000/"},
    "graphviz":      {"host": "http://172.17.42.1:60000/"},
    "pali":          {"host": "http://172.17.42.1:61000/"}
}


def call_plugin_generic(plugin, method, route, data=None, headers=None):
    plug = get_plugin(plugin)
    try:
        request = requests.request(method, plug['host'] + route + "/", data=data, timeout=5, headers=headers)
        request.encoding = 'utf-8'
        return request.text
    except (requests.exceptions.Timeout, requests.exceptions.ConnectionError) as e:
        raise PluginException("Could not connect to plugin.")


def call_plugin_html(plugin, info, state, task_id=None):
    plugin_data = json.dumps({"markup": info, "state": state, "taskID": task_id})
    return call_plugin_generic(plugin,
                               'post',
                               'html',
                               data=plugin_data,
                               headers={'Content-type': 'application/json'})


def call_plugin_multihtml(plugin, plugin_data):
    return call_plugin_generic(plugin,
                               'post',
                               'multihtml',
                               data=plugin_data,
                               headers={'Content-type': 'application/json'})


def call_plugin_resource(plugin, filename):
    try:
        plug = get_plugin(plugin)
        request = requests.get(plug['host'] + filename, timeout=5, stream=True)
        request.encoding = 'utf-8'
        return request
    except requests.exceptions.Timeout:
        raise PluginException("Could not connect to plugin: " + plugin)


def call_plugin_answer(plugin, answer_data):
    return call_plugin_generic(plugin,
                               'put',
                               'answer',
                               json.dumps(answer_data),
                               headers={'Content-type': 'application/json'})


# Get lists of js and css files required by plugin, as well as list of Angular modules they define. 
def plugin_reqs(plugin):
    return call_plugin_generic(plugin, 'get', 'reqs')


# Gets plugin info (host)
def get_plugin(plugin):
    if plugin in PLUGINS:
        return PLUGINS[plugin]
    raise PluginException("Plugin does not exist.")


# Get address towards which the plugin must send its further requests, such as answers
def get_plugin_tim_url(plugin):
    if plugin in PLUGINS:
        return TIM_URL + "/" + plugin
    raise PluginException("Plugin does not exist.")