# -*- coding: utf-8 -*-
import json
from functools import lru_cache
from typing import List

import re

import requests
from flask import current_app

from timApp.document.timjsonencoder import TimJsonEncoder
from timApp.markdown.dumboclient import call_dumbo, DumboOptions
from timApp.util.logger import log_warning
from timApp.plugin.plugin import Plugin, AUTOMD
from timApp.plugin.pluginOutputFormat import PluginOutputFormat
from timApp.plugin.pluginexception import PluginException
from timApp.timtypes import DocumentType as Document

from timApp.plugin.timtable import timTable


TIM_URL = ""

CSPLUGIN_NAME = 'csplugin'
SVNPLUGIN_NAME = 'showfile'
HASKELLPLUGIN_NAME = 'haskellplugins'
PALIPLUGIN_NAME = 'pali'
JSAVPLUGIN_NAME = 'jsav'
IMAGEXPLUGIN_NAME = 'imagex'
MARKUP = 'markup'
REGEXATTRS = 'regexattrs'
AUTOMDATTRS = 'automdattrs'

PLUGINS = None
PLUGIN_REGEX_OBJS = {}
QSTMDATTRS = ["rows", "choices", "[0-9]", ".*[Tt]ext"]

"""
plugin class attributes
host:        host address for the plugin server
REGEXATTRS:  list of regexp for yaml attribut names that are handled as markdown when automd attribute is true or 
             plugin class is AUTOMDATTRS: true
AUTOMDATTRS: if true, plugin is allways automd
"""


def get_plugins():
    global PLUGINS
    if PLUGINS is None:
        PLUGINS = {
            "csPlugin": {"host": "http://" + CSPLUGIN_NAME + ":5000/cs/"},
            "taunoPlugin": {"host": "http://" + CSPLUGIN_NAME + ":5000/cs/tauno/"},
            "simcirPlugin": {"host": "http://" + CSPLUGIN_NAME + ":5000/cs/simcir/"},
            "csPluginRikki": {"host": "http://" + CSPLUGIN_NAME + ":5000/cs/rikki/"},  # demonstrates a broken plugin
            "showCode": {"host": "http://" + SVNPLUGIN_NAME + ":5000/svn/"},
            "showImage": {"host": "http://" + SVNPLUGIN_NAME + ":5000/svn/image/"},
            "showVideo": {"host": "http://" + SVNPLUGIN_NAME + ":5000/svn/video/"},
            "showPdf": {"host": "http://" + SVNPLUGIN_NAME + ":5000/svn/pdf/"},
            "mcq": {"host": "http://" + HASKELLPLUGIN_NAME + ":5001/"},
            "mcq2": {"host": "http://" + "localhost" + f":{current_app.config['QST_PLUGIN_PORT']}/qst/mcq/", REGEXATTRS: QSTMDATTRS, AUTOMDATTRS: True},
            "mmcq": {"host": "http://" + HASKELLPLUGIN_NAME + ":5002/"},
            "mmcq2": {"host": "http://" + "localhost" + f":{current_app.config['QST_PLUGIN_PORT']}/qst/mmcq/", REGEXATTRS: QSTMDATTRS, AUTOMDATTRS: True},
            "uploader": {"host": current_app.config['UPLOADER_CONTAINER_URL']},
            #  "shortNote": {"host": "http://" + HASKELLPLUGIN_NAME + ":5003/"},
            # "graphviz": {"host": "http://" + HASKELLPLUGIN_NAME + ":5004/"},
            "graphviz": {"host": "http://" + CSPLUGIN_NAME + ":5000/cs/graphviz/"},
            "pali": {"host": "http://" + PALIPLUGIN_NAME + ":5000/"},
            "jsav": {"host": "http://" + JSAVPLUGIN_NAME + ":5000/"},
            "imagex": {"host": "http://" + IMAGEXPLUGIN_NAME + ":5000/"},
            "qst": {"host": "http://" + "localhost" + f":{current_app.config['QST_PLUGIN_PORT']}/qst/", REGEXATTRS: QSTMDATTRS, AUTOMDATTRS: True},
            "timTable": {"host": "http://" + "localhost" + f":{current_app.config['QST_PLUGIN_PORT']}/timTable/", "instance": timTable.TimTable(), 'lazy': False},
            "tape": {"host": "http://" + "localhost" + f":{current_app.config['QST_PLUGIN_PORT']}/tape/"},
            "echo": {"host": "http://" + "tim" + ":5000/echoRequest/", "skip_reqs": True}
        }
    return PLUGINS


def get_plugin_regex_obj(plugin: str):
    regex_obj = PLUGIN_REGEX_OBJS.get(plugin)
    if regex_obj is None:
        regexattrs = get_plugin(plugin).get("regexattrs")
        regex_pattern = "((" + ")|(".join(regexattrs) + "))"
        regex_obj = re.compile(regex_pattern)
        PLUGIN_REGEX_OBJS[plugin] = regex_obj
    return regex_obj


# QST_REGEX_OBJ = get_plugin_regex_obj('qst')

def call_plugin_generic(plugin: str, method: str, route: str, data=None, headers=None, params=None):
    plug = get_plugin(plugin)
    try:
        read_timeout = 30
        host = plug['host']
        if route == 'multimd' and (plugin == "mmcq" or plugin == "mcq"):  # hack to handle mcq and mmcq in tim by qst
            plug = get_plugin('qst')
            host = plug['host'] + plugin + '/'
        request = requests.request(method, host + route + "/", data=data,
                                   timeout=(0.5, read_timeout), headers=headers, params=params)
        request.encoding = 'utf-8'
        return request.text
    except (requests.exceptions.ConnectTimeout, requests.exceptions.ConnectionError) as e:
        log_warning(f'Connection failed to plugin {plugin}: {e}')
        raise PluginException(f"Could not connect to {plugin}.")
    except requests.exceptions.ReadTimeout as e:
        log_warning(f'Read timeout occurred for plugin {plugin} in route {route}: {e}')
        raise PluginException(f"Read timeout occurred when calling {plugin}.")


def render_plugin(doc: Document, plugin: Plugin, output_format: PluginOutputFormat):
    plugin_data = plugin.render_json()
    if doc.get_settings().plugin_md():
        convert_md([plugin_data],
                   options=plugin.par.get_dumbo_options(base_opts=doc.get_settings().get_dumbo_options()),
                   outtype='md' if output_format == PluginOutputFormat.HTML else 'latex')
    return call_plugin_generic(plugin.type,
                               'post',
                               output_format.value,
                               data=json.dumps(plugin_data,
                                               cls=TimJsonEncoder),
                               headers={'Content-type': 'application/json'})


def call_mock_dumbo_s(s):
    s = s.replace('`', '')
    s = s.replace('#', '\\#')
    s = s.replace('%', '\\%')
    return s


def list_to_dumbo(markup_list):
    i = 0
    for val in markup_list:
        ic = i
        i += 1
        if type(val) is dict:
            dict_to_dumbo(val)
            continue
        if type(val) is markup_list:
            list_to_dumbo(val)
            continue
        if not type(val) is str:
            continue
        if not val.startswith("md:"):
            continue

        v = val[3:]
        v = call_mock_dumbo_s(v)
        markup_list[ic] = v


def dict_to_dumbo(pm):
    for mkey in pm:
        val = pm[mkey]
        if type(val) is dict:
            dict_to_dumbo(val)
            continue
        if type(val) is list:
            list_to_dumbo(val)
            continue

        if not type(val) is str:
            continue

        if not val.startswith("md:"):
            continue
        v = val[3:]
        v = call_mock_dumbo_s(v)
        pm[mkey] = v


def convert_md(plugin_data: List[dict], options: DumboOptions, outtype='md', plugin_opts: List[DumboOptions]=None):
    markups = [p['markup'] for p in plugin_data]
    html_markups = call_dumbo(markups, f'/{outtype}keys', options=options, data_opts=plugin_opts)
    for p, h in zip(plugin_data, html_markups):
        p['markup'] = h


def prepare_for_dumbo_attr_list_recursive(regex_obj, plugin_data: dict):
    for key, value in plugin_data.items():
        if isinstance(value, dict):
            prepare_for_dumbo_attr_list_recursive(regex_obj, value)
        elif isinstance(value, list):
            if regex_obj.search(key) is not None:
                prepare_for_dumbo_attr_list_list_recursive(regex_obj, value)
        elif isinstance(value, str):
             if regex_obj.search(key) is not None:
                 if not plugin_data[key].startswith('md:'):
                    plugin_data[key] = "md:" + value


def prepare_for_dumbo_attr_list_list_recursive(regex_obj, data: list):
    for i in range(0, len(data)):
        item = data[i]
        if isinstance(item, dict):
            prepare_for_dumbo_attr_list_recursive(regex_obj, item)
        elif isinstance(item, list):
            prepare_for_dumbo_attr_list_list_recursive(regex_obj, data)
        elif isinstance(item, str):
            if not item.startswith('md:'):
                data[i] = "md:" + item


def convert_tex_mock(plugin_data):
    if type(plugin_data) is dict:
        dict_to_dumbo(plugin_data)
        return
    for p in plugin_data:
        pm = p["markup"]
        dict_to_dumbo(pm)


def render_plugin_multi(doc: Document, plugin: str, plugin_data: List[Plugin],
                        plugin_output_format: PluginOutputFormat = PluginOutputFormat.HTML,
                        default_auto_md: bool = False):
    opts = doc.get_settings().get_dumbo_options()
    plugin_dumbo_opts = [p.par.get_dumbo_options(base_opts=opts) for p in plugin_data]
    plugin_dicts = [p.render_json() for p in plugin_data]

    plugin_instance = get_inner_plugin_instance(plugin)
    inner = plugin_instance is not None

    plugin_dict = get_plugin(plugin)
    plugin_automd = plugin_dict.get(AUTOMDATTRS, default_auto_md)
    regexattrs = plugin_dict.get(REGEXATTRS)

    for plug_dict in plugin_dicts:
        if has_auto_md(plug_dict[MARKUP], plugin_automd):
            if regexattrs is not None:
                regex_obj = get_plugin_regex_obj(plugin)
                # use attribute list instead of calling the plugin
                prepare_for_dumbo_attr_list_recursive(regex_obj, plug_dict)
            elif inner:
                try:
                    plugin_instance.prepare_for_dumbo(plug_dict)
                except:
                    continue
            else:
                raise PluginException("automd for non-inner plugins not implemented yet") # TODO implement

    if doc.get_settings().plugin_md():
        convert_md(plugin_dicts,
                   options=opts,
                   plugin_opts=plugin_dumbo_opts,
                   outtype='md' if plugin_output_format == PluginOutputFormat.HTML else 'latex')

    if inner and plugin_output_format == PluginOutputFormat.HTML:
        return plugin_instance.multihtml_direct_call(plugin_dicts)

    return call_plugin_generic(plugin,
                                'post',
                                ('multimd' if plugin_output_format == PluginOutputFormat.MD else 'multihtml'),
                                data=json.dumps(plugin_dicts, cls=TimJsonEncoder),
                                headers={'Content-type': 'application/json'})


def get_inner_plugin_instance(plugin: str):
    """
    Gets the instance of an inner plugin.
    An inner plugin runs in the same container with TIM itself.
    If the plugin is not an inner plugin, returns None.
    :param plugin: The name of the plugin.
    :return: The instance of the plugin if it's an inner plugin, otherwise None.
    """
    instance = get_plugin(plugin).get("instance")
    return instance


def has_auto_md(data, default: bool):
    return data.get(AUTOMD, default)


def call_plugin_resource(plugin, filename, args=None):
    try:
        plug = get_plugin(plugin)
        # We need to avoid calling ourselves to avoid infinite request loop.
        if plug['host'].startswith('http://localhost'):
            raise PluginException('Plugin route not found')
        request = requests.get(plug['host'] + filename, timeout=5, stream=True, params=args)
        request.encoding = 'utf-8'
        return request
    except requests.exceptions.Timeout:
        raise PluginException("Could not connect to plugin: " + plugin)


def call_plugin_answer(plugin, answer_data):
    return call_plugin_generic(plugin,
                               'put',
                               'answer',
                               json.dumps(answer_data, cls=TimJsonEncoder),
                               headers={'Content-type': 'application/json'})


# Get lists of js and css files required by plugin, as well as list of Angular modules they define.
@lru_cache(maxsize=100)
def plugin_reqs(plugin):
    return call_plugin_generic(plugin, 'get', 'reqs')


# Gets plugin info (host)
def get_plugin(plugin: str):
    plugins = get_plugins()
    plug = plugins.get(plugin)
    if plug:
        return plug
    raise PluginException("Plugin does not exist.")
