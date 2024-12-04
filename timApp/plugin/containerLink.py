import json
import re
from dataclasses import dataclass
from functools import lru_cache
from re import Pattern
from typing import Any

import requests
from flask import current_app, has_request_context
from requests import Response

from timApp.document.docsettings import DocSettings
from timApp.plugin.plugin import Plugin, AUTOMD
from timApp.plugin.pluginOutputFormat import PluginOutputFormat
from timApp.plugin.pluginexception import PluginException
from timApp.plugin.timtable import timTable
from timApp.util.locale import get_locale
from timApp.util.logger import log_warning
from tim_common.dumboclient import call_dumbo, DumboOptions
from tim_common.timjsonencoder import TimJsonEncoder

CSPLUGIN_DOMAIN = "csplugin"
DRAGPLUGIN_DOMAIN = "drag"
FEEDBACKPLUGIN_DOMAIN = "feedback"
FIELDPLUGIN_DOMAIN = "fields"
HASKELLPLUGIN_DOMAIN = "haskellplugins"
IMAGEXPLUGIN_DOMAIN = "imagex"
JSRUNNERPLUGIN_DOMAIN = "jsrunner"
PALIPLUGIN_DOMAIN = "pali"
SVNPLUGIN_DOMAIN = "showfile"

MARKUP = "markup"

QSTMDATTRS = [
    "header",
    "expl",
    "stem",
    "rows",
    "reason",
    "choices",
    "[0-9]",
    ".*[Tt]ext",
]

FBMDATTRS = ["nextTask", "questionItems", "choices", "levels"]
DRAGATTRS = ["words"]
TEXTFIELDATTRS = ["header", "stem", "inputstem"]
GOALTABLEATTRS = ["goals", "stem", "header"]


@dataclass
class PluginReg:
    name: str
    domain: str
    port: int = 5000
    path: str = "/"
    automd: bool | None = None  # if true, plugin is always automd

    # List of regexp for yaml attribute names that are handled as markdown when automd attribute is true or
    # plugin has automd=True
    regexattrs: list[str] | None = None
    skip_reqs: bool = False
    lazy: bool = True
    can_give_task: bool = False
    instance: Any | None = None  # TODO get rid of this; only used by timtable

    @property
    def host(self) -> str:  # TODO rename to url because contains path
        return f"http://{self.domain}:{self.port}{self.path}"


@lru_cache
def get_plugins() -> dict[str, PluginReg]:
    qst_port = current_app.config["QST_PLUGIN_PORT"]
    internal_domain = current_app.config["INTERNAL_PLUGIN_DOMAIN"]
    plugin_list = [
        PluginReg(name="csPlugin", domain=CSPLUGIN_DOMAIN, path="/cs/"),
        PluginReg(name="taunoPlugin", domain=CSPLUGIN_DOMAIN, path="/cs/tauno/"),
        PluginReg(name="simcirPlugin", domain=CSPLUGIN_DOMAIN, path="/cs/simcir/"),
        PluginReg(name="graphviz", domain=CSPLUGIN_DOMAIN, path="/cs/graphviz/"),
        PluginReg(name="showCode", domain=SVNPLUGIN_DOMAIN, path="/svn/"),
        PluginReg(name="showImage", domain=SVNPLUGIN_DOMAIN, path="/svn/image/"),
        PluginReg(name="showImages", domain=SVNPLUGIN_DOMAIN, path="/svn/multiimages/"),
        PluginReg(name="showVideo", domain=SVNPLUGIN_DOMAIN, path="/svn/video/"),
        PluginReg(name="showPdf", domain=SVNPLUGIN_DOMAIN, path="/svn/pdf/"),
        PluginReg(name="mcq", domain=HASKELLPLUGIN_DOMAIN, port=5001),
        PluginReg(name="mmcq", domain=HASKELLPLUGIN_DOMAIN, port=5002),
        PluginReg(
            name="mcq2",
            domain=internal_domain,
            port=qst_port,
            path="/qst/mcq/",
            regexattrs=QSTMDATTRS,
            automd=True,
        ),
        PluginReg(
            name="mmcq2",
            domain=internal_domain,
            port=qst_port,
            path="/qst/mmcq/",
            regexattrs=QSTMDATTRS,
            automd=True,
        ),
        PluginReg(name="pali", domain=PALIPLUGIN_DOMAIN),
        # TODO: field is just a dummy class to get route for /field - better solution is needed
        PluginReg(
            name="field",
            domain=FIELDPLUGIN_DOMAIN,
            regexattrs=TEXTFIELDATTRS,
            automd=True,
        ),
        PluginReg(
            name="textfield",
            domain=FIELDPLUGIN_DOMAIN,
            path="/tf/",
            regexattrs=TEXTFIELDATTRS,
            automd=True,
        ),
        PluginReg(
            name="cbfield",
            domain=FIELDPLUGIN_DOMAIN,
            path="/cb/",
            regexattrs=TEXTFIELDATTRS,
            automd=True,
        ),
        PluginReg(
            name="cbcountfield",
            domain=internal_domain,
            port=qst_port,
            path="/cbcountfield/",
            regexattrs=TEXTFIELDATTRS,
            automd=True,
        ),
        PluginReg(
            name="rbfield",
            domain=FIELDPLUGIN_DOMAIN,
            path="/rb/",
            regexattrs=TEXTFIELDATTRS,
            automd=True,
        ),
        PluginReg(
            name="numericfield",
            domain=FIELDPLUGIN_DOMAIN,
            path="/nf/",
            regexattrs=TEXTFIELDATTRS,
            automd=True,
        ),
        PluginReg(
            name="goaltable",
            domain=FIELDPLUGIN_DOMAIN,
            path="/goaltable/",
            regexattrs=GOALTABLEATTRS,
            automd=True,
        ),
        PluginReg(name="multisave", domain=FIELDPLUGIN_DOMAIN, path="/ms/"),
        PluginReg(name="dropdown", domain=FIELDPLUGIN_DOMAIN, path="/dropdown/"),
        PluginReg(name="jsrunner", domain=JSRUNNERPLUGIN_DOMAIN),
        PluginReg(name="imagex", domain=IMAGEXPLUGIN_DOMAIN),
        PluginReg(
            name="qst",
            domain=internal_domain,
            port=qst_port,
            path="/qst/",
            regexattrs=QSTMDATTRS,
            automd=True,
        ),
        PluginReg(
            name="timMenu", domain=internal_domain, port=qst_port, path="/timMenu/"
        ),
        PluginReg(
            name="timTable",
            domain=internal_domain,
            port=qst_port,
            path="/timTable/",
            instance=timTable.TimTable(),
            lazy=False,
        ),
        PluginReg(
            name="tableForm",
            domain=internal_domain,
            port=qst_port,
            path="/tableForm/",
            lazy=False,
        ),
        PluginReg(
            name="reviewcanvas",
            domain=internal_domain,
            port=qst_port,
            path="/reviewcanvas/",
            lazy=False,
        ),
        PluginReg(
            name="importData",
            domain=internal_domain,
            port=qst_port,
            path="/importData/",
        ),
        PluginReg(
            name="userSelect",
            domain=internal_domain,
            port=qst_port,
            path="/userSelect/",
        ),
        PluginReg(
            name="groupJoin",
            domain=internal_domain,
            port=qst_port,
            path="/groupJoin/",
        ),
        PluginReg(name="tape", domain=internal_domain, port=qst_port, path="/tape/"),
        PluginReg(name="echo", domain="tim", path="/echoRequest/", skip_reqs=True),
        PluginReg(
            name="feedback",
            domain=FEEDBACKPLUGIN_DOMAIN,
            regexattrs=FBMDATTRS,
            automd=True,
        ),
        PluginReg(
            name="drag", domain=DRAGPLUGIN_DOMAIN, regexattrs=DRAGATTRS, automd=True
        ),
        PluginReg(
            name="calendar", domain=internal_domain, port=qst_port, path="/calendar/"
        ),
        PluginReg(name="steps", domain=internal_domain, port=qst_port, path="/steps/"),
        PluginReg(
            name="examGroupManager",
            domain=internal_domain,
            port=qst_port,
            path="/examGroupManager/",
        ),
        PluginReg(
            name="quantumCircuit",
            domain=internal_domain,
            port=qst_port,
            path="/quantumCircuit/",
        ),
        PluginReg(
            name="symbolbutton",
            domain=internal_domain,
            port=qst_port,
            path="/symbolbutton/",
        ),
    ]
    plugins = {p.name: p for p in plugin_list}
    return plugins


@lru_cache
def get_plugin_regex_obj(plugin_name: str) -> Pattern:
    plugin = get_plugin(plugin_name)
    assert plugin.regexattrs is not None
    regex_pattern = "((" + ")|(".join(plugin.regexattrs) + "))"
    regex_obj = re.compile(regex_pattern)
    return regex_obj


def call_plugin_generic(
    plugin: str,
    method: str,
    route: str,
    data: Any = None,
    headers: Any = None,
    params: Any = None,
    read_timeout: int = 30,
) -> Response:
    plug = get_plugin(plugin)
    host = plug.host
    if route == "multimd" and (
        plugin == "mmcq" or plugin == "mcq"
    ):  # hack to handle mcq and mmcq in tim by qst
        plug = get_plugin("qst")
        host = plug.host + plugin + "/"
    url = host + route
    # Pass current locale if we are in a request context
    # This allows translating any server text via plugins if they support it
    if has_request_context():
        locale = get_locale()
        if headers is None:
            headers = {}
        headers["Accept-Language"] = locale
    try:
        r = do_request(method, url, data, params, headers, read_timeout)
    except (
        requests.exceptions.ConnectTimeout,
        requests.exceptions.ConnectionError,
    ) as e:
        log_warning(f"Connection failed to plugin {plugin}: {e}")
        raise PluginException(f"Connect timeout when calling {plugin} ({url}).")
    except requests.exceptions.ReadTimeout as e:
        log_warning(f"Read timeout occurred for plugin {plugin} in route {route}: {e}")
        raise PluginException(f"Read timeout when calling {plugin} ({url}).")
    else:
        if r.status_code >= 500:
            raise PluginException(f"Got response with status code {r.status_code}")
        return r


def do_request(
    method: str,
    url: str,
    data: Any,
    params: Any,
    headers: Any,
    read_timeout: int,
) -> requests.Response:
    resp = requests.request(
        method,
        url,
        data=data,
        timeout=(current_app.config["PLUGIN_CONNECT_TIMEOUT"], read_timeout),
        headers=headers,
        params=params,
    )
    resp.encoding = "utf-8"
    return resp


# Not used currently.
plugin_request_fn = do_request


def render_plugin(
    docsettings: DocSettings, plugin: Plugin, output_format: PluginOutputFormat
) -> str:
    plugin_data = plugin.render_json()
    if docsettings.plugin_md():
        convert_md(
            [plugin_data],
            options=plugin.par.get_dumbo_options(
                base_opts=docsettings.get_dumbo_options()
            ),
            outtype="md" if output_format == PluginOutputFormat.HTML else "latex",
        )
    return call_plugin_generic(
        plugin.type,
        "post",
        output_format.value,
        data=json.dumps(plugin_data, cls=TimJsonEncoder),
        headers={"Content-type": "application/json"},
    ).text


def call_mock_dumbo_s(s: str) -> str:
    s = s.replace("`", "")
    s = s.replace("#", "\\#")
    s = s.replace("%", "\\%")
    return s


def list_to_dumbo(markup_list: list[Any]) -> None:
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


def dict_to_dumbo(pm: dict) -> None:
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


def convert_md(
    plugin_data: list[dict],
    options: DumboOptions,
    outtype: str = "md",
    plugin_opts: list[DumboOptions] | None = None,
) -> None:
    markups = [p for p in plugin_data]
    html_markups = call_dumbo(
        markups, f"/{outtype}keys", options=options, data_opts=plugin_opts
    )
    if isinstance(html_markups, dict) and "error" in html_markups:
        # TODO: Notify user of failed MD conversion or do safe parsing
        return
    for p, h in zip(plugin_data, html_markups):
        p.clear()
        p.update(h)


def prepare_for_dumbo_attr_list_recursive(
    regex_obj: Pattern, plugin_data: dict
) -> None:
    for key, value in plugin_data.items():
        if isinstance(value, dict):
            prepare_for_dumbo_attr_list_recursive(regex_obj, value)
        elif isinstance(value, list):
            if regex_obj.search(str(key)) is not None:
                prepare_for_dumbo_attr_list_list_recursive(regex_obj, value)
        elif isinstance(value, str):
            if regex_obj.search(str(key)) is not None:
                if not plugin_data[key].startswith("md:"):
                    plugin_data[key] = "md:" + value


def prepare_for_dumbo_attr_list_list_recursive(regex_obj: Pattern, data: list) -> None:
    for i in range(0, len(data)):
        item = data[i]
        if isinstance(item, dict):
            prepare_for_dumbo_attr_list_recursive(regex_obj, item)
        elif isinstance(item, list):
            prepare_for_dumbo_attr_list_list_recursive(regex_obj, item)
        elif isinstance(item, str):
            if not item.startswith("md:"):
                data[i] = "md:" + item


def convert_tex_mock(plugin_data: dict | list) -> None:
    if isinstance(plugin_data, dict):
        dict_to_dumbo(plugin_data)
        return
    for p in plugin_data:
        pm = p["markup"]
        dict_to_dumbo(pm)


def render_plugin_multi(
    docsettings: DocSettings,
    plugin: str,
    plugin_data: list[Plugin],
    plugin_output_format: PluginOutputFormat = PluginOutputFormat.HTML,
    default_auto_md: bool = False,
) -> str:
    opts = docsettings.get_dumbo_options()
    plugin_dumbo_opts = [p.par.get_dumbo_options(base_opts=opts) for p in plugin_data]
    plugin_dicts = [p.render_json() for p in plugin_data]
    plugin_reg = get_plugin(plugin)
    plugin_automd = (
        plugin_reg.automd if plugin_reg.automd is not None else default_auto_md
    )
    regexattrs = plugin_reg.regexattrs

    for plug_dict in plugin_dicts:
        if has_auto_md(plug_dict[MARKUP], plugin_automd):
            if regexattrs is not None:
                regex_obj = get_plugin_regex_obj(plugin_reg.name)
                # use attribute list instead of calling the plugin
                prepare_for_dumbo_attr_list_recursive(regex_obj, plug_dict)
            elif plugin_reg.instance:
                try:
                    plugin_reg.instance.prepare_for_dumbo(plug_dict)
                except:
                    continue
            else:
                raise PluginException(
                    "automd for non-inner plugins not implemented yet"
                )  # TODO implement

    if docsettings.plugin_md():
        convert_md(
            plugin_dicts,
            options=opts,
            plugin_opts=plugin_dumbo_opts,
            outtype="md"
            if plugin_output_format == PluginOutputFormat.HTML
            else "latex",
        )

    if plugin_reg.instance and plugin_output_format == PluginOutputFormat.HTML:
        return plugin_reg.instance.multihtml_direct_call(plugin_dicts)

    return call_plugin_generic(
        plugin,
        "post",
        ("multimd" if plugin_output_format == PluginOutputFormat.MD else "multihtml"),
        data=json.dumps(plugin_dicts, cls=TimJsonEncoder),
        headers={"Content-type": "application/json"},
    ).text


def has_auto_md(data: dict, default: bool) -> bool:
    return data.get(AUTOMD, default)


def call_plugin_resource(
    plugin: str, filename: str, args: Any = None
) -> requests.Response:
    try:
        plug = get_plugin(plugin)
        # We need to avoid calling ourselves to avoid infinite request loop.
        if plug.host.startswith(
            f'http://{current_app.config["INTERNAL_PLUGIN_DOMAIN"]}'
        ):
            raise PluginException("Plugin route not found")
        resp = requests.get(plug.host + filename, timeout=5, stream=True, params=args)
        resp.encoding = "utf-8"
        return resp
    except requests.exceptions.Timeout:
        raise PluginException("Could not connect to plugin: " + plugin)


def call_plugin_answer(plugin: str, answer_data: dict) -> str:
    markup = answer_data.get("markup") or {}
    timeout = markup.get("timeout")
    if not isinstance(timeout, int):
        if isinstance(timeout, str) and timeout.isnumeric():
            timeout = int(timeout)
        else:
            timeout = 25
    # use timeout + 5 so that plugin will realize the timeout first
    return call_plugin_generic(
        plugin,
        "put",
        "answer",
        json.dumps(answer_data, cls=TimJsonEncoder),
        headers={"Content-type": "application/json"},
        read_timeout=min(timeout + 5, 120),
    ).text


# Get lists of js and css files required by plugin, as well as list of Angular modules they define.
@lru_cache(maxsize=100)
def plugin_reqs(plugin: str) -> str:
    return call_plugin_generic(plugin, "get", "reqs").text


# Gets plugin info (host)
def get_plugin(plugin: str) -> PluginReg:
    plugins = get_plugins()
    plug = plugins.get(plugin)
    if plug:
        return plug
    raise PluginException("Plugin does not exist.")
