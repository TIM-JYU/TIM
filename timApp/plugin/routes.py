import json

from flask import request, Response, stream_with_context, Blueprint

from timApp.auth.accesshelper import get_doc_or_abort, verify_task_access
from timApp.auth.accesstype import AccessType
from timApp.auth.sessioninfo import get_current_user_object, logged_in, user_context_with_logged_in
from timApp.document.timjsonencoder import TimJsonEncoder
from timApp.document.viewcontext import default_view_ctx
from timApp.plugin.containerLink import call_plugin_resource, call_plugin_generic
from timApp.plugin.pluginexception import PluginException
from timApp.plugin.taskid import TaskId, TaskIdAccess
from timApp.timdb.exceptions import TimDbException
from timApp.util.flask.requesthelper import RouteException, NotExist
from timApp.util.flask.responsehelper import json_response

plugin_bp = Blueprint('plugin',
                      __name__,
                      url_prefix='')


def return_resource_response(resp):
    headers = {k: v for k, v in resp.headers.items() if k not in (
        'transfer-encoding',
    )}
    return resp.raw.read(), resp.status_code, headers


@plugin_bp.route("/<plugin>/<path:filename>")
def plugin_call(plugin, filename):
    try:
        resp = call_plugin_resource(plugin, filename, request.args)
        return return_resource_response(resp)
    except PluginException as e:
        raise NotExist(str(e))


@plugin_bp.route("/echoRequest/<path:filename>")
def echo_request(filename):
    def generate():
        yield 'Request URL: ' + request.url + "\n\n"
        yield 'Headers:\n\n'
        yield from (k + ": " + v + "\n" for k, v in request.headers.items())

    return Response(stream_with_context(generate()), mimetype='text/plain')


@plugin_bp.route("/<plugin>/template/<template>/<index>")
def view_template(plugin, template, index):
    try:
        resp = call_plugin_resource(plugin, "template?file=" + template + "&idx=" + index)
        return return_resource_response(resp)
    except PluginException:
        raise NotExist()


@plugin_bp.route("/plugin/<plugintype>/<task_id_ext>/fetchExternal", methods=['PUT', 'POST'])
def plugin_tid_call(plugintype: str, task_id_ext: str):
    """plugin_call but with task id and markup"""
    tid = TaskId.parse(task_id_ext)
    d = get_doc_or_abort(tid.doc_id)
    d.document.insert_preamble_pars()

    curr_user = get_current_user_object()

    try:
        vr = verify_task_access(
            d,
            tid,
            AccessType.view,
            TaskIdAccess.ReadWrite,
            context_user=user_context_with_logged_in(curr_user),
            allow_grace_period=True,
            view_ctx=default_view_ctx,
        )
        plugin = vr.plugin
    except (PluginException, TimDbException) as e:
        raise PluginException(str(e))

    if plugin.type != plugintype:
        raise PluginException(f'Plugin type mismatch: {plugin.type} != {plugintype}')

    if not logged_in() and not plugin.known.anonymous:
        raise RouteException('You must be logged in to fetch on this task.')

    info = plugin.get_info([curr_user], 0)

    call_data = {'markup': plugin.values,
                'taskID': tid.doc_task,
                'info': info,}

    plugin_response = call_plugin_generic(plugintype,
                    request.method,
                    "fetchExternal",
                    json.dumps(call_data, cls=TimJsonEncoder),
                    headers={'Content-type': 'application/json'},
                    read_timeout=30)
    try:
        jsonresp = json.loads(plugin_response)
    except ValueError as e:
        raise PluginException(
            'The plugin response was not a valid JSON string. The response was: ' + plugin_response) from e

    return json_response(jsonresp)
