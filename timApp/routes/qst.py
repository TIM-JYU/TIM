"""Routes for qst (question) plugin."""
from flask import Blueprint, render_template
import binascii
import routes.lecture

from routes.lecture import *

from options import get_option
from routes.accesshelper import verify_manage_access, verify_ownership, get_rights, verify_view_access, \
    has_manage_access
from timdb.blocktypes import from_str
from timdb.models.docentry import DocEntry
from timdb.models.folder import Folder
from timdb.models.usergroup import UserGroup
from flask.helpers import send_file


from .common import *

qst_plugin = Blueprint('qst_plugin',
                       __name__,
                       url_prefix='')  # TODO: Better URL prefix.


@qst_plugin.route("/qst/reqs/")
def qst_reqs():
    reqs = {
        "type": "embedded",
        "js": ["/static/scripts/timHelper.js",
               "/static/scripts/controllers/qstController.js",
               # "/static/scripts/directives/dynamicAnswerSheet.js"
               ],
        # "css": [],dynami
        "angularModule": ["qstApp"],
        "multihtml": True,
    }

    return jsonResponse(reqs)


@qst_plugin.route("/qst/answer/", methods=["PUT"])
def qst_answer():
    jsondata = request.get_json()
    tim_info = {}
    answers = jsondata['input']['answers']
    spoints = jsondata['markup'].get('points')
    if spoints:
        points_table = create_points_table(spoints)
        points = calculate_points_from_json_answer(answers, points_table)
        tim_info["points"] = points
    info = jsondata['info']
    markup = jsondata['markup']
    result = False
    if info and info['max_answers'] and info['max_answers'] <= info.get('earlier_answers', 0) + 1:
        result = True
    if not result:
        markup = None

    save = answers
    web = {'result': "Vastattu", 'markup': markup, 'show_result': result, 'state': save}
    return jsonResponse({'save': save, 'web': web, "tim_info": tim_info})


@qst_plugin.route("/qst/multihtml/", methods=["POST"])
def qst_multihtml():
    jsondata = request.get_json()
    multi = []
    for jso in jsondata:
        multi.append(qst_get_html(jso))
    return jsonResponse(multi)


@qst_plugin.route("/qst/html/", methods=["POST"])
def qst_html():
    jsondata = request.get_json()

    html = qst_get_html(jsondata)
    return Response(html, mimetype="text/html")


def qst_get_html(jso):
    result = False
    info = jso['info']
    markup = jso['markup']
    if info and info['max_answers'] and info['max_answers'] <= info.get('earlier_answers', 0):
        result = True
    if not result:
        if 'points' in markup:
            del markup['points']
        if 'expl' in markup:
            del markup['expl']
    jso['show_result'] = result

    attrs = json.dumps(jso)

    hx = 'xxxHEXJSONxxx' + binascii.hexlify(attrs.encode("UTF8")).decode()
    attrs = hx
    runner = 'qst-runner'
    s = '<' + runner + '>' + attrs + '</' + runner + '>'
    return s
