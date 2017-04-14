"""Routes for qst (question) plugin."""
import binascii
import json
import yaml
import re

from flask import Blueprint
from flask import Response
from flask import abort
from flask import request

from containerLink import convert_md
from responsehelper import json_response
from documentmodel.document import Document
from plugin import parse_plugin_values, Plugin, PluginException
from sessioninfo import get_current_user_object
from requesthelper import verify_json_params;

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

    return json_response(reqs)


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
    convert_md(jsondata)
    info = jsondata['info']
    markup = jsondata['markup']
    result = False
    if info and info['max_answers'] and info['max_answers'] <= info.get('earlier_answers', 0) + 1:
        result = True
    if not result:
        markup = None

    save = answers
    web = {'result': "Vastattu", 'markup': markup, 'show_result': result, 'state': save}
    return json_response({'save': save, 'web': web, "tim_info": tim_info})


def is_review(request):
    # print(query)
    result = request.full_path.find("review=") >= 0
    return result


@qst_plugin.route("/qst/multihtml/", methods=["POST"])
def qst_multihtml():
    jsondata = request.get_json()
    multi = []
    for jso in jsondata:
        multi.append(qst_get_html(jso, is_review(request)))
    return json_response(multi)


@qst_plugin.route("/qst/qetQuestionMD/", methods=['POST'])
def get_question_md():
    doc_id, md = verify_json_params('docId', 'text')
    # md = verify_json_params('text')
    markup = json.loads(md)
    plugin_data = {}
    plugin_data['markup'] = markup;
    convert_md(plugin_data)

    return json_response({'md': plugin_data['markup']})


@qst_plugin.route("/qst/html/", methods=["POST"])
def qst_html():
    jsondata = request.get_json()

    html = qst_get_html(jsondata, is_review(request))
    return Response(html, mimetype="text/html")


def delete_key(d, key):
    if key in d:
        del d[key]


def set_explanation(markup):
    # if expl, change to 1-based, if xpl rename to expl
    if 'xpl' in markup:
        markup['expl'] = markup.pop('xpl')
        return
    if 'expl' not in markup:
        return
    expl = markup['expl']
    xpl = {}
    markup['expl'] = xpl
    for key in expl:
        try:
            n = int(key)
            xpl[n+1] = expl[key]
        except ValueError:
            pass

NOLAZY = "<!--nolazy-->"


def qst_str(state):
    s = str(state)
    s = s.replace("], ", "\n")
    s = s.replace("]", "")
    s = s.replace("[", "")
    s = s.replace("''", "-")
    s = s.replace("'", "")
    return s


def qst_get_html(jso, review):
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
        if 'xpl' in markup:
            del markup['xpl']
    else:
        set_explanation(markup)
    jso['show_result'] = result

    if review:
        usercode = qst_str(jso.get("state","-"))
        s = ""
        result = NOLAZY + '<div class="review" ng-non-bindable><pre>' + usercode + '</pre>' + s + '</div>'
        return result



    attrs = json.dumps(jso)

    hx = 'xxxHEXJSONxxx' + binascii.hexlify(attrs.encode("UTF8")).decode()
    attrs = hx
    runner = 'qst-runner'
    if markup.get('isQuestion',''):
        runner = 'question-runner'
    s = '<' + runner + '>' + attrs + '</' + runner + '>'
    return s


def question_convert_js_to_yaml(md):
    # save question. How to pick up question see lecture.py, get_question_data_from_document
    markup = json.loads(md)
    question_title = markup["json"]["questionTitle"]
    question_title = question_title.replace('"', '').replace("'", '')
    # taskid = questionTitle.replace(" ", "")  # TODO: make better conversion to ID
    taskid = question_title.replace("md:", "")
    taskid = re.sub('[^A-Za-z0-9]', '', taskid)
    oldid = markup.get('taskId')
    if not oldid:
        markup['taskId'] = taskid
    else:
        taskid = oldid
    qst = markup.get("qst", False)
    delete_key(markup, "question")  # old attribute
    delete_key(markup, "taskId")
    delete_key(markup, "qst")
    if 'expl' in markup:
        markup['xpl'] = markup.pop('expl')

    mdyaml = yaml.dump(markup, encoding='utf-8', allow_unicode=True, default_flow_style=False).decode(
        'utf-8')  # see also .safe_dump
    mdyaml = mdyaml.replace("\n\n", "\n")
    # mdyaml = mdyaml.replace("\\xC4", "Ä").replace("\\xD6", "Ö").replace(
    #    "\\xC5", "Å").replace("\\xE4", "ä").replace("\\xF6", "ö").replace("\\xE5", "å")
    prefix = ' '
    if qst:
        prefix = ' d'  # like document question
    # result = '``` {#' + taskid + prefix + 'question="' + question + '" plugin="qst"}\n' + mdyaml + '```\n'
    result = '``` {#' + taskid + prefix + 'question="' + 'true' + '" plugin="qst"}\n' + mdyaml + '```\n'
    return result


def get_question_data_from_document(doc_id, par_id, edit=False):
    '''
    Get markup for question
    :param doc_id: documtn id
    :param par_id: paragraph id
    :param edit: is purposu to edit data or show data
    :return: markup for question
    '''
    # pick up question from document, for saving question see edit.py question_convert_js_to_yam
    # par_id might be like 100.Ruksit.SpQA0NX2itOd  and one must cut the beginin
    i = par_id.rfind(".")
    if i >= 0:
        par_id = par_id[i + 1:]
    par = Document(doc_id).get_paragraph(par_id)
    try:
        plugin_values = Plugin.from_paragraph(par, user=get_current_user_object()).values
    except PluginException:
        return abort(400, 'Paragraph is not a plugin: {}'.format(par_id))
    plugindata = {'markup': plugin_values}
    if not edit:
        convert_md(plugindata)
    markup = plugindata.get('markup')
    markup["qst"] = not par.is_question()
    json = markup.get("json", None)
    if json:  # backward compability
        question_title = json.get("title", "")
        if question_title and not json.get("questionTitle", ""):
            json["questionTitle"] = question_title
        delete_key(json, "title")
        set_explanation(markup)
    attrs = par.get_attrs()
    if attrs:
        markup["taskId"] = attrs.get("taskId", "")
    # points = markup.get('points', '')
    # jso# n = markup.get('json')
    # expl = markup.get('expl', '')
    # markup['json'] = {}
    # return json, points, expl, markup
    return markup


def create_points_table(points):
    points_table = []
    if points and points != '':
        points_split = points.split('|')
        for row in points_split:
            row_points = row.split(';')
            row_points_dict = {}
            for col in row_points:
                if col != '':
                    col_points = col.split(':', 2)
                    if len(col_points) == 1:
                        row_points_dict[col_points[0]] = 1
                    else:
                        row_points_dict[col_points[0]] = float(col_points[1])
            points_table.append(row_points_dict)
    return points_table


def calculate_points_from_json_answer(single_answers, points_table):
    points = 0.0
    for (oneAnswer, point_row) in zip(single_answers, points_table):
        for oneLine in oneAnswer:
            if oneLine in point_row:
                points += point_row[oneLine]
    return points


def calculate_points(answer, points_table):
    # single_answers = []
    # all_answers = answer.split('|')
    # for answer in all_answers:
    #    single_answers.append(answer.split(','))

    single_answers = json.loads(answer)
    return calculate_points_from_json_answer(single_answers, points_table)
