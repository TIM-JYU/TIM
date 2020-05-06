"""Routes for qst (question) plugin."""
import json
import random
import re
from dataclasses import dataclass
from typing import Dict, Optional, List, Union, Any, TypeVar
from xml.sax.saxutils import quoteattr

import yaml
from flask import Blueprint
from flask import Response
from flask import abort
from flask import request
from marshmallow import missing, EXCLUDE

from markupmodels import GenericMarkupModel
from marshmallow_dataclass import class_schema
from pluginserver_flask import GenericAnswerModel
from timApp.auth.sessioninfo import get_current_user_object
from timApp.document.docinfo import DocInfo
from timApp.lecture.askedjson import normalize_question_json
from timApp.markdown.dumboclient import DumboOptions
from timApp.plugin.containerLink import convert_md
from timApp.plugin.containerLink import prepare_for_dumbo_attr_list_recursive, get_plugin_regex_obj
from timApp.plugin.plugin import Plugin, PluginException
from timApp.plugin.plugin import get_num_value
from timApp.plugin.plugin import get_value
from timApp.tim_app import csrf
from timApp.util.flask.requesthelper import verify_json_params, use_model
from timApp.util.flask.responsehelper import json_response
from utils import Missing

qst_plugin = Blueprint('qst_plugin',
                       __name__,
                       url_prefix='')  # TODO: Better URL prefix.


@dataclass
class QuestionInDocument:
    markup: Dict
    qst: bool
    taskId: Optional[str]
    docId: int
    parId: str
    isPreamble: bool


@qst_plugin.route("/qst/mcq/reqs")
def qst_mcq_reqs():
    reqs = {
        "type": "embedded",
        "js": ["qst"],
        "multihtml": True,
        "multimd": True
    }

    return json_response(reqs)


@qst_plugin.route("/qst/mmcq/reqs")
def qst_mmcq_reqs():
    reqs = {
        "type": "embedded",
        "js": ["qst"],
        "multihtml": True,
        "multimd": True
    }

    return json_response(reqs)


@qst_plugin.route("/qst/reqs")
def qst_reqs():
    reqs = {
        "type": "embedded",
        "js": ["qst"],
        "multihtml": True,
        "multimd": True
    }

    return json_response(reqs)


@qst_plugin.route("/qst/mcq/answer", methods=["PUT"])
@csrf.exempt
def qst_mcq_answer():
    jsondata = request.get_json()
    convert_mcq_to_qst(jsondata)
    return qst_answer_jso(AnswerSchema().load(jsondata))


@qst_plugin.route("/qst/mmcq/answer", methods=["PUT"])
@csrf.exempt
def qst_mmcq_answer():
    jsondata = request.get_json()
    convert_mcq_to_qst(jsondata, True)
    return qst_answer_jso(AnswerSchema().load(jsondata))


@dataclass
class QstInputModel:
    answers: List[List[str]]
    nosave: Union[bool, Missing] = missing


@dataclass
class QstMarkupModel(GenericMarkupModel):
    class Meta:
        unknown = EXCLUDE

    points: Union[str, Missing] = missing
    minpoints: Union[float, Missing] = missing
    maxpoints: Union[float, Missing] = missing
    # TODO: Set proper types
    answerFieldType: Union[str, None, Missing] = missing
    expl: Union[Any, Missing] = missing
    headers: Union[Any, Missing] = missing
    matrixType: Union[str, None, Missing] = missing
    questionText: Union[str, None, Missing] = missing
    questionTitle: Union[str, None, Missing] = missing
    questionType: Union[str, None, Missing] = missing
    rows: Union[Any, Missing] = missing
    randomizedRows: Union[int, Missing] = missing
    randomSeed: Union[int, Missing] = missing
    doNotMove: Union[List[int], int, Missing] = missing


QstBasicState = List[List[str]]


@dataclass
class QstRandomState:
    c: QstBasicState
    order: List[int]


# Store answer in original row order if no randomizedRows specified in markup:
# [["1"], [], ["1"], ["2"]]
# Otherwise specify order in which rows were presented
# {"c": [["1"], ["2"], ["3"], []], "order": [3, 7, 4, 5]}
QstStateModel = Union[QstBasicState, QstRandomState]


@dataclass
class QstAnswerModel(GenericAnswerModel[QstInputModel, QstMarkupModel, QstStateModel]):
    pass


AnswerSchema = class_schema(QstAnswerModel)


@qst_plugin.route("/qst/answer", methods=["PUT"])
@csrf.exempt
@use_model(QstAnswerModel)
def qst_answer(m):
    return qst_answer_jso(m)


def qst_filter_markup_points(points: str, question_type: str, rand_arr: List[int]) -> str:
    """
    filter markup's points field based on pre-generated array
    """
    # TODO: Use constants
    if question_type == 'true-false' or question_type == 'matrix':
        # point format 1:1;2:-0.5|1:-0.5;2:1 where | splits rows input, ; column input
        ret = points.split('|')
        ret = qst_set_array_order(ret, rand_arr)
        ret = '|'.join(ret)
    else:
        # point format 1:1;2:-0.5;3:-0.5 where ; splits row input
        ret = create_points_table(points)[0]
        ret = qst_pick_expls(ret, rand_arr)
        ret = ';'.join(str(key) + ':' + str(val) for [key, val] in ret.items())
    return ret


def qst_answer_jso(m: QstAnswerModel):
    tim_info = {}
    answers = m.input.answers
    spoints = m.markup.points
    markup = m.markup
    info = m.info
    rand_arr = None
    prev_state = m.state
    # if prev state exists, try to get order from there
    if isinstance(prev_state, QstRandomState):
        rand_arr = prev_state.order
    # if prev state is none, check if markup wants random order
    if prev_state is None and rand_arr is None and m.markup.randomizedRows and info and info.user_id:
        random_seed = m.markup.randomSeed
        if random_seed is None or random_seed is missing:
            random_seed = 0
        locks = m.markup.doNotMove
        if locks is None or locks is missing:
            locks = []
        if isinstance(locks, int):
            locks = [locks]
        seed_string = info.user_id + m.taskID
        rand_arr = qst_rand_array(len(m.markup.rows), m.markup.randomizedRows, seed_string, random_seed, locks)
    if spoints:
        if rand_arr:
            question_type = m.markup.questionType
            spoints = qst_filter_markup_points(spoints, question_type, rand_arr)
            m.markup.points = spoints
        points_table = create_points_table(spoints)
        points = calculate_points_from_json_answer(answers, points_table)
        minpoints = markup.minpoints if markup.minpoints is not missing else -1e20
        maxpoints = markup.maxpoints if markup.maxpoints is not missing else 1e20
        if points < minpoints:
            points = minpoints
        if points > maxpoints:
            points = maxpoints
        tim_info["points"] = points

    webstate = answers
    if rand_arr is not None:
        # TODO: Schema?
        answers = {'c': answers, 'order': rand_arr}
        m.markup.rows = qst_set_array_order(m.markup.rows, rand_arr)
        m.markup.expl = qst_pick_expls(m.markup.expl, rand_arr)

    result = False
    if info and info.max_answers and info.max_answers <= info.earlier_answers + 1 and prev_state != answers:
        result = True
    jsonmarkup = m.markup.get_visible_data()
    convert_md([jsonmarkup], options=DumboOptions.default())  # TODO get mathtype from doc settings?
    save = answers
    if not markup.show_points():
        jsonmarkup.pop('expl', None)
        jsonmarkup.pop('points', None)
    web = {'result': "Vastattu", 'markup': jsonmarkup if result else None, 'show_result': result, 'state': webstate}
    return json_response({'save': save, 'web': web, "tim_info": tim_info})


def is_review(jso):
    result = jso.get("review", False)
    return result


@qst_plugin.route("/qst/multihtml", methods=["POST"])
@csrf.exempt
def qst_multihtml():
    jsondata = request.get_json()
    multi = []
    for jso in jsondata:
        multi.append(qst_get_html(jso, is_review(jso)))
    return json_response(multi)


def copy_value(name, qjso, jsom, defvalue=None):
    v = jsom.get(name, defvalue)
    if v:
        qjso[name] = v


def convert_mcq_to_qst(jso, is_mmcq=False):
    jsom = jso.get("markup")
    qjso = {
        "questionText": jsom.get("stem", ""),
        "questionTitle": jsom.get("stem", ""),
        "stem": jsom.get("headerText", "Check your understanding"),
        "answerFieldType": "radio",
        "isTask": True,
    }
    jso["markup"] = qjso
    copy_value("answerLimit", qjso, jsom, "1")
    copy_value("header", qjso, jsom)
    copy_value("footer", qjso, jsom)
    copy_value("button", qjso, jsom)
    copy_value("buttonText", qjso, jsom)
    if is_mmcq:
        qjso["questionType"] = "true-false"
        true_text = jsom.get("trueText", "True")
        false_text = jsom.get("falseText", "False")
        qjso["headers"] = [true_text, false_text]
        state = jso.get("state", None)
        if isinstance(state, list) and state:
            item = state[0]
            if not isinstance(item, list):
                new_state = []
                for item in state:
                    new_item = []
                    if item == True:
                        new_item = ["1"]
                    if item == False:
                        new_item = ["2"]
                    new_state.append(new_item)
                jso["state"] = new_state
    else:
        qjso["questionType"] = "radio-vertical"
        qjso["headers"] = []
        state = jso.get("state", None)
        if isinstance(state, int):
            jso["state"] = [[str(state + 1)]]

    rows = []
    qjso["rows"] = rows
    choices = jsom.get("choices", [])
    for c in choices:
        rows.append(c.get("text", ""))
    expl = {}
    qjso["expl"] = expl
    i = 1
    for c in choices:
        expl[str(i)] = c.get("reason", "")
        i += 1
    points = ""
    sep = ""
    i = 1
    for c in choices:
        cor = c.get("correct", False)
        if is_mmcq:
            if cor:
                points += sep + "1:1"
            else:
                points += sep + "2:1"
            sep = "|"
        else:
            if cor:
                points = str(i) + ":1"
                break
        i += 1

    qjso["points"] = points


@qst_plugin.route("/qst/mcq/multihtml", methods=["POST"])
@csrf.exempt
def qst__mcq_multihtml():
    jsondata = request.get_json()
    multi = []
    for jso in jsondata:
        convert_mcq_to_qst(jso)
        multi.append(qst_get_html(jso, is_review(jso)))
    return json_response(multi)


@qst_plugin.route("/qst/mmcq/multihtml", methods=["POST"])
@csrf.exempt
def qst__mmcq_multihtml():
    jsondata = request.get_json()
    multi = []
    for jso in jsondata:
        convert_mcq_to_qst(jso, True)
        multi.append(qst_get_html(jso, is_review(jso)))
    return json_response(multi)


@qst_plugin.route("/qst/multimd", methods=["POST"])
@csrf.exempt
def qst_multimd():
    jsondata = request.get_json()
    multi = []
    for jso in jsondata:
        multi.append(qst_get_md(jso))
    return json_response(multi)


@qst_plugin.route("/qst/mcq/multimd", methods=["POST"])
@csrf.exempt
def qst_mcq_multimd():
    jsondata = request.get_json()
    multi = []
    for jso in jsondata:
        multi.append(mcq_get_md(jso))
    return json_response(multi)


@qst_plugin.route("/qst/mmcq/multimd", methods=["POST"])
@csrf.exempt
def qst_mmcq_multimd():
    jsondata = request.get_json()
    multi = []
    for jso in jsondata:
        multi.append(mmcq_get_md(jso))
    return json_response(multi)


@qst_plugin.route("/qst/getQuestionMD/", methods=['POST'])
def get_question_md():
    md, = verify_json_params('text')
    markup = json.loads(md)
    plugin_data = {'markup': markup}
    prepare_for_dumbo_attr_list_recursive(get_plugin_regex_obj('qst'), plugin_data)
    convert_md([plugin_data], options=DumboOptions.default())

    return json_response({'md': plugin_data['markup']})


@qst_plugin.route("/qst/html/", methods=["POST"])
@csrf.exempt
def qst_html():
    jsondata = request.get_json()

    html = qst_get_html(jsondata, is_review(jsondata))
    return Response(html, mimetype="text/html")


@qst_plugin.route("/qst/<path:path>")
def not_found(path):
    """This 404 route is required because qst plugin is in the same process - otherwise calling a non-existent qst route
    results in a weird request loop, at least when running locally.
    """
    return abort(404)


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
            xpl[n + 1] = expl[key]
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


def mcq_get_md(jso):
    """
    Gives question in format:
        \mcq{Onko kuu}{Valitse tosi lause}
        {|l l|}
        {
        \hline $\bigcirc$ & Kuu on lähempänä kuin aurinko \\
        \hline $\bigcirc$ & Kuu on yhtä iso kuin aurinko \\
        \hline $\bigcirc$ & Kuu on juustoa \\
        \hline
        }

    :param jso: json block to make the markdown or TeX
    """
    markup = jso['markup']
    printlikeqst = get_value(markup, 'texprintlikeqst', 0)
    user_print = jso.get('userPrint', False)

    header = markup.get('header', markup.get('headerText', 'Check your understanding'))
    footer = markup.get('footer', '')
    stem = markup.get('stem', '')
    texcolumns = markup.get('texcolumns', '|l l|' if not printlikeqst else 'l L')
    user_answer = jso.get('state', None)
    if user_answer is None:
        user_print = False
    if user_print:
        texcolumns = markup.get('texusercolumns', '|l l l|' if not printlikeqst else 'l L L')
    texhline = markup.get('texhline', '\\hline' if not printlikeqst else '')
    if texhline:
        texhline += ' '
    choices = markup.get('choices', [])
    cstr = ''
    idx = 0
    for choice in choices:
        user_mark = ''
        correct = choice.get('correct', False)
        reason = ''
        checked = ' '
        if user_print:
            reason = ' & ' + choice.get('reason', '')
            if idx == user_answer:
                user_mark = ' \, \\cmark' if correct else ' \, \\xmark'
                checked = '*'
        line = texhline + '\\radiobutton' + checked + user_mark + '& ' + choice.get('text', '') + reason + ' \\\\\n'
        cstr += line
        idx += 1

    if cstr and texhline:
        cstr += texhline + '\n'

    result = format_qst_or_mcq(printlikeqst, header, stem, texcolumns, cstr, footer)

    return result


def format_qst_or_mcq(printlikeqst, header, stem, texcolumns, cstr, footer):
    if printlikeqst:
        result = format_qst_tex(header, stem, '', texcolumns, cstr, footer)
    else:
        result = format_mcq_tex(header, stem, texcolumns, cstr)
    return result


def format_mcq_tex(header, stem, texcolumns, cstr):
    # IMPORTANT: only add linebreaks when there are open curly braces. Required for Pandoc to parse TeX correctly.
    return f'''
\\mcq{{{header}}}{{{stem}}}{{{texcolumns}}}{{
{cstr}
}}
'''


def format_qst_tex(header, stem, question_text, texcolumns, cstr, footer):
    # IMPORTANT: only add linebreaks when there are open curly braces. Required for Pandoc to parse TeX correctly.
    return f'''
\\qsty{{{header}}}{{{stem}}}{{{question_text}}}{{{texcolumns}}}{{
{cstr}
}}{{
{footer}
}}
'''


def mmcq_get_md(jso):
    """
    Gives question in format:
        \mcq{Onko kuu}{Valitse tosi lause}
        {|l l|}
        {
        \hline $\bigcirc$ & Kuu on lähempänä kuin aurinko \\
        \hline $\bigcirc$ & Kuu on yhtä iso kuin aurinko \\
        \hline $\bigcirc$ & Kuu on juustoa \\
        \hline
        }

    :param jso: json block to make the markdown or TeX
    """
    markup = jso['markup']
    user_print = jso.get('userPrint', False)
    printlikeqst = get_value(markup, 'texprintlikeqst', 0)

    header = markup.get('header', markup.get('headerText', 'Check your understanding'))
    footer = markup.get('footer', '')
    stem = markup.get('stem', '')
    true_text = markup.get('trueText', 'True')
    false_text = markup.get('falseText', 'False')
    correct_mark = markup.get('texcorrectText', '\\cmark')
    wrong_mark = markup.get('texwrongText', '\\xmark')

    texcolumns = markup.get('texcolumns', '|l c c|' if not printlikeqst else 'L c c')
    user_answer = jso.get('state', [])
    if not user_answer:
        user_print = False
    if user_print:
        texcolumns = markup.get('texusercolumns', '|l c c c l|' if not printlikeqst else 'L c c c L')
    texhline = markup.get('texhline', '\\hline' if not printlikeqst else '')
    if texhline:
        texhline += ' '
    choices = markup.get('choices', [])
    reason = ' & ' + ' ' + ' & ' + ' ' if user_print else ''
    cstr = texhline + ' & ' + true_text + ' & ' + false_text + reason + ' \\\\\n'
    idx = 0
    for choice in choices:
        correct = choice.get('correct', False)
        reason = ''
        leftbox = '\\checkbox'
        rightbox = '\\checkbox'
        if user_print and user_answer:
            ua = None
            if len(user_answer) > idx:
                ua = user_answer[idx]
            correct_or_wrong_mark = correct_mark if ua == correct else wrong_mark
            if ua:
                leftbox += '*'
            # noinspection PySimplifyBooleanCheck
            if ua == False:   # do not replace with not ua because it can be None
                rightbox += '*'
            reason = ' & & '
            if ua is not None:
                reason = ' & ' + correct_or_wrong_mark + ' & ' + choice.get('reason', '')
        line = texhline + choice.get('text', '') + '& ' + leftbox + ' & ' + rightbox + reason + ' \\\\\n'
        cstr += line
        idx += 1

    if cstr and texhline:
        cstr += texhline + '\n'

    result = format_qst_or_mcq(printlikeqst, header, stem, texcolumns, cstr, footer)
    return result


qst_attrs = {
    'answerLimit',
    'button',
    'buttonText',
    'footer',
    'header',
    'isTask',
    'lazy',
    'resetText',
    'stem',
    'hideBrowser',
    'tag'
}


def qst_rand_array(max_count: int,
                   randoms: int,
                   seed_word: str,
                   random_seed: int = 0,
                   locks: Optional[List[int]] = None) -> List[int]:
    """
    get array of count integers between 1 and max_count (incl.) using word and extra number as seed
    :param max_count: highest possible number (incl.) and max return list length
    :param randoms: how many random numbers to fill the array with
    :param seed_word: input word to generate random seed
    :param random_seed: extra number to edit the seed
    :param locks: positions that can't be shuffled, indexing starting from 1. Any position over max_count will be
    interpreted as max_count
    :return: shuffled array of integers of up to max_count values
    """
    if locks is None:
        locks = []
    for i, val in enumerate(locks):
        if val > max_count:
            locks[i] = max_count
        elif val < 1:
            locks[i] = 1
    locks = list(set(locks))
    locks.sort()
    total = randoms + len(locks)
    if total > max_count:
        total = max_count
    ret = []
    seed_array = []
    orig = list(range(1, max_count + 1))
    for i, val in enumerate(locks):
        if len(orig) == 0:
            break
        if val > max_count:
            orig.pop(len(orig) - 1)
        try:
            orig.pop(val - 1 - i)
        except IndexError:
            pass
    # Temp seed generator
    for char in seed_word:
        try:
            seed_array.append(int(char, 36))
        except ValueError:
            pass
    seed = int(''.join(map(str, seed_array)))
    random.seed(seed + random_seed)
    random.shuffle(orig)
    for i in range(1, total + 1):
        if len(locks) >= total - len(ret):
            ret.append(locks[0])
            locks.pop(0)
        elif len(locks) > 0 and locks[0] == i:
            ret.append(i)
            locks.pop(0)
        else:
            ret.append(orig.pop(0))
    return ret


T = TypeVar('T')


def qst_set_array_order(arr: List[T], order_array: List[int]) -> List[T]:
    """
    pick items from arr in order given by order_array
    indices start from 1
    """
    ret = []
    for val in order_array:
        try:
            ret.append(arr[val - 1])
        except IndexError:
            pass
    return ret


def qst_pick_expls(orig_expls: Dict[str, T], order_array: List[int]) -> Dict[str, T]:
    """
    pick items from dict where keys are str converted integers in order given by order_array
    indices start from 1
    """
    ret = {}
    for i, val in enumerate(order_array):
        pos = str(val)
        picked = orig_expls.get(pos, None)
        if picked is not None:
            ret[str(i + 1)] = picked
    return ret


def qst_get_html(jso, review):
    result = False
    info = jso['info']
    markup = jso['markup']
    rand_arr = None
    prev_state = jso.get('state', None)
    if prev_state and isinstance(prev_state, dict):
        rand_arr = prev_state.get('order')
        jso['state'] = prev_state.get('c')
    rows = markup.get('rows', [])
    if not prev_state and rand_arr is None:  # no previous answer, check markup for new order
        rcount = markup.get('randomizedRows', 0)
        # TODO: try to convert string
        if not isinstance(rcount, int):
            rcount = 0
        if rcount is None:
            rcount = 0
        if rcount > 0:
            # markup['rows'] = qst_randomize_rows(rows,rcount,jso['user_id'])
            random_seed = markup.get('randomSeed', 0)
            # TODO: use random seed generation within qst_rand_array if seed was string
            if not isinstance(random_seed, int):
                random_seed = 0
            locks = markup.get('doNotMove', [])
            # TODO: MarkupModel should handle these checks?
            if locks is None:
                locks = []
            if isinstance(locks, int):
                locks = [locks]
            for val in locks:
                if not isinstance(val, int):
                    locks = []
                    break
            if random_seed is None:
                random_seed = 0
            seed_string = jso.get('user_id', "") + jso.get('taskID', "")
            rand_arr = qst_rand_array(len(rows), rcount, seed_string, random_seed, locks)
    if rand_arr is not None:  # specific order found in prev.ans or markup
        markup['rows'] = qst_set_array_order(rows, rand_arr)
        markup['expl'] = qst_pick_expls(markup['expl'], rand_arr)

    markup = normalize_question_json(markup,
                                     allow_top_level_keys=
                                     qst_attrs)
    jso['markup'] = markup
    if info and info['max_answers'] \
            and get_num_value(info, 'max_answers', 1) <= get_num_value(info, 'earlier_answers', 0):
        result = True
    if not result:
        markup.pop('points', None)
        markup.pop('expl', None)
    elif rand_arr is not None:
        points = markup.get('points')
        if points:
            question_type = markup.get('questionType')
            points = qst_filter_markup_points(points, question_type, rand_arr)
            markup['points'] = points

    jso['show_result'] = result

    if review:
        usercode = qst_str(jso.get("state", "-"))
        s = ""
        result = NOLAZY + '<div class="review" ng-non-bindable><pre>' + usercode + '</pre>' + s + '</div>'
        return result

    attrs = json.dumps(jso, sort_keys=True)

    runner = 'qst-runner'
    s = f'<{runner} json={quoteattr(attrs)}></{runner}>'
    return s


def qst_get_md(jso):
    result = False
    info = jso['info']
    markup = jso['markup']
    points_table = create_points_table(markup.get('points'))

    set_explanation(markup)
    jso['show_result'] = result

    # attrs = json.dumps(jso)
    user_print = jso.get('userPrint', False)

    print_reason = get_num_value(info, 'max_answers', 1) <= get_num_value(info, 'earlier_answers', 0)

    header = markup.get('header', '')
    footer = markup.get('footer', '')
    texboxw = markup.get('texboxw', '0.2')
    texboxh = markup.get('texboxh', '10')
    stem = markup.get('stem', '')
    stem += '\par' if stem else ''
    qjson = markup  # .get('json',{})
    question_text = qjson.get('questionText', '')
    question_type = qjson.get('questionType', '')
    vertical = question_type.find('vertical') >= 0
    headers = qjson.get('headers', [])

    texcolumns = markup.get('texcolumns', 'l L' if vertical else '')
    user_answer = jso.get('state', [])
    if not user_answer:
        user_print = False
    if user_print:
        texcolumns = markup.get('texusercolumns', 'l L L' if vertical else '')
    texhline = markup.get('texhline', '')
    if texhline:
        texhline += ' '
    rows = qjson.get('rows', [])
    cstr = ''
    expl = markup.get('expl', {})
    empty_theader = True
    theader = ' &'
    sep = ''
    tc = ' '
    for h in headers:
        if h:
            empty_theader = False
        theader += sep + h
        sep = ' & '
        tc += 'c '
    if user_print:
        if not texcolumns:
            texcolumns = 'L' + tc + 'L'
    else:
        if not texcolumns:
            texcolumns = 'L' + tc

    aft = qjson.get('answerFieldType', '')
    boxdefault = ''
    leftbox = '\\radiobutton'
    if aft == 'checkbox':
        leftbox = '\\checkbox'
    if aft == 'text':
        leftbox = '\\ttextbox{' + str(texboxw) + '}'
        boxdefault = '{\\vspace{' + str(texboxh) + 'mm}}'

    idx = 0
    for row in rows:
        if type(row) is not str:
            continue
        exp = expl.get(str(idx + 1), '')
        lbox = ''
        sep = ''
        lh = len(headers)
        if lh == 0:
            lh = 1
        for i in range(0, lh):
            box = leftbox
            uidx = 0 if vertical else idx
            ui = idx if vertical else i
            cell_points = 0
            boxdef = boxdefault
            if user_print and user_answer:
                ua = uidx < len(user_answer) and str(ui + 1) in user_answer[uidx]
                if len(points_table) > uidx:
                    cell_points = get_num_value(points_table[uidx], str(ui + 1), 0)
                box = leftbox
                if aft == 'text':
                    if idx < len(user_answer):
                        uar = user_answer[idx]
                        if i < len(uar):
                            ua = uar[i]
                            if ua:
                                boxdef = '{' + ua + '}'
                    box += boxdef
                else:
                    if ua:
                        box += '*'
                    if cell_points > 0:
                        box = "\correct{" + box + "}"
                    if cell_points:
                        box = "\lbpoints{" + str(cell_points) + "}{" + box + "}"
            else:
                box += boxdef
            lbox += sep + box
            sep = " & "
        reason = ' '
        if user_print and print_reason:
            reason = ' & ' + exp
        if vertical:
            line = texhline + lbox + ' & ' + row + reason + ' \\\\\n'
        else:
            line = texhline + row + ' & ' + lbox + reason + ' \\\\\n'
        cstr += line
        idx += 1

    if cstr and texhline:
        cstr += texhline + '\n'

    if not empty_theader:
        cstr = theader + ' \\\\\n' + texhline + '\n' + cstr

    result = format_qst_tex(header, stem, question_text, texcolumns, cstr, footer)
    return result


def question_convert_js_to_yaml(markup: Dict, is_task: bool, task_id: Optional[str]):
    # save question. How to pick up question see lecture.py, get_question_data_from_document
    markup = normalize_question_json(markup, allow_top_level_keys=qst_attrs)
    question_title = markup["questionTitle"]
    question_title = question_title.replace('"', '').replace("'", '')
    # taskid = questionTitle.replace(" ", "")  # TODO: make better conversion to ID
    taskid = question_title.replace("md:", "")
    taskid = re.sub('[^A-Za-z0-9]', '', taskid)
    if task_id:
        taskid = task_id
    qst = is_task
    markup.pop('question', None)  # old attribute
    markup.pop('taskId', None)
    markup.pop('qst', None)

    mdyaml = yaml.dump(markup, encoding='utf-8', allow_unicode=True, default_flow_style=False).decode(
        'utf-8')  # see also .safe_dump
    mdyaml = mdyaml.replace("\n\n", "\n")
    prefix = ' '
    if qst:
        prefix = ' d'  # like document question
    result = '``` {#' + taskid + prefix + 'question="' + 'true' + '" plugin="qst"}\n' + mdyaml + '```\n'
    return result


def get_question_data_from_document(d: DocInfo, par_id: str, edit=False) -> QuestionInDocument:
    """
    Get markup for question

    :param d: document
    :param par_id: paragraph id
    :param edit: is purposu to edit data or show data
    :return: markup for question
    """
    # pick up question from document, for saving question see edit.py question_convert_js_to_yam
    # par_id might be like 100.Ruksit.SpQA0NX2itOd  and one must cut the beginin
    i = par_id.rfind(".")
    if i >= 0:
        par_id = par_id[i + 1:]
    d.document.insert_preamble_pars()
    par = d.document.get_paragraph(par_id)
    try:
        plugin_values = Plugin.from_paragraph(par, user=get_current_user_object()).values
    except PluginException:
        return abort(400, f'Paragraph is not a plugin: {par_id}')
    plugindata = {'markup': plugin_values}
    markup = plugindata.get('markup')
    if markup.get("choices", None):
        convert_mcq_to_qst(plugindata, par.get_attr('plugin', '').find('mmcq') == 0)
    if not edit or edit == 'false':
        settings = d.document.get_settings()
        prepare_for_dumbo_attr_list_recursive(get_plugin_regex_obj('qst'), plugindata)
        convert_md([plugindata], options=par.get_dumbo_options(base_opts=settings.get_dumbo_options()))
    markup = plugindata.get('markup')
    return QuestionInDocument(
        markup=normalize_question_json(markup, allow_top_level_keys=qst_attrs),
        qst=not par.is_question(),
        taskId=par.get_attr('taskId'),
        docId=d.id,
        parId=par_id,
        isPreamble=bool(par.from_preamble()),
    )


def create_points_table(points):
    points_table = []
    if points and points != '':
        points = str(points)
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


def calculate_points_from_json_answer(single_answers: List[List[str]], points_table):
    points = 0.0
    for (oneAnswer, point_row) in zip(single_answers, points_table):
        for oneLine in oneAnswer:
            if oneLine in point_row:
                points += point_row[oneLine]
    return points


def calculate_points(answer, points_table):
    single_answers = json.loads(answer)
    return calculate_points_from_json_answer(single_answers, points_table)
