"""Routes for qst (question) plugin."""
import json
import re
from dataclasses import dataclass, asdict
from typing import Union, Any

import yaml
from flask import Blueprint, render_template_string
from flask import Response
from flask import request
from marshmallow import missing, EXCLUDE, ValidationError

from timApp.auth.sessioninfo import get_current_user_object
from timApp.document.docinfo import DocInfo
from timApp.document.usercontext import UserContext
from timApp.document.viewcontext import default_view_ctx
from timApp.lecture.askedjson import normalize_question_json
from timApp.lecture.question_utils import (
    qst_set_array_order,
    qst_pick_expls,
    qst_rand_array,
    qst_handle_randomization,
    calculate_points_from_json_answer,
    create_points_table,
    qst_filter_markup_points,
)
from timApp.plugin.containerLink import convert_md
from timApp.plugin.containerLink import (
    prepare_for_dumbo_attr_list_recursive,
    get_plugin_regex_obj,
)
from timApp.plugin.plugin import Plugin, PluginException
from timApp.plugin.plugin import get_num_value
from timApp.plugin.plugin import get_value
from timApp.tim_app import csrf
from timApp.util.flask.requesthelper import (
    verify_json_params,
    use_model,
    RouteException,
    NotExist,
)
from timApp.util.flask.responsehelper import json_response
from timApp.util.utils import get_dataclass_field_names
from tim_common.dumboclient import DumboOptions
from tim_common.markupmodels import GenericMarkupModel
from tim_common.marshmallow_dataclass import class_schema
from tim_common.pluginserver_flask import (
    GenericAnswerModel,
    GenericHtmlModel,
    render_validationerror,
    make_base64,
)
from tim_common.timjsonencoder import TimJsonEncoder
from tim_common.utils import Missing

qst_plugin = Blueprint(
    "qst_plugin", __name__, url_prefix=""
)  # TODO: Better URL prefix.


@dataclass
class QuestionInDocument:
    markup: dict
    qst: bool
    taskId: str | None
    docId: int
    parId: str
    isPreamble: bool


@qst_plugin.get("/qst/mcq/reqs")
def qst_mcq_reqs():
    reqs = {"type": "embedded", "js": ["qst"], "multihtml": True, "multimd": True}

    return json_response(reqs)


@qst_plugin.get("/qst/mmcq/reqs")
def qst_mmcq_reqs():
    reqs = {"type": "embedded", "js": ["qst"], "multihtml": True, "multimd": True}

    return json_response(reqs)


@qst_plugin.get("/qst/reqs")
def qst_reqs():
    reqs = {"type": "embedded", "js": ["qst"], "multihtml": True, "multimd": True}

    return json_response(reqs)


@qst_plugin.put("/qst/mcq/answer")
@csrf.exempt
def qst_mcq_answer():
    jsondata = request.get_json()
    convert_mcq_to_qst(jsondata)
    return qst_answer_jso(AnswerSchema().load(jsondata))


@qst_plugin.put("/qst/mmcq/answer")
@csrf.exempt
def qst_mmcq_answer():
    jsondata = request.get_json()
    convert_mcq_to_qst(jsondata, True)
    return qst_answer_jso(AnswerSchema().load(jsondata))


@dataclass
class QstInputModel:
    answers: list[list[str]]
    nosave: bool | Missing = missing


@dataclass
class QstMarkupModel(GenericMarkupModel):
    class Meta:
        unknown = EXCLUDE

    points: str | Missing = missing
    minpoints: float | Missing = missing
    maxpoints: float | Missing = missing
    # TODO: Set proper types
    answerFieldType: str | None | Missing = missing
    expl: Any | Missing = missing
    headers: Any | Missing = missing
    matrixType: str | None | Missing = missing
    questionText: str | None | Missing = missing
    questionTitle: str | None | Missing = missing
    questionType: str | None | Missing = missing
    rows: list[Any] | Missing = missing
    randomizedRows: int | None | Missing = missing
    randomSeed: int | Missing = missing
    doNotMove: list[int] | int | None | Missing = missing
    defaultPoints: int | float | None | Missing = missing
    savedText: str | None | Missing = missing
    size: str | None | Missing = missing
    customHeader: str | None | Missing = missing
    autosave: bool | None | Missing = missing


QstBasicState = list[list[str]]


@dataclass
class QstRandomState:
    c: QstBasicState
    order: list[int]


# Store answer in original row order if no randomizedRows specified in markup:
# [["1"], [], ["1"], ["2"]]
# Otherwise specify order in which rows were presented
# {"c": [["1"], ["2"], ["3"], []], "order": [3, 7, 4, 5]}
QstStateModel = Union[QstBasicState, QstRandomState]


@dataclass
class QstAnswerModel(GenericAnswerModel[QstInputModel, QstMarkupModel, QstStateModel]):
    pass


AnswerSchema = class_schema(QstAnswerModel)


@qst_plugin.put("/qst/answer")
@csrf.exempt
@use_model(QstAnswerModel)
def qst_answer(m):
    return qst_answer_jso(m)


def qst_answer_jso(m: QstAnswerModel):
    tim_info = {}
    answers = m.input.answers
    spoints = m.markup.points
    default_points = m.markup.defaultPoints
    markup = m.markup
    info = m.info
    rand_arr = None
    prev_state = m.state
    # if prev state exists, try to get order from there
    if isinstance(prev_state, QstRandomState):
        rand_arr = prev_state.order
    # if prev state is none, check if markup wants random order
    if (
        prev_state is None
        and rand_arr is None
        and m.markup.randomizedRows
        and info
        and info.user_id
    ):
        random_seed = m.markup.randomSeed
        if random_seed is None or random_seed is missing:
            random_seed = 0
        locks = m.markup.doNotMove
        if locks is None or locks is missing:
            locks = []
        if isinstance(locks, int):
            locks = [locks]
        seed_string = info.user_id + m.taskID
        rand_arr = qst_rand_array(
            len(m.markup.rows), m.markup.randomizedRows, seed_string, random_seed, locks
        )
    if spoints or default_points:
        if spoints:
            if rand_arr:
                question_type = m.markup.questionType
                spoints = qst_filter_markup_points(spoints, question_type, rand_arr)
                m.markup.points = spoints
            points_table = create_points_table(spoints)
        else:
            points_table = None
        points = calculate_points_from_json_answer(
            answers, points_table, default_points
        )
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
        answers = {"c": answers, "order": rand_arr}
        m.markup.rows = qst_set_array_order(m.markup.rows, rand_arr)
        m.markup.expl = qst_pick_expls(m.markup.expl, rand_arr)

    result = False
    if (
        info
        and (
            (info.max_answers and info.max_answers <= info.earlier_answers + 1)
            or not info.max_answers
        )
        and prev_state != answers
    ):
        result = True
    jsonmarkup = m.markup.get_visible_data()
    convert_qst_md(jsonmarkup)  # TODO get mathtype from doc settings?
    save = answers
    show_points = (
        info.show_points if info is not None and info.show_points is not None else True
    )
    if not show_points:
        jsonmarkup.pop("expl", None)
        jsonmarkup.pop("points", None)
        jsonmarkup.pop("defaultPoints", None)

    # TODO: Qst sends entire plugin markup for building the ui
    #  -> Should send only attributes used by qst ui
    jsonmarkup.pop("modelAnswer", None)

    savedText = "Saved"  # markup.savedText or "Saved"

    web = {
        "result": savedText,
        "markup": jsonmarkup if result else None,
        "show_result": result,
        "state": webstate,
    }
    return json_response({"save": save, "web": web, "tim_info": tim_info})


def is_review(jso):
    result = jso.get("review", False)
    return result


@dataclass
class QstHtmlModel(GenericHtmlModel[QstInputModel, QstMarkupModel, QstStateModel]):
    def get_component_html_name(self) -> str:
        return "tim-qst"

    def get_static_html(self) -> str:
        return render_static_qst(self)


QstHtmlSchema = class_schema(QstHtmlModel)


@qst_plugin.post("/qst/multihtml")
@csrf.exempt
def qst_multihtml():
    jsondata = request.get_json()
    schema = QstHtmlSchema()
    multi = []
    for jso in jsondata:
        try:
            p = schema.load(jso)
        except ValidationError as e:
            multi.append(render_validationerror(e))
        else:
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
    copy_value("answerLimit", qjso, jsom, 1)
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


@qst_plugin.post("/qst/mcq/multihtml")
@csrf.exempt
def qst__mcq_multihtml():
    jsondata = request.get_json()
    multi = []
    for jso in jsondata:
        convert_mcq_to_qst(jso)
        multi.append(qst_get_html(jso, is_review(jso)))
    return json_response(multi)


@qst_plugin.post("/qst/mmcq/multihtml")
@csrf.exempt
def qst__mmcq_multihtml():
    jsondata = request.get_json()
    multi = []
    for jso in jsondata:
        convert_mcq_to_qst(jso, True)
        multi.append(qst_get_html(jso, is_review(jso)))
    return json_response(multi)


@qst_plugin.post("/qst/multimd")
@csrf.exempt
def qst_multimd():
    jsondata = request.get_json()
    multi = []
    for jso in jsondata:
        multi.append(qst_get_md(jso))
    return json_response(multi)


@qst_plugin.post("/qst/mcq/multimd")
@csrf.exempt
def qst_mcq_multimd():
    jsondata = request.get_json()
    multi = []
    for jso in jsondata:
        # Do not convert to qst and call qst_get_md - prints are different
        multi.append(mcq_get_md(jso))
    return json_response(multi)


@qst_plugin.post("/qst/mmcq/multimd")
@csrf.exempt
def qst_mmcq_multimd():
    jsondata = request.get_json()
    multi = []
    for jso in jsondata:
        # Do not convert to qst and call qst_get_md - prints are different
        multi.append(mmcq_get_md(jso))
    return json_response(multi)


@qst_plugin.post("/qst/getQuestionMD/")
def get_question_md():
    (md,) = verify_json_params("text")
    markup = json.loads(md)
    plugin_data = {"markup": markup}
    convert_qst_md(plugin_data)
    return json_response({"md": plugin_data["markup"]})


@qst_plugin.post("/qst/html/")
@csrf.exempt
def qst_html():
    jsondata = request.get_json()

    html = qst_get_html(jsondata, is_review(jsondata))
    return Response(html, mimetype="text/html")


@qst_plugin.get("/qst/<path:path>")
def not_found(path):
    """This 404 route is required because qst plugin is in the same process - otherwise calling a non-existent qst route
    results in a weird request loop, at least when running locally.
    """
    raise NotExist()


def set_explanation(markup):
    # if expl, change to 1-based, if xpl rename to expl
    if "xpl" in markup:
        markup["expl"] = markup.pop("xpl")
        return
    if "expl" not in markup:
        return
    expl = markup["expl"]
    xpl = {}
    markup["expl"] = xpl
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
    ::
        \\mcq{Onko kuu}{Valitse tosi lause}
        {|l l|}
        {
        \\hline $\bigcirc$ & Kuu on lähempänä kuin aurinko \\
        \\hline $\bigcirc$ & Kuu on yhtä iso kuin aurinko \\
        \\hline $\bigcirc$ & Kuu on juustoa \\
        \\hline
        }

    :param jso: json block to make the markdown or TeX
    """
    markup = jso["markup"]
    printlikeqst = get_value(markup, "texprintlikeqst", 0)
    user_print = jso.get("userPrint", False)

    header = markup.get("header", markup.get("headerText", "Check your understanding"))
    footer = markup.get("footer", "")
    stem = markup.get("stem", "")
    texcolumns = markup.get("texcolumns", "|l l|" if not printlikeqst else "l L")
    user_answer = jso.get("state", None)
    if user_answer is None:
        user_print = False
    if user_print:
        texcolumns = markup.get(
            "texusercolumns", "|l l l|" if not printlikeqst else "l L L"
        )
    texhline = markup.get("texhline", "\\hline" if not printlikeqst else "")
    if texhline:
        texhline += " "
    choices = markup.get("choices", [])
    cstr = ""
    idx = 0
    for choice in choices:
        user_mark = ""
        correct = choice.get("correct", False)
        reason = ""
        checked = " "
        if user_print:
            reason = " & " + choice.get("reason", "")
            if idx == user_answer:
                user_mark = " \\, \\cmark" if correct else " \\, \\xmark"
                checked = "*"
        line = (
            texhline
            + "\\radiobutton"
            + checked
            + user_mark
            + "& "
            + choice.get("text", "")
            + reason
            + " \\\\\n"
        )
        cstr += line
        idx += 1

    if cstr and texhline:
        cstr += texhline + "\n"

    result = format_qst_or_mcq(printlikeqst, header, stem, texcolumns, cstr, footer)

    return result


def format_qst_or_mcq(printlikeqst, header, stem, texcolumns, cstr, footer):
    if printlikeqst:
        result = format_qst_tex(header, stem, "", texcolumns, cstr, footer)
    else:
        result = format_mcq_tex(header, stem, texcolumns, cstr)
    return result


def format_mcq_tex(header, stem, texcolumns, cstr):
    # IMPORTANT: only add linebreaks when there are open curly braces. Required for Pandoc to parse TeX correctly.
    return f"""
\\mcq{{{header}}}{{{stem}}}{{{texcolumns}}}{{
{cstr}
}}
"""


def format_qst_tex(header, stem, question_text, texcolumns, cstr, footer):
    # IMPORTANT: only add linebreaks when there are open curly braces. Required for Pandoc to parse TeX correctly.
    return f"""
\\qsty{{{header}}}{{{stem}}}{{{question_text}}}{{{texcolumns}}}{{
{cstr}
}}{{
{footer}
}}
"""


def mmcq_get_md(jso):
    """
    Gives question in format:
    ::

        \\mcq{Onko kuu}{Valitse tosi lause}
        {|l l|}
        {
        \\hline $\bigcirc$ & Kuu on lähempänä kuin aurinko \\
        \\hline $\bigcirc$ & Kuu on yhtä iso kuin aurinko \\
        \\hline $\bigcirc$ & Kuu on juustoa \\
        \\hline
        }

    :param jso: json block to make the markdown or TeX
    """
    markup = jso["markup"]
    user_print = jso.get("userPrint", False)
    printlikeqst = get_value(markup, "texprintlikeqst", 0)

    header = markup.get("header", markup.get("headerText", "Check your understanding"))
    footer = markup.get("footer", "")
    stem = markup.get("stem", "")
    true_text = markup.get("trueText", "True")
    false_text = markup.get("falseText", "False")
    correct_mark = markup.get("texcorrectText", "\\cmark")
    wrong_mark = markup.get("texwrongText", "\\xmark")

    texcolumns = markup.get("texcolumns", "|l c c|" if not printlikeqst else "L c c")
    user_answer = jso.get("state", [])
    if not user_answer:
        user_print = False
    if user_print:
        texcolumns = markup.get(
            "texusercolumns", "|l c c c l|" if not printlikeqst else "L c c c L"
        )
    texhline = markup.get("texhline", "\\hline" if not printlikeqst else "")
    if texhline:
        texhline += " "
    choices = markup.get("choices", [])
    reason = " & " + " " + " & " + " " if user_print else ""
    cstr = texhline + " & " + true_text + " & " + false_text + reason + " \\\\\n"
    idx = 0
    for choice in choices:
        correct = choice.get("correct", False)
        reason = ""
        leftbox = "\\checkbox"
        rightbox = "\\checkbox"
        if user_print and user_answer:
            ua = None
            if len(user_answer) > idx:
                ua = user_answer[idx]
            correct_or_wrong_mark = correct_mark if ua == correct else wrong_mark
            if ua:
                leftbox += "*"
            # noinspection PySimplifyBooleanCheck
            if ua == False:  # do not replace with not ua because it can be None
                rightbox += "*"
            reason = " & & "
            if ua is not None:
                reason = (
                    " & " + correct_or_wrong_mark + " & " + choice.get("reason", "")
                )
        line = (
            texhline
            + choice.get("text", "")
            + "& "
            + leftbox
            + " & "
            + rightbox
            + reason
            + " \\\\\n"
        )
        cstr += line
        idx += 1

    if cstr and texhline:
        cstr += texhline + "\n"

    result = format_qst_or_mcq(printlikeqst, header, stem, texcolumns, cstr, footer)
    return result


qst_own_attributes = {
    "defaultPoints",
    "doNotMove",
    "isTask",
    "randomizedRows",
    "savedText",
    "size",
    "tag",
}

generic_attribute_names = get_dataclass_field_names(GenericMarkupModel)

common = generic_attribute_names & qst_own_attributes
if common:
    raise Exception(
        f"qst_own_attributes does not need to list generic attributes: {common}"
    )

qst_attrs = qst_own_attributes.union(generic_attribute_names)


def qst_get_html(jso, review):
    result = False
    qst_handle_randomization(jso)
    info = jso["info"]
    markup = jso["markup"]
    markup = normalize_question_json(markup)
    jso["markup"] = markup
    result = qst_try_hide_points(jso)
    jso["show_result"] = result

    if review:
        usercode = qst_str(jso.get("state", "-"))
        s = ""
        result = f'{NOLAZY}<div class="review" ng-non-bindable><pre>{usercode}</pre>{s}</div>'
        return result
    runner = "tim-qst"
    s = f'<{runner} json="{make_base64(jso, TimJsonEncoder)}"></{runner}>'
    return s


def qst_try_hide_points(jso):
    """
    Checks whether to remove points and explanations from markup or keep them

    :param jso: request json with info and markup
    :return: true if user has reached answer limit
    """
    info = jso["info"]
    markup = jso["markup"]
    limit_reached = get_num_value(info, "max_answers", 1) <= get_num_value(
        info, "earlier_answers", 0
    )
    show_points = info.get("show_points", True) if info is not None else True
    if not limit_reached or not show_points:
        markup.pop("points", None)
        markup.pop("expl", None)
        markup.pop("defaultPoints", None)
    return limit_reached


def qst_get_md(jso):
    result = False
    qst_handle_randomization(jso)
    info = jso["info"]
    markup = jso["markup"]

    jso["show_result"] = result

    # attrs = json.dumps(jso)
    user_print = jso.get("userPrint", False)

    print_reason = qst_try_hide_points(jso)

    points_table = create_points_table(markup.get("points"))
    set_explanation(markup)

    header = markup.get("header", "")
    footer = markup.get("footer", "")
    texboxw = markup.get("texboxw", "0.2")
    texboxh = markup.get("texboxh", "10")
    stem = markup.get("stem", "")
    if stem:
        stem += r"\par"
    qjson = markup  # .get('json',{})
    question_text = qjson.get("questionText", "")
    question_type = qjson.get("questionType", "")
    vertical = question_type.find("vertical") >= 0
    headers = qjson.get("headers", [])

    texcolumns = markup.get("texcolumns", "l L" if vertical else "")
    user_answer = jso.get("state", [])
    if not user_answer:
        user_print = False
    if user_print:
        texcolumns = markup.get("texusercolumns", "l L L" if vertical else "")
    texhline = markup.get("texhline", "")
    if texhline:
        texhline += " "
    rows = qjson.get("rows", [])
    cstr = ""
    expl = markup.get("expl", {})
    empty_theader = True
    theader = " &"
    sep = ""
    tc = " "
    for h in headers:
        # Some legacy qst plugin instances have dicts in `headers`,
        # so we need to handle that case.
        # TODO: Use some script to update all legacy qst instances.
        if isinstance(h, dict):
            h = h.get("text", "")
        if h:
            empty_theader = False
        theader += sep + h
        sep = " & "
        tc += "c "
    if user_print:
        if not texcolumns:
            texcolumns = "L" + tc + "L"
    else:
        if not texcolumns:
            texcolumns = "L" + tc

    aft = qjson.get("answerFieldType", "")
    boxdefault = ""
    leftbox = "\\radiobutton"
    if aft == "checkbox":
        leftbox = "\\checkbox"
    if aft == "text":
        leftbox = "\\ttextbox{" + str(texboxw) + "}"
        boxdefault = "{\\vspace{" + str(texboxh) + "mm}}"

    idx = 0
    for row in rows:
        if type(row) is not str:
            continue
        exp = expl.get(str(idx + 1), "")
        lbox = ""
        sep = ""
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
                if aft == "text":
                    if idx < len(user_answer):
                        uar = user_answer[idx]
                        if i < len(uar):
                            ua = uar[i]
                            if ua:
                                boxdef = "{" + ua + "}"
                    box += boxdef
                else:
                    if ua:
                        box += "*"
                    if cell_points > 0:
                        box = r"\correct{" + box + "}"
                    if cell_points:
                        box = r"\lbpoints{" + str(cell_points) + "}{" + box + "}"
            else:
                box += boxdef
            lbox += sep + box
            sep = " & "
        reason = " "
        if user_print and print_reason:
            reason = " & " + exp
        if vertical:
            line = texhline + lbox + " & " + row + reason + " \\\\\n"
        else:
            line = texhline + row + " & " + lbox + reason + " \\\\\n"
        cstr += line
        idx += 1

    if cstr and texhline:
        cstr += texhline + "\n"

    if not empty_theader:
        cstr = theader + " \\\\\n" + texhline + "\n" + cstr

    result = format_qst_tex(header, stem, question_text, texcolumns, cstr, footer)
    return result


def question_convert_js_to_yaml(markup: dict, is_task: bool, task_id: str | None):
    # save question. How to pick up question see lecture.py, get_question_data_from_document
    markup = normalize_question_json(markup)
    question_title = markup["questionTitle"]
    question_title = question_title.replace('"', "").replace("'", "")
    # taskid = questionTitle.replace(" ", "")  # TODO: make better conversion to ID
    taskid = question_title.replace("md:", "")
    taskid = re.sub("[^A-Za-z0-9]", "", taskid)
    if task_id:
        taskid = task_id
    markup.pop("question", None)  # old attribute
    markup.pop("taskId", None)
    markup.pop("qst", None)

    mdyaml = yaml.dump(
        markup, encoding="utf-8", allow_unicode=True, default_flow_style=False
    ).decode(
        "utf-8"
    )  # see also .safe_dump
    mdyaml = mdyaml.replace("\n\n", "\n")

    # Don't add 'dquestion' plugin attribute as it is not needed or processed. Instead, use is_task value
    # to determine whether question is a document question (ie. task), or a lecture question.
    question_attr = f' question="{str(is_task).lower()}"'

    result = "``` {#" + taskid + question_attr + ' plugin="qst"}\n' + mdyaml + "```\n"
    return result


def get_question_data_from_document(
    d: DocInfo, par_id: str, edit=False
) -> QuestionInDocument:
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
        par_id = par_id[i + 1 :]
    d.document.insert_preamble_pars()
    par = d.document.get_paragraph(par_id)
    try:
        plugin_values = Plugin.from_paragraph(
            par,
            default_view_ctx,
            user=UserContext.from_one_user(get_current_user_object()),
        ).values
    except PluginException:
        raise RouteException(f"Paragraph is not a plugin: {par_id}")
    plugindata = {"markup": plugin_values}
    markup = plugindata.get("markup")
    if markup.get("choices", None):
        convert_mcq_to_qst(plugindata, par.get_attr("plugin", "").find("mmcq") == 0)
    if not edit or edit == "false":
        settings = d.document.get_settings()
        convert_qst_md(
            plugindata, par.get_dumbo_options(base_opts=settings.get_dumbo_options())
        )
    markup = plugindata.get("markup")
    return QuestionInDocument(
        markup=normalize_question_json(markup),
        qst=par.is_question(),
        taskId=par.get_attr("taskId"),
        docId=d.id,
        parId=par_id,
        isPreamble=bool(par.from_preamble()),
    )


def convert_qst_md(plugindata: dict, dumbo_opts: DumboOptions | None = None) -> None:
    if not dumbo_opts:
        dumbo_opts = DumboOptions.default()
    prepare_for_dumbo_attr_list_recursive(get_plugin_regex_obj("qst"), plugindata)
    convert_md([plugindata], options=dumbo_opts)


def render_static_qst(m: QstHtmlModel):
    return render_template_string(
        f"""
<div class="qst">qst static placeholder</div>
<br>
        """,
        **asdict(m.markup),
    )
