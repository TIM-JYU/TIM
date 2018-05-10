import copy
import json
import re
from typing import Dict, Any, NamedTuple, Optional
from xml.sax.saxutils import quoteattr

import yaml
from flask import Blueprint
from flask import Response
from flask import abort
from flask import request

from timApp.accesshelper import verify_admin, verify_edit_access, verify_manage_access, verify_view_access, \
    has_manage_access, ItemLockedException, get_doc_or_abort
from timApp.containerLink import convert_md
from timApp.plugin import Plugin, PluginException
from timApp.plugin import get_num_value
from timApp.plugin import get_value
from timApp.requesthelper import verify_json_params
from timApp.responsehelper import json_response, ok_response, text_response
from timApp.sessioninfo import get_current_user_object
from timApp.timdb.docinfo import DocInfo
from timApp.timdb.models.askedjson import normalize_question_json
from timApp.timdb.models.docentry import DocEntry
from timApp.markdownconverter import md_to_html
from timApp.dumboclient import call_dumbo
from timApp.plugins.timTableLatex import convert_table


timTable_plugin = Blueprint('timTable_plugin',
                            __name__,
                            url_prefix='/timTable/')

# Reserved words in the TimTable format
TABLE = 'table'
ROWS = 'rows'
ROW = 'row'
COLUMNS = 'columns'
COLUMN = 'column'
CELL = 'cell'
TYPE = 'type'
TEXT = 'text'
FORMULA = 'formula'
NUMBER = 'number'
DATABLOCK = 'tabledatablock'
CELLS = 'cells'
COL = 'col'
ASCII_OF_A = 65
ASCII_CHAR_COUNT = 26
MARKUP = 'markup'


@timTable_plugin.route("reqs/")
@timTable_plugin.route("reqs")
def timTable_reqs():
    reqs = {
        "type": "embedded",
        "js": [
            # "js/timTable.js"
            # "tim/controllers/qstController",
        ],
        "angularModule": [],
        "multihtml": True,
        "multimd": True
    }

    return json_response(reqs)


@timTable_plugin.route("multihtml/", methods=["POST"])
def qst_multihtml():
    jsondata = request.get_json()
    multi = []
    for jso in jsondata:
        multi.append(qst_get_html(jso, is_review(request)))
    return json_response(multi)


@timTable_plugin.route("getCellData", methods=["GET"])
def tim_table_get_cell_data():
    multi = []
    args = request.args
    doc = DocEntry.find_by_id(int(args['docId']))
    if not doc:
        abort(404)
    verify_edit_access(doc)
    par = doc.document.get_paragraph(args['parId'])
    plug = Plugin.from_paragraph(par)
    yaml = plug.values
    cell_cnt = None
    if is_datablock(yaml):
        cell_cnt = find_cell_from_datablock(yaml[TABLE][DATABLOCK][CELLS], int(args[ROW]), int(args[COL]))
    if cell_cnt != None:
        multi.append(cell_cnt)
    else:
        rows = yaml[TABLE][ROWS]
        cell_content = find_cell(rows,int(args['row']),int(args['col']))
        multi.append(cell_content)
    return json_response(multi)


@timTable_plugin.route("saveCell", methods=["POST"])
def tim_table_save_cell_list():
    multi = []
   # args = request.args  #params
    # par = doc.get_paragraph(args['parId'])
    # doc = DocEntry.find_by_id(int(args['docId'])).document
    cellContent = verify_json_params('cellContent')
    docId = verify_json_params('docId')
    parId = verify_json_params('parId')
    row = verify_json_params('row')
    col = verify_json_params('col')  # todo: put directly in below use cases
    #doc = DocEntry.find_by_id(int(docId)).document
    doc = DocEntry.find_by_id(docId[0])
    if not doc:
        abort(404)
    verify_edit_access(doc)
    par = doc.document.get_paragraph(parId[0])
    plug = Plugin.from_paragraph(par)
    yaml = plug.values
    if is_datablock(yaml):
        #cell_coordinate = save_cell(yaml[TABLE][DATABLOCK], int(args[ROW]),int(args[COL]), args['cellContent'])
        save_cell(yaml[TABLE][DATABLOCK], row[0], col[0], cellContent[0])
    else:
        create_datablock(yaml[TABLE])
        save_cell(yaml[TABLE][DATABLOCK], row[0], col[0], cellContent[0])

    cc = str(cellContent[0])
    '''
    if len(cc) > 3:
        ccTmp = cc[:3]
        if ccTmp == 'md:':
            cellHtml = md_to_html(cc[3:])
            print(cellHtml)
    '''
    html = call_dumbo([cc], "/mdkeys")
    plug.save()
    multi.append(html[0])
    return json_response(multi)


@timTable_plugin.route("addRow", methods=["POST"])
def tim_table_add_row():
    """
    Adds a row into the table.
    :return:
    """
    doc_id, par_id = verify_json_params('docId', 'parId')
    d = DocEntry.find_by_id(doc_id)
    if not d:
        abort(404)
    verify_edit_access(d)
    par = d.document_as_current_user.get_paragraph(par_id)
    plug = Plugin.from_paragraph(par)
    try:
        rows = plug.values[TABLE][ROWS]
    except KeyError:
        return abort(400)
    last_row_index = len(rows) - 1
    # clone the previous row's data into the new row
    rows.append({'row': copy.deepcopy(rows[-1]['row'])})
    plug.save()
    return json_response(call_dumbo(plug.values, '/mdkeys'))


@timTable_plugin.route("addColumn", methods=["POST"])
def tim_table_add_column():
    """
    Adds a new cell into each row on the table.
    In other words, adds a column into the table.
    :return:
    """
    doc_id, par_id = verify_json_params('docId', 'parId')
    d = DocEntry.find_by_id(doc_id)
    if not d:
        abort(404)
    verify_edit_access(d)
    par = d.document_as_current_user.get_paragraph(par_id)
    plug = Plugin.from_paragraph(par)
    try:
        rows = plug.values[TABLE][ROWS]
    except KeyError:
        return abort(400)

    for row in rows:
        try:
            current_row = row[ROW]
        except KeyError:
            return abort(400)
        last_cell = current_row[-1]
        if isinstance(last_cell, str):
            current_row.append({CELL: ""})
        else:
            # Copy the last cell's other properties for the new cell, but leave the text empty
            new_cell = copy.deepcopy(last_cell)
            new_cell[CELL] = ''
            current_row.append(new_cell)
        
    plug.save()
    return json_response(call_dumbo(plug.values, '/mdkeys'))

@timTable_plugin.route("multimd/", methods=["POST"])
def tim_table_multimd():
    jsondata = request.get_json()
    multi = []
    for jso in jsondata:
        tbl = jso[MARKUP][TABLE]
        latexTable = str(convert_table(tbl))
        multi.append(latexTable)
        # print(latexTable)
    return json_response(multi)


def is_datablock(yaml: dict) -> bool:
    try:
        if yaml[TABLE][DATABLOCK]:
            return True
        else:
            return False
    except:
        return False


def create_datablock(table: dict):
    table['tabledatablock'] = {}
    table['tabledatablock']['type'] = 'relative'
    table['tabledatablock']['cells'] = {}


def save_cell(datablock: dict, row: int, col: int, cell_content: str) -> str:
    coordinate = colnum_to_letters(col) + str(row+1)
    try:
        datablock['cells'].update({coordinate: cell_content})
    except:
        pass


def find_cell(rows: list, row: int, col: int) -> str:
   right_row = rows[row][ROW]
   right_cell = right_row[col]
   if isinstance(right_cell, str) or isinstance(right_cell, int) or isinstance(right_cell, float):
       return right_cell
   return right_cell[CELL]


def find_cell_from_datablock(cells: dict, row: int, col: int) -> str:
    ret = None
    coordinate = colnum_to_letters(col) + str(row+1)
    try:
        value = cells[coordinate]
        ret = value
    except:
        pass
    return ret


def colnum_to_letters(column_index: int) -> str:
    last_char = chr(ASCII_OF_A + (column_index % ASCII_CHAR_COUNT))
    remainder = column_index // ASCII_CHAR_COUNT

    if remainder == 0:
        return last_char
    elif remainder <= ASCII_CHAR_COUNT:
        return chr(ASCII_OF_A + remainder - 1) + last_char

    # recursive call to figure out the rest of the letters
    return colnum_to_letters(remainder - 1) + last_char


def is_review(request):
    # print(query)
    result = request.full_path.find("review=") >= 0
    return result


NOLAZY = "<!--nolazy-->"


def qst_get_html(jso, review):
    result = False
    info = jso['info']
    markup = jso['markup']
    """
    #markup = normalize_question_json(markup,
                                     #allow_top_level_keys=
                                     #{
                                     #    'button',
                                     #    'buttonText',
                                     #    'footer',
                                     #    'header',
                                     #    'isTask',
                                     #    'lazy',
                                     #    'resetText',
                                     #    'stem',
                                    # })
    #jso['markup'] = markup
    if info and info['max_answers'] and info['max_answers'] <= info.get('earlier_answers', 0):
        result = True
    if not result:
        delete_key(markup, 'points')
        delete_key(markup, 'expl')
    jso['show_result'] = result

    if review:
        usercode = qst_str(jso.get("state", "-"))
        s = ""
        result = NOLAZY + '<div class="review" ng-non-bindable><pre>' + usercode + '</pre>' + s + '</div>'
        return result
    """
    attrs = json.dumps(jso['markup'])

    # runner = 'qst-runner'
    # s = f'<{runner} json={quoteattr(attrs)}></{runner}>'

    runner = 'tim-table'
    s = f'<{runner} data={quoteattr(attrs)}></{runner}>'

    return s


def delete_key(d: Dict[str, Any], key: str):
    if key in d:
        del d[key]


def qst_str(state):
    s = str(state)
    s = s.replace("], ", "\n")
    s = s.replace("]", "")
    s = s.replace("[", "")
    s = s.replace("''", "-")
    s = s.replace("'", "")
    return s


"""
@timTable_plugin.route("multihtml/", methods=["POST"])
def timTable_multihtml():
    jsondata = request.get_json()
    multi = []
    for jso in jsondata:
        table = jso['markup'].get(TABLE)
        multi.append('')
        print(table)
        stylesAttributes = stylesAndAttributes(table)
        multi[0] = '<table' + concatDefinitions(stylesAttributes['attributes']) + concatStyleDefinitions(
            stylesAttributes['styles']
        ) + '>'
        if COLUMNS in table:
            multi[0] = multi[0] + parse_columns(table[COLUMNS])
        for row in table[ROWS]:
            multi[0] = multi[0] + parse_row(row)
    return json_response(multi)"""


def parse_columns(columns: list) -> str:
    colHtml = ''
    for col in columns:
        stylesAttributes = stylesAndAttributes(col)
        colHtml += '<col' + concatDefinitions(stylesAttributes['attributes'])
        colHtml += concatStyleDefinitions(stylesAttributes['styles']) + '>'
    return colHtml


def parse_row(row: dict) -> str:
    """Here
        """
    tdStr = ''
    if ROW in row:
        for item in row[ROW]:
            if isinstance(item, str) or isinstance(item, int) or isinstance(item, float):
                tdStr += "<td>" + str(
                    item) + "</td>"  # In this case the cell is represented just by the content, TODO: md-parser
            else:
                tdStr += parse_cell(item)
        stylesAttributes = stylesAndAttributes(row)
        start = "<tr" + concatDefinitions(stylesAttributes['attributes']) + concatStyleDefinitions(
            stylesAttributes['styles'])
    return start.strip() + ">" + tdStr + "</tr>"


def parse_cell(cell: dict) -> str:
    """
        """
    # if isinstance(cell, str):
    #    return "<td>"+cell+"</td>"
    stylesAttributes = stylesAndAttributes(cell)
    attributes = stylesAttributes['attributes']
    start = "<td" + concatDefinitions(attributes)
    styles = stylesAttributes['styles']
    start += concatStyleDefinitions(styles)
    ans = start.strip() + ">" + handleType(cell) + "</td>"
    return ans


def handleType(cell: dict):
    cellContent = cell[CELL]
    if isinstance(cellContent, int) or isinstance(cellContent, float):
        cellContent = str(cellContent)  # Because formulas are not implemented, casting to string can be done.

    if TYPE in cell:
        if (cell[TYPE] == TEXT): return cellContent  # Todo: md-parseri
        if (cell[TYPE] == NUMBER): return cellContent  # Not implemented yet.
        if (cell[TYPE] == FORMULA): return ""  # Not implemented yet.
    return cellContent


def concatDefinitions(definitions: list) -> str:
    """{'colspan="2"', 'alignment="2"'}
       -> 'colspan="2" alignment="2"'
     """
    defStr = ''
    for definition in definitions:
        defStr += definition + ' '
    return " " + defStr.strip()


def concatStyleDefinitions(definitions: list) -> str:
    defStr = 'style=\"'
    for definition in definitions:
        defStr += definition
    return " " + defStr.strip() + "\""


def stylesAndAttributes(elements: dict) -> dict:
    styleDefinitions = []
    attributeDefinitions = []
    retDict = {'styles': styleDefinitions, 'attributes': attributeDefinitions}
    for key, value in elements.items():
        if key == "colspan":
            attributeDefinitions.append(key + "=" + "\"" + str(value) + "\"")
        if key == "rowspan":
            attributeDefinitions.append(key + "=" + "\"" + str(value) + "\"")
        if key == "id":
            attributeDefinitions.append(key + "=" + "\"" + str(value) + "\"")
        if key == "text-align":
            styleDefinitions.append(key + ":" + value + ";")
        if key == "vertical-align":
            styleDefinitions.append(key + ":" + value + ";")
        if key == "background-color":
            styleDefinitions.append(key + ":" + value + ";")
        if key == "border":
            styleDefinitions.append(key + ":" + value + ";")
        if key == "border-top":
            styleDefinitions.append(key + ":" + value + ";")
        if key == "border-bottom":
            styleDefinitions.append(key + ":" + value + ";")
        if key == "border-left":
            styleDefinitions.append(key + ":" + value + ";")
        if key == "border-right":
            styleDefinitions.append(key + ":" + value + ";")
        if key == "width":
            styleDefinitions.append(key + ":" + str(value) + ";")
        if key == "height":
            styleDefinitions.append(key + ":" + str(value) + ";")
    return retDict
