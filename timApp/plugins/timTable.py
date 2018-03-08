import json
import re
from typing import Dict, Any, NamedTuple, Optional
from xml.sax.saxutils import quoteattr

import yaml
from flask import Blueprint
from flask import Response
from flask import abort
from flask import request

from timApp.containerLink import convert_md
from timApp.plugin import Plugin, PluginException
from timApp.plugin import get_num_value
from timApp.plugin import get_value
from timApp.requesthelper import verify_json_params
from timApp.responsehelper import json_response
from timApp.sessioninfo import get_current_user_object
from timApp.timdb.docinfo import DocInfo
from timApp.timdb.models.askedjson import normalize_question_json

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

@timTable_plugin.route("reqs/")
@timTable_plugin.route("reqs")
def timTable_reqs():
    reqs = {
        "type": "Hello World",
        "js": [
            # "tim/controllers/qstController",
               ],
        "angularModule": [],
        "multihtml": True,
        "multimd": True
    }

    return json_response(reqs)


@timTable_plugin.route("multihtml/", methods=["POST"])
def timTable_multihtml():
    jsondata = request.get_json()
    multi = []
    for jso in jsondata:
        table = jso['markup'].get(TABLE)
        multi.append('')
        stylesAttributes = stylesAndAttributes(table)
        multi[0] = '<table' + concatDefinitions(stylesAttributes['attributes']) + concatStyleDefinitions(
            stylesAttributes['styles']
        ) + '>'
        if COLUMNS in table:
            multi[0] = multi[0]  + parse_columns(table[COLUMNS])
        for row in table[ROWS]:
                multi[0] =  multi[0] + parse_row(row)
    return json_response(multi)

def parse_columns(columns: list) -> str:
    colHtml = ''
    for col in columns:
        stylesAttributes = stylesAndAttributes(col)
        colHtml += '<col' + concatDefinitions(stylesAttributes['attributes'])
        colHtml +=  concatStyleDefinitions(stylesAttributes['styles']) + '>'
    return colHtml

def parse_row(row: dict) -> str:
    """Here
        """
    tdStr = ''
    if ROW in row:
        for item in row[ROW]:
            if isinstance(item, str) or isinstance(item, int) or isinstance(item, float):
                tdStr += "<td>" + str(item) + "</td>"  # In this case the cell is represented just by the content, TODO: md-parser
            else:
                tdStr += parse_cell(item)
        stylesAttributes = stylesAndAttributes(row)
        start = "<tr" + concatDefinitions(stylesAttributes['attributes']) + concatStyleDefinitions(
            stylesAttributes['styles'])
    return start.strip() + ">" + tdStr + "</tr>"


def parse_cell(cell: dict) -> str:
    """
        """
    #if isinstance(cell, str):
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
        cellContent = str(cellContent) #Because formulas are not implemented, casting to string can be done.

    if TYPE in cell:
        if (cell[TYPE] == TEXT): return cellContent  #Todo: md-parseri
        if (cell[TYPE] == NUMBER): return cellContent #Not implemented yet.
        if (cell[TYPE] == FORMULA): return ""  #Not implemented yet.
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
    retDict = {'styles': styleDefinitions, 'attributes':attributeDefinitions}
    for key, value in elements.items():
        if key == "colspan":
            attributeDefinitions.append(key+"=" + "\"" + str(value) + "\"")
        if key  == "rowspan":
            attributeDefinitions.append(key+"=" + "\"" + str(value) + "\"")
        if key == "id":
            attributeDefinitions.append(key + "=" + "\"" + str(value) + "\"")
        if key  == "text-align":
            styleDefinitions.append(key + ":" + value + ";")
        if key  == "vertical-align":
            styleDefinitions.append(key + ":" + value + ";")
        if key  == "background-color":
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