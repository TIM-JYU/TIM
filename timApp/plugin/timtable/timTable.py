import copy
import json
from typing import Optional
from xml.sax.saxutils import quoteattr
from distutils import util;
from flask import Blueprint
from flask import abort
from flask import request
from timApp.auth.accesshelper import verify_edit_access
from timApp.plugin.plugin import Plugin
from timApp.util.flask.requesthelper import verify_json_params, get_option
from timApp.util.flask.responsehelper import json_response
from timApp.document.docentry import DocEntry
from timApp.markdown.dumboclient import call_dumbo
from timApp.plugin.timtable.timTableLatex import convert_table

timTable_plugin = Blueprint('timTable_plugin',
                            __name__,
                            url_prefix='/timTable/')

# Reserved words in the TimTable format and other needed constants
TABLE = 'table'
AUTOMD = 'automd'
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
        "multimd": True,
        "auto_convert_md": False
    }
    return json_response(reqs)


@timTable_plugin.route("multihtml/", methods=["POST"])
def tim_table_multihtml():
    """
    Route for getting the HTML of all TimTable plugins in a document.
    :return:
    """
    jsondata = request.get_json()
    multi = []
    for jso in jsondata:
        multi.append(tim_table_get_html(jso, is_review(request)))
    return json_response(multi)


def tim_table_get_html(jso, review):
    """
    Returns the HTML of a single TimTable paragraph.
    :param jso:
    :param review:
    :return:
    """
    values = jso[MARKUP]
    html_values = prepare_for_and_call_dumbo(values)
    attrs = json.dumps(html_values)
    runner = 'tim-table'
    s = f'<{runner} data={quoteattr(attrs)}></{runner}>'
    return s


@timTable_plugin.route("getCellData", methods=["GET"])
def tim_table_get_cell_data():
    """
    Route for getting the content of a cell.
    :return: The cell content in the specified index.
    """
    multi = []
    args = request.args
    doc_id = get_option(request, 'docId', None, cast=int)
    if not doc_id:
        abort(400)
    doc = DocEntry.find_by_id(doc_id)
    if not doc:
        abort(404)
    verify_edit_access(doc)
    par = doc.document.get_paragraph(args['parId'])
    plug = Plugin.from_paragraph(par)
    yaml = plug.values
    cell_cnt = None
    if is_datablock(yaml):
        cell_cnt = find_cell_from_datablock(yaml[TABLE][DATABLOCK][CELLS], int(args[ROW]), int(args[COL]))
    if cell_cnt is not None:
        multi.append(cell_cnt)
    else:
        rows = yaml[TABLE][ROWS]
        cell_content = find_cell(rows,int(args['row']),int(args['col']))
        multi.append(cell_content)
    return json_response(multi)


@timTable_plugin.route("saveCell", methods=["POST"])
def tim_table_save_cell_list():
    """
    Saves cell content
    :return: The cell content as html
    """
    multi = []
    cell_content, docid, parid, row, col = verify_json_params('cellContent', 'docId', 'parId', 'row', 'col')
    doc = DocEntry.find_by_id(docid)
    if not doc:
        abort(404)
    verify_edit_access(doc)
    par = doc.document_as_current_user.get_paragraph(parid)
    plug = Plugin.from_paragraph(par)
    yaml = plug.values
    if is_datablock(yaml):
        save_cell(yaml[TABLE][DATABLOCK], row, col, cell_content)
    else:
        create_datablock(yaml[TABLE])
        save_cell(yaml[TABLE][DATABLOCK], row, col, cell_content)

    cc = str(cell_content)
    if is_auto_md_enabled(plug.values) and not cc.startswith('md:'):
        cc = 'md: ' + cc
    html = call_dumbo([cc], '/mdkeys')
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
    # clone the previous row's data into the new row but remove the cell content
    copy_row = copy.deepcopy(rows[-1])
    rows.append(copy_row)
    # rows.append({'row': copy.deepcopy(rows[-1]['row'])})
    row = rows[-1]['row']
    for i in range(len(row)):
        if isinstance(row[i], str) or isinstance(row[i], int) or isinstance(row[i], bool) \
                or isinstance(row[i], float):
            row[i] = {CELL: ''}
        else:
            row[i][CELL] = ''
    plug.save()
    return json_response(prepare_for_and_call_dumbo(plug.values))


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
        if isinstance(last_cell, str) or isinstance(last_cell, int) or isinstance(last_cell, bool) \
                or isinstance(last_cell, float):
            current_row.append({CELL: ""})
        else:
            # Copy the last cell's other properties for the new cell, but leave the text empty
            new_cell = copy.deepcopy(last_cell)
            new_cell[CELL] = ''
            current_row.append(new_cell)
        
    plug.save()
    return json_response(prepare_for_and_call_dumbo(plug.values))


@timTable_plugin.route("multimd/", methods=["POST"])
def tim_table_multimd():
    """
    Handles latex printing.
    :return: Table as latex.
    """
    jsondata = request.get_json()
    multi = []
    for jso in jsondata:
        tbl = jso[MARKUP][TABLE]
        latexTable = str(convert_table(tbl))
        multi.append(latexTable)
    return json_response(multi)


def is_datablock(yaml: dict) -> bool:
    """
    Checks if tableDataBlock exists
    :param yaml:
    :return: Boolean indicating the existance of tabledatablock
    """
    try:
        if yaml[TABLE][DATABLOCK]:
            return True
        else:
            return False
    except KeyError:
        return False


def create_datablock(table: dict):
    """
    Creates tableDatablock
    :param table:
    :return:
    """
    table['tabledatablock'] = {}
    table['tabledatablock']['type'] = 'relative'
    table['tabledatablock']['cells'] = {}


def save_cell(datablock: dict, row: int, col: int, cell_content: str):
    """
    Updates datablock with the content and the coordinate of a cell.
    :param datablock:
    :param row: Row index
    :param col: Column index
    :param cell_content: Cell content
    :return:
    """
    coordinate = colnum_to_letters(col) + str(row+1)
    try:
        datablock['cells'].update({coordinate: cell_content})
    except:
        pass


def find_cell(rows: list, row: int, col: int) -> str:
    """
    Get cell from index place if exists
    :param rows: List of cells
    :param row: Row index
    :param col: Column index
    :return: Cell from specified index
    """
    right_row = rows[row][ROW]
    right_cell = right_row[col]
    if isinstance(right_cell, str) or isinstance(right_cell, int) or isinstance(right_cell, float):
       return right_cell
    return right_cell[CELL]


def find_cell_from_datablock(cells: dict, row: int, col: int) -> Optional[str]:
    """
    Finds cell from datablock
    :param cells: all cells
    :param row: Row index
    :param col: Column index
    :return: cell if exists
    """
    ret = None
    coordinate = colnum_to_letters(col) + str(row+1)
    try:
        value = cells[coordinate]
        ret = value
    except:
        pass
    return ret


def colnum_to_letters(column_index: int) -> str:
    """
    Transforms column index to letter
    :param column_index: ex. 2
    :return: column index as letter
    """
    last_char = chr(ASCII_OF_A + (column_index % ASCII_CHAR_COUNT))
    remainder = column_index // ASCII_CHAR_COUNT

    if remainder == 0:
        return last_char
    elif remainder <= ASCII_CHAR_COUNT:
        return chr(ASCII_OF_A + remainder - 1) + last_char

    # recursive call to figure out the rest of the letters
    return colnum_to_letters(remainder - 1) + last_char


def is_review(request):
    """
    Check if request is review
    :param request:
    :return:
    """
    result = request.full_path.find("review=") >= 0
    return result


def prepare_for_and_call_dumbo(values):
    """
    Prepares the table's markdown for Dumbo conversion and
    runs it through Dumbo.
    :param values: The plugin paragraph's markdown.
    :return: The conversion result from Dumbo.
    """
    return call_dumbo(prepare_for_dumbo(values), '/mdkeys')


def is_auto_md_enabled(values):
    """
    Checks whether auto-md logic is enabled for a table.
    :param values: The plugin values as a list or dict.
    :return: True if auto-md is enabled, otherwise false.
    """
    try:
        auto_md = values[TABLE][AUTOMD]
        if not isinstance(auto_md, bool):
            return False
        return auto_md
    except (ValueError, KeyError):
        return False


def prepare_for_dumbo(values):
    """
    Prepares the table's markdown for Dumbo conversion.
    :param values: The plugin paragraph's markdown.
    :return: The table's markdown, prepared for dumbo conversion.
    """
    auto_md = is_auto_md_enabled(values)

    try:
        rows = values[TABLE][ROWS]
    except KeyError:
        return values

    if auto_md:
        # regular row data
        for row in rows:
            rowdata = row[ROW]
            for i in range(len(rowdata)):
                cell = rowdata[i]
                if is_of_unconvertible_type(cell):
                        continue

                if isinstance(cell, str):
                    if cell.startswith('md:'):
                        continue
                    rowdata[i] = 'md: ' + cell
                else:
                    cell[CELL] = 'md: ' + cell[CELL]

        # datablock
        data_block = None
        try:
            data_block = values[TABLE][DATABLOCK][CELLS]
        except KeyError:
            pass

        if data_block is not None:
            for key, value in data_block.items():
                if isinstance(value, str) and value.startswith('md:'):
                    data_block[key] = 'md: ' + value

    return values


def is_of_unconvertible_type(value):
    return isinstance(value, int) or isinstance(value, bool) or isinstance(value, float)
