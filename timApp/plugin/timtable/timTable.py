import copy
import json
from typing import Any, Dict, List, Optional, Tuple, Union
from xml.sax.saxutils import quoteattr
from flask import Blueprint
from flask import abort
from flask import request
from timApp.auth.accesshelper import verify_edit_access
from timApp.auth.sessioninfo import get_current_user_object
from timApp.document.document import Document
from timApp.plugin.plugin import Plugin
from timApp.plugin.timtable.row_owner_info import RowOwnerInfo
from timApp.util.flask.requesthelper import verify_json_params, get_option
from timApp.util.flask.responsehelper import json_response
from timApp.document.docentry import DocEntry
from timApp.markdown.dumboclient import call_dumbo, DumboOptions
from timApp.plugin.timtable.timTableLatex import convert_table
from timApp.timdb.sqa import db

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
RELATIVE = 'relative'
UNIQUE_ROW_COUNT = 'uniqueRowCount'
GLOBAL_APPEND_MODE = 'globalAppendMode'
DATA_INPUT = "dataInput"
BACKGROUND_COLOR = 'backgroundColor'
MD = 'md:'
ID = 'id'
ASCII_OF_A = 65
ASCII_CHAR_COUNT = 26
MARKUP = 'markup'
DUMBO_PARAMS = '/mdkeys'


# class to enable direct calls from TIM container
class TimTable:
    def __init__(self):
        pass


    @staticmethod
    def prepare_for_dumbo(values):
        return prepare_for_dumbo(values[MARKUP])


    @staticmethod
    def multihtml_direct_call(jsondata):
        return tim_table_multihtml_direct(jsondata)


class RelativeDataBlockValue:
    def __init__(self, row: int, column: int, data: Union[str, Dict[str, Any]]):
        self.row = row
        self.column = column
        self.data = data

@timTable_plugin.route("reqs/")
@timTable_plugin.route("reqs")
def tim_table_reqs():
    reqs = {
        "type": "embedded",
        "js": [
            # "js/timTable.js"
            # "tim/controllers/qstController",
        ],
        "angularModule": [],
        "multihtml": True,
        "multimd": True,
        "default_automd": True,
    }
    return json_response(reqs)


def tim_table_multihtml_direct(jsondata):
    """
    Directly callable method for getting the HTML of all TimTable plugins.
    :param jsondata: The data of the plugins.
    :return: The data of the plugins converted to HTML.
    """
    multi = []
    for jso in jsondata:
        multi.append(tim_table_get_html(jso, is_review(request)))
    return json.dumps(multi)


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


def prepare_multi_for_dumbo(timtable_list):
    """
    Prepares multiple TimTables (given in a request) for Dumbo.
    :param timtable_list:
    :return:
    """
    for table in timtable_list:
        prepare_for_dumbo(table[MARKUP])


def tim_table_get_html(jso, review):
    """
    Returns the HTML of a single TimTable paragraph.
    :param jso:
    :param review:
    :return:
    """
    values = jso[MARKUP]
    attrs = json.dumps(values)
    runner = 'tim-table'
    s = f'<{runner} data={quoteattr(attrs)}></{runner}>'
    return s


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
        if isinstance(cell_cnt, dict):
            if CELL in cell_cnt:
                multi.append(cell_cnt[CELL])
            else:
                return abort(400)
        else:
            multi.append(cell_cnt)
    else:
        rows = yaml[TABLE][ROWS]
        cell_content = find_cell(rows,int(args['row']),int(args['col']))
        multi.append(cell_content)
    return json_response(multi)


@timTable_plugin.route("addRow", methods=["POST"])
def tim_table_add_row():
    """
    Adds a row into the table.
    :return: The entire table's data after the row has been added.
    """
    doc_id, par_id, row_id = verify_json_params('docId', 'parId', 'rowId')
    d, plug = get_plugin_from_paragraph(doc_id, par_id)
    verify_edit_access(d)
    add_row(plug, row_id)
    return json_response(prepare_for_and_call_dumbo(plug))


@timTable_plugin.route("addUserSpecificRow", methods=["POST"])
def tim_table_add_user_specific_row():
    """
    Adds an user-specific row into the table.
    :return: The entire table's data after the row has been added.
    """
    doc_id, par_id = verify_json_params('docId', 'parId')
    d, plug = get_plugin_from_paragraph(doc_id, par_id)
    unique_id = add_row(plug, -1)
    # todo database stuff
    user = get_current_user_object()
    owner_info = RowOwnerInfo(doc_id=doc_id, par_id=par_id,
                              unique_row_id=unique_id,
                              usergroup_id=user.get_personal_group().id)
    db.session.add(owner_info)
    # db.session.flush()
    db.session.commit()
    return json_response(prepare_for_and_call_dumbo(plug))


def add_row(plug: Plugin, row_id: int):
    """
    Generic function for adding a row.
    :param plug: The plugin.
    :param row_id: The place (index) where the row should be added. -1 can be used for appending
     rows to the end of the table.
    :return: The unique ID of the row, or None if it has no ID.
    """
    try:
        rows = plug.values[TABLE][ROWS]
    except KeyError:
        return abort(400)
    if row_id == -1:
        row_id = len(rows)
    elif len(rows) < row_id:
        return abort(400)

    # clone the previous row's data into the new row but remove the cell content
    copy_row = copy.deepcopy(rows[row_id - 1])
    rows.insert(row_id, copy_row)
    # rows.append({'row': copy.deepcopy(rows[-1]['row'])})
    row = rows[row_id]['row']
    unique_id = None
    if is_in_global_append_mode(plug):
        unique_id = pop_unique_row_id(plug)
        rows[row_id][ID] = unique_id
    for i in range(len(row)):
        if is_primitive(row[i]):
            row[i] = {CELL: ''}
        else:
            row[i][CELL] = ''
    if row_id < len(rows) - 1:
        datablock_entries = construct_datablock_entry_list_from_yaml(plug)
        for entry in datablock_entries:
            if entry.row >= row_id:
                entry.row = entry.row + 1
        apply_datablock_from_entry_list(plug, datablock_entries)
    plug.save()
    return unique_id


def pop_unique_row_id(plug: Plugin) -> int:
    """
    Returns an unique ID for a new row.
    :param plug: The plugin instance.
    :return:
    """
    try:
        unique_row_count = int(plug.values[UNIQUE_ROW_COUNT])
    except KeyError:
        unique_row_count = 0

    unique_row_str = str(unique_row_count + 1)
    plug.values[UNIQUE_ROW_COUNT] = unique_row_str

    return unique_row_count


@timTable_plugin.route("addDatablockRow", methods=["POST"])
def tim_table_add_datablock_row():
    """
    Adds a row into the table's datablock.
    Doesn't affect the table's regular YAML.
    :return: The entire table's data after the row has been added.
    """
    doc_id, par_id, row_id = verify_json_params('docId', 'parId', 'rowId')
    d, plug = get_plugin_from_paragraph(doc_id, par_id)

    if not is_datablock(plug.values):
        create_datablock(plug.values[TABLE])

    datablock_entries = construct_datablock_entry_list_from_yaml(plug)
    new_datablock_entries = []

    if datablock_entries:
        for entry in datablock_entries:
            if entry.row == row_id - 1:
                new_datablock_entries.append(RelativeDataBlockValue(row_id, entry.column, ''))
            elif entry.row >= row_id:
                entry.row += 1

            new_datablock_entries.append(entry)
    else:
        try:
            rows = plug.values[TABLE][ROWS]
        except KeyError:
            return abort(400)
        if not rows:
            return abort(400)
        row = rows[-1][ROW]
        for i in range(0, len(row)):
            new_datablock_entries.append(RelativeDataBlockValue(len(rows), i, ''))

    apply_datablock_from_entry_list(plug, new_datablock_entries)
    plug.save()

    return json_response(prepare_for_and_call_dumbo(plug))


@timTable_plugin.route("addColumn", methods=["POST"])
def tim_table_add_column():
    """
    Adds a new cell into each row on the table.
    In other words, adds a column into the table.
    :return: The entire table's data after the column has been added.
    """
    doc_id, par_id, col_id = verify_json_params('docId', 'parId', 'colId')
    d, plug = get_plugin_from_paragraph(doc_id, par_id)
    verify_edit_access(d)
    try:
        rows = plug.values[TABLE][ROWS]
    except KeyError:
        return abort(400)

    if col_id < 1:
        # Add a column to the end of each row, regardless of their length
        for row in rows:
            try:
                current_row = row[ROW]
            except KeyError:
                return abort(400)
            last_cell = current_row[-1]
            if is_primitive(last_cell):
                current_row.append({CELL: ""})
            else:
                # Copy the last cell's other properties for the new cell, but leave the text empty
                new_cell = copy.deepcopy(last_cell)
                new_cell[CELL] = ''
                current_row.append(new_cell)
    else:
        for row in rows:
            try:
                current_row = row[ROW]
            except KeyError:
                return abort(400)
            if len(current_row) < col_id:
                continue
            previous_cell = current_row[col_id - 1]
            if is_primitive(previous_cell):
                current_row.insert(col_id, {CELL: ""})
            else:
                new_cell = copy.deepcopy(previous_cell)
                new_cell[CELL] = ''
                current_row.insert(col_id, {CELL: ""})

    plug.save()
    return json_response(prepare_for_and_call_dumbo(plug))


@timTable_plugin.route("addDatablockColumn", methods=["POST"])
def tim_table_add_datablock_column():
    """
    Adds a column into the table's datablock.
    Doesn't affect the table's regular YAML.
    :return: The entire table's data after the column has been added.
    """
    doc_id, par_id, col_id = verify_json_params('docId', 'parId')
    d, plug = get_plugin_from_paragraph(doc_id, par_id)
    verify_edit_access(d)

    if not is_datablock(plug.values):
        create_datablock(plug.values[TABLE])

    column_counts, datablock_entries = get_column_counts(plug)

    for row_index, column_count in column_counts.items():
        datablock_entries.append(RelativeDataBlockValue(row_index, column_count, ''))

    apply_datablock_from_entry_list(plug, datablock_entries)
    plug.save()
    return json_response(prepare_for_and_call_dumbo(plug))


def get_column_counts(plug: Plugin) -> Tuple[Dict[int, int], List[RelativeDataBlockValue]]:
    """
    Returns the number of columns for each row.
    Takes both the regular table structure and the datablock into account.
    :return: A dict with row indexes as keys and respective column counts as values,
    and also a list of datablock entries.
    """
    column_counts = {}
    try:
        rows = plug.values[TABLE][ROWS]
    except KeyError:
        return abort(400)
    for i in range(0, len(rows)):
        try:
            current_row = rows[i][ROW]
        except KeyError:
            return abort(400)
        column_counts[i] = len(current_row)

    datablock_entries = []
    if is_datablock(plug.values):
        datablock_entries = construct_datablock_entry_list_from_yaml(plug)
        for entry in datablock_entries:
            if not entry.row in column_counts or column_counts[entry.row] <= entry.column:
                column_counts[entry.row] = entry.column + 1

    return column_counts, datablock_entries


@timTable_plugin.route("removeDatablockColumn", methods=["POST"])
def tim_table_remove_datablock_column():
    """
    Removes a column from the table's datablock.
    Doesn't affect the regular table structure.
    :return: The entire table's data after the column has been removed.
    """
    doc_id, par_id = verify_json_params('docId', 'parId')
    d, plug = get_plugin_from_paragraph(doc_id, par_id)
    verify_edit_access(d)

    column_counts, datablock_entries = get_column_counts(plug)

    new_datablock_entries = []
    for entry in datablock_entries:
        if entry.column < column_counts[entry.row] - 1:
            new_datablock_entries.append(entry)

    apply_datablock_from_entry_list(plug, new_datablock_entries)
    plug.save()
    return json_response(prepare_for_and_call_dumbo(plug))


@timTable_plugin.route("removeRow", methods=["POST"])
def tim_table_remove_row():
    """
    Removes a row from the table.
    :return: The entire table's data after the row has been removed.
    """
    doc_id, par_id, row_id, datablock_only = verify_json_params('docId', 'parId', 'rowId', 'datablockOnly')
    d, plug = get_plugin_from_paragraph(doc_id, par_id)
    verify_edit_access(d)
    if not datablock_only:
        try:
            rows = plug.values[TABLE][ROWS]
        except KeyError:
            return abort(400)

        if len(rows) <= row_id:
            return abort(400)
        rows.pop(row_id)

    if is_datablock(plug.values):
        datablock_entries = construct_datablock_entry_list_from_yaml(plug)
        new_datablock_entries = []
        for entry in datablock_entries:
            if entry.row == row_id:
                continue

            if entry.row > row_id:
                entry.row -= 1
            new_datablock_entries.append(entry)

        apply_datablock_from_entry_list(plug, new_datablock_entries)

    plug.save()
    return json_response(prepare_for_and_call_dumbo(plug))


@timTable_plugin.route("removeColumn", methods=["POST"])
def tim_table_remove_column():
    """
    Removes a column from the table.
    :return: The entire table's data after the column has been removed.
    """
    doc_id, par_id, col_id = verify_json_params('docId', 'parId', 'colId')
    d, plug = get_plugin_from_paragraph(doc_id, par_id)
    verify_edit_access(d)
    try:
        rows = plug.values[TABLE][ROWS]
    except KeyError:
        return abort(400)

    for row in rows:
        try:
            current_row = row[ROW]
        except KeyError:
            return abort(400)
        if len(current_row) <= col_id:
            continue # continue instead of erroring out, some rows might have colspan in
                     # their cells while we can still remove the column from other rows

        current_row.pop(col_id)

    if is_datablock(plug.values):
        datablock_entries = construct_datablock_entry_list_from_yaml(plug)
        new_datablock_entries = []
        for entry in datablock_entries:
            if entry.column == col_id:
                continue

            if entry.column > col_id:
                entry.column -= 1
            new_datablock_entries.append(entry)
        apply_datablock_from_entry_list(plug, new_datablock_entries)

    plug.save()
    return json_response(prepare_for_and_call_dumbo(plug))

#############################
# Table editor toolbar routes
#############################

@timTable_plugin.route("setCellBackgroundColor", methods=["POST"])
def tim_table_set_cell_background_color():
    """
    Sets a cell's background color.
    :return: The entire table's data after the cell's background color has been set.
    """
    doc_id, par_id, row_id, col_id, color = verify_json_params('docId', 'parId', 'rowId', 'colId', 'color')
    return set_cell_style_attribute(doc_id, par_id, row_id, col_id, BACKGROUND_COLOR, color)


@timTable_plugin.route("setCellColor", methods=["POST"])
def tim_table_set_cell_foreground_color():
    """
    Sets a cell's foreground color.
    :return: The entire table's data after the cell's foreground color has been set.
    """
    doc_id, par_id, row_id, col_id, color = verify_json_params('docId', 'parId', 'rowId', 'colId', 'color')
    return set_cell_style_attribute(doc_id, par_id, row_id, col_id, 'color', color)


@timTable_plugin.route("setCellTextAlign", methods=["POST"])
def tim_table_set_cell_text_align():
    """
    Sets a cell's text align.
    :return: The entire table's data after the cell's text align has been set.
    """
    doc_id, par_id, row_id, col_id, text_align = verify_json_params('docId', 'parId', 'rowId', 'colId', 'textAlign')
    return set_cell_style_attribute(doc_id, par_id, row_id, col_id, 'textAlign', text_align)


def set_cell_style_attribute(doc_id, par_id, row_id, col_id, attribute, value):
    """
    Sets a style attribute for a cell.
    :param doc_id: Document ID
    :param par_id: Paragraph ID
    :param row_id: Row index
    :param col_id: Column index
    :param attribute: The attribute to set.
    :param value: The value of the attribute.
    :return: The entire table's data after the style attribute has been set.
    """
    d, plug = get_plugin_from_paragraph(doc_id, par_id)
    verify_edit_access(d)
    data_input_mode = is_in_datainput_mode(plug)
    if data_input_mode:
        datablock_entries = construct_datablock_entry_list_from_yaml(plug)
        existing_datablock_entry = None
        for entry in datablock_entries:
            if entry.row == row_id and entry.column == col_id:
                existing_datablock_entry = entry
                break

        if not existing_datablock_entry:
            try:
                cell_content = find_cell(plug.values[TABLE][ROWS], row_id, col_id)
            except KeyError:
                cell_content = ''
            new_entry = RelativeDataBlockValue(row_id, col_id, {attribute: value, CELL: cell_content} )
            datablock_entries.append(new_entry)
        else:
            if isinstance(existing_datablock_entry.data, str):
                existing_datablock_entry.data = {CELL: existing_datablock_entry.data, attribute: value}
            else:
                existing_datablock_entry.data[attribute] = value
        apply_datablock_from_entry_list(plug, datablock_entries)
    else:
        try:
            rows = plug.values[TABLE][ROWS]
        except KeyError:
            return abort(400)

        if len(rows) <= row_id:
            return abort(400)
        row = rows[row_id]
        try:
            row_data = row[ROW]
        except KeyError:
            return abort(400)
        if len(row_data) <= col_id:
            return abort(400)
        cell = row_data[col_id]
        if is_primitive(cell):
            row_data[col_id] = {CELL: cell, attribute: value}
        else:
            cell[attribute] = value

    plug.save()
    return json_response(prepare_for_and_call_dumbo(plug))



def get_plugin_from_paragraph(doc_id, par_id) -> (DocEntry, Plugin):
    """
    Returns the DocEntry and the plugin instance from a document and paragraph ID.
    :param doc_id: The document ID
    :param par_id: The paragraph ID
    :return: Tuple of a DocEntry and the plugin instance.
    """
    d = DocEntry.find_by_id(doc_id)
    if not d:
        abort(404)
    verify_edit_access(d)
    par = d.document_as_current_user.get_paragraph(par_id)
    return d, Plugin.from_paragraph(par)


def is_datablock(yaml: Dict[str, Any]) -> bool:
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


def create_datablock(table: Dict[str, Any]):
    """
    Creates tableDatablock
    :param table:
    :return:
    """
    table[DATABLOCK] = {}
    table[DATABLOCK][TYPE] = RELATIVE
    table[DATABLOCK][CELLS] = {}


@timTable_plugin.route("saveCell", methods=["POST"])
def tim_table_save_cell_list():
    """
    Saves cell content
    :return: The cell content as html
    """
    multi = []
    cell_content, docid, parid, row, col = verify_json_params('cellContent', 'docId', 'parId', 'row', 'col')
    d, plug = get_plugin_from_paragraph(docid, parid)
    yaml = plug.values
    # verify_edit_access(d)
    if is_in_global_append_mode(plug):
        raise NotImplementedError
        user = get_current_user_object()
        q = RowOwnerInfo.query
        # TODO figure out filter
        # q.filter()
    else:
        verify_edit_access(d)

    if is_datablock(yaml):
        save_cell(yaml[TABLE][DATABLOCK], row, col, cell_content)
    else:
        create_datablock(yaml[TABLE])
        save_cell(yaml[TABLE][DATABLOCK], row, col, cell_content)

    cc = str(cell_content)
    if plug.is_automd_enabled(True) and not cc.startswith(MD):
        cc = MD + cc
    settings = d.document.get_settings()
    html = call_dumbo([cc], DUMBO_PARAMS, options=plug.par.get_dumbo_options(base_opts=settings.get_dumbo_options()))
    plug.save()
    multi.append(html[0])
    return json_response(multi)


def save_cell(datablock: Dict[str, Any], row: int, col: int, cell_content: Union[str, Dict[str, Any]]):
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
        cells = datablock[CELLS]
        if coordinate in cells:
            existing_value = cells[coordinate]
            if isinstance(existing_value, dict):
                existing_value[CELL] = cell_content
                return

        datablock[CELLS].update({coordinate: cell_content})
    except:
        pass


def find_cell(rows: list, row: int, col: int) -> str:
    """
    Gets cell from index place if it exists, otherwise returns an empty string
    :param rows: List of cells
    :param row: Row index
    :param col: Column index
    :return: Cell from specified index
    """
    if row >= len(rows):
        return ''
    right_row = rows[row][ROW]
    if col >= len(right_row):
        return ''
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
    except KeyError:
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


def datablock_key_to_indexes(datablock_key: str) -> Tuple[int, int]:
    """
    Gets the column and row indexes from a single relative datablock entry.
    :param datablock_key: The entry in the relative datablock.
    :return: Column and row indexes in a tuple.
    """

    # get the letter part from the datablock key, for example AB12 -> AB
    columnstring = ""
    for c in datablock_key:
        if c.isalpha():
            columnstring += c
        else:
            break

    rowstring = datablock_key[len(columnstring):]
    row_index = int(rowstring)

    chr_index = len(columnstring) - 1
    column_index = 0
    for c in columnstring.encode('ascii'):
        # ascii encoding returns a list of bytes, so we can use c directly
        addition = ((ASCII_CHAR_COUNT**chr_index) * (c - ASCII_OF_A)) + 1
        column_index += addition
    return column_index - 1, row_index - 1


def is_in_global_append_mode(plug: Plugin) -> bool:
    """
    Checks whether global append mode is enabled.
    In global append mode even users without edit rights can add rows,
    but they can only edit the content of rows that they've added.
    :param plug: The plugin instance.
    :return: True if global append mode is enabled, otherwise false.
    """
    return plug.values.get(GLOBAL_APPEND_MODE, False)


def is_in_datainput_mode(plug: Plugin) -> bool:
    """
    Checks whether the table is in data input mode.
    :param plug: The plugin instance.
    :return: True if the table is in data input mode, otherwise false.
    """
    return plug.values.get(DATA_INPUT, False)


def is_review(request):
    """
    Check if request is review
    :param request:
    :return:
    """
    result = request.full_path.find("review=") >= 0
    return result


def prepare_for_and_call_dumbo(plug: Plugin):
    """
    Prepares the table's markdown for Dumbo conversion and
    runs it through Dumbo.
    :param plug: The plugin instance.
    :return: The conversion result from Dumbo.
    """
    par = plug.par
    if par:
        doc: Document = par.doc
        dumbo_opts = par.get_dumbo_options(base_opts=doc.get_settings().get_dumbo_options())
    else:
        dumbo_opts = DumboOptions.default()
    if plug.is_automd_enabled(default = True):
        return call_dumbo(prepare_for_dumbo(plug.values), DUMBO_PARAMS, options=dumbo_opts)

    return call_dumbo(plug.values, DUMBO_PARAMS, options=dumbo_opts)


def prepare_for_dumbo(values):
    """
    Prepares the table's markdown for Dumbo conversion when automd is enabled.
    :param values: The plugin paragraph's markdown.
    :return: The table's markdown, prepared for dumbo conversion.
    """

    try:
        rows = values[TABLE][ROWS]
    except KeyError:
        return values

    # regular row data
    for row in rows:
        rowdata = row[ROW]
        for i in range(len(rowdata)):
            cell = rowdata[i]
            if is_of_unconvertible_type(cell):
                    continue

            if isinstance(cell, str):
                if cell.startswith(MD):
                    continue
                rowdata[i] = MD + cell
            elif isinstance(cell, dict) and isinstance(cell[CELL], str):
                cell[CELL] = MD + cell[CELL]

    # datablock
    data_block = None
    try:
        data_block = values[TABLE][DATABLOCK][CELLS]
    except KeyError:
        pass

    if data_block is not None:
        for key, value in data_block.items():
            if isinstance(value, str):
                if not value.startswith(MD):
                    data_block[key] = MD + value
            elif isinstance(value, dict):
                for subkey, subvalue in value.items():
                    if isinstance(subvalue, str) and not subvalue.startswith(MD):
                        data_block[key][subkey] = MD + subvalue

    return values


def is_of_unconvertible_type(value):
    return isinstance(value, int) or isinstance(value, bool) or isinstance(value, float)


def is_primitive(value):
    return is_of_unconvertible_type(value) or isinstance(value, str)


def construct_datablock_entry_list_from_yaml(plug: Plugin) -> List[RelativeDataBlockValue]:
    """
    Parses a relative datablock and returns its data as a list of
    RelativeDataBlockValue instances.
    :param plug: The plugin instance.
    :return: A list of RelativeDataBlockValues.
    """
    try:
        values = plug.values[TABLE][DATABLOCK][CELLS]
    except KeyError:
        return []

    if not values:
        return []

    final_list = []
    for key, value in values.items():
        column_index, row_index = datablock_key_to_indexes(key)
        final_list.append(RelativeDataBlockValue(row_index, column_index, value))
    return final_list


def create_datablock_from_entry_list(relative_data_block_values: list) -> Dict[str, Any]:
    """
    Creates the datablock from a list of RelativeDataBlockValues.
    :param relative_data_block_values: The list of RelativeDataBlockValues.
    :return: The datablock as a dict.
    """
    cells = {}

    for entry in relative_data_block_values:
        key = colnum_to_letters(entry.column) + str(entry.row + 1)
        cells[key] = entry.data

    datablock = {}
    datablock[CELLS] = cells
    datablock[TYPE] = RELATIVE
    return datablock


def apply_datablock_from_entry_list(plug: Plugin, relative_data_block_values: list):
    plug.values[TABLE][DATABLOCK] = create_datablock_from_entry_list(relative_data_block_values)
