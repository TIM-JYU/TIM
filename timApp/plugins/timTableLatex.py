"""
Converts timTable-json into LaTeX.

Visa Naukkarinen
"""

import copy
import re
from typing import List

# Default values:

default_text_color = "black"
default_colspan = 1
default_rowspan = 1
default_font_size = 10
default_width = "*"  # * = auto-width
default_height = "0pt"  # Won't cut the first line of text even at 0pt.
default_text_h_align = "l"
default_font_family = "cmr"
# Color "none" isn't supported by LaTeX; if this value is used,
# the setting won't be added at all, making the color transparent.
default_transparent_color = "none"

# Mappings:
# TODO: Add missing pairs (at least the more common ones).
# HTML (from json) text-styles and corresponding LaTeX-replacements:
replace_pairs = [("<strong>", "\\textbf{"),
                 ("</strong>", "}"),
                 ("<em>", "\\textit{"),
                 ("</em>", "}"),
                 ("\\[", ""),
                 ("\\int", "?"),
                 ("\\]", "")]
# HTML font families and their LaTeX-codes:
fonts = {'monospace': 'pcr',
         'sans-serif': 'cmss'}


class TimTableException(Exception):
    """
    Base exception class.
    """


class TableBorderException(TimTableException):
    """
    If there's something wrong with border-related things.
    """


class IndexConversionError(TimTableException):
    """
    Error raised if attempt to convert Excel-type cell coordinate
    like A3 fails.
    """


class CellBorders:
    """
    Contains attributes of a cell's borders.
    """

    def __init__(self, left=False, right=False, top=False, bottom=False,
                 color_bottom=(default_transparent_color, False),
                 color_top=(default_transparent_color, False),
                 color_left=(default_transparent_color, False),
                 color_right=(default_transparent_color, False)):
        """
        :param left: Border-line exists.
        :param right:
        :param top:
        :param bottom:
        """
        # TODO: Add border style and thickness.
        # Whether the borderline should be drawn.
        self.left = left
        self.right = right
        self.top = top
        self.bottom = bottom
        # The color of the borderline.
        self.color_bottom = color_bottom
        self.color_top = color_top
        self.color_left = color_left
        self.color_right = color_right

    def __repr__(self) -> str:
        return custom_repr(self)


class Cell:
    """
    LaTeX-table cell containing all its attributes.
    """

    def __init__(
            self, index: int = -1, content: str = "",
            colspan: int = default_colspan, rowspan: int = default_rowspan,
            text_color=default_text_color, text_color_html: bool = False,
            bg_color=default_transparent_color, bg_color_html: bool = False,
            h_align=default_text_h_align, font_size=default_font_size,
            cell_width=default_width, cell_height=default_height,
            line_space=0, pbox="10cm", font_family=default_font_family,
            borders: CellBorders = CellBorders()):
        """

        :param index:
        :param content:
        :param colspan:
        :param rowspan:
        :param text_color:
        :param text_color_html:
        :param bg_color:
        :param bg_color_html:
        :param h_align:
        :param font_size:
        :param cell_width:
        :param cell_height:
        :param line_space:
        :param pbox: Length of an element that allows linebreaks in text.
        :param borders: Object containing border-data.
        """
        self.index = index
        self.content = content
        self.colspan = colspan
        self.rowspan = rowspan
        self.text_color = text_color
        self.text_color_html = text_color_html
        self.bg_color = bg_color
        self.bg_color_html = bg_color_html
        self.h_align = h_align
        self.cell_width = cell_width
        self.cell_height = cell_height
        self.font_size = font_size
        self.line_space = line_space
        self.pbox = pbox
        self.borders = borders
        self.font_family = font_family

    def __str__(self) -> str:
        # LaTeX has text-h-align and cell-v-borders in the same place:
        v_border_and_align = ""
        if self.borders.left:
            l_b_color, l_b_html = self.borders.color_left
            v_border_and_align += f"!{{\color{format_color(l_b_color, l_b_html)}\\vrule}}"
        v_border_and_align += self.h_align
        if self.borders.right:
            r_b_color, r_b_html = self.borders.color_right
            v_border_and_align += f"!{{\color{format_color(r_b_color, r_b_html)}\\vrule}}"

        # HTML-colors have an extra tag
        cell_color = format_color(self.bg_color, self.bg_color_html)
        if self.bg_color == "none":
            cell_color = ""
        else:
            cell_color = f"\\cellcolor{cell_color}"
        content = self.content

        return f"\\multicolumn{{{self.colspan}}}{{{v_border_and_align}}}{{" \
               f"\\multirow{{{self.rowspan}}}{{{self.cell_width}}}{{" \
               f"{cell_color}" \
               f"\\fontsize{{{self.font_size}}}{{{self.line_space}}}" \
               f"\\selectfont{{\\textcolor{{{self.text_color}}}{{{{" \
               f"\\fontfamily{{{self.font_family}}}\\selectfont{{" \
               f"\\centering {content}}}}}}}}}}}}}"

    def __repr__(self) -> str:
        return custom_repr(self)


def format_color(color: str, html_color: bool) -> str:
    """
    Converts color to LaTeX-format depending on whether it's
    html or normal color.
    :param color: Color name or hex-code.
    :param html_color: Whether the color is in hex or not.
    :return:
    """
    if html_color:
        return f"[HTML]{{{color}}}"
    else:
        return f"{{{color}}}"


class Row:
    """
    LaTeX-table row.
    """

    def __init__(self, index: int, cells: List[Cell]) -> None:
        """
        :param index: Row index.
        :param cells: A list of the cells this row contains.
        """
        self.index = index
        self.cells = cells

    def __str__(self) -> str:
        """
        :return: LaTeX-format of all the cells in row, separated by '&'.
        """
        if not self.cells or len(self.cells) < 1:
            return ""
        output = f"{str(self.cells[0])} "
        for i in range(1, len(self.cells)):
            output += f"& {str(self.cells[i])}"
        return output

    def get_row_height(self) -> str:
        """
        Gives the largest cell height to be used as row height.
        Currently this way because separate cell heights aren't supported.
        :return:
        """
        height = 0
        for i in range(0, len(self.cells)):
            height = max(height, pt_to_float(self.cells[i].cell_height))
        return f"{height}pt"

    def add_cell(self, i: int, cell: Cell) -> None:
        """
        Adds a cell to index in row and uses the first free index.
        :param i:
        :param cell:
        :return:
        """
        # index:rowspan in html/json-format:
        # 0:1  1:2  2:1  3:1  4:2
        # 0:1       1:1  2:1
        #
        # index:rowspan in LaTeX-format:
        # 0:1  1:1  2:1  3:1  4:1
        # 0:1  1:-2 2:1  3:1  4:-2

        # TODO: find the optimal place for sorting
        # Sorting the cells by index is necessary.
        self.cells = sorted(self.cells, key=lambda c: c.index)

        for j in range(i, len(self.cells)):
            if i == self.cells[j].index:
                i = i + 1
        cell.index = i
        self.cells.append(cell)

    def get_rowspan(self) -> int:
        """
        Get the sum of row's cells' colspans.
        :return:
        """
        i = 0
        for cell in self.cells:
            i += cell.colspan
        return i

    def __repr__(self):
        return custom_repr(self)


def pt_to_float(pt: str) -> float:
    """
    Parses a float from LaTeX pt units;
    for example "12.333pt" -> 12.333.
    :param pt:
    :return:
    """
    return float(pt.replace("pt", "").strip())


class HorizontalBorder:
    """
    Horizontal line between rows.
    """

    def __init__(self, row_above: Row = None, row_below: Row = None) -> None:
        """
        In LaTeX there can't be duplicate h-lines, so the line needs to be a
        composite of all cell-borders from rows above and below.
        :param row_above:
        :param row_below:
        """
        self.row_above = row_above
        self.row_below = row_below

    def __str__(self) -> str:
        """
        Draws line "-" between cells if there's a border coming from above or below,
        otherwise no line "~".
        :return:
        """
        output = ""

        # Get cell-count from the rows.
        try:
            above_count = self.row_above.get_rowspan()
        # If there's no row:
        except AttributeError:
            above_count = 0
        try:
            below_count = self.row_below.get_rowspan()
        except AttributeError:
            below_count = 0

        max_count = max(above_count, below_count)
        if max_count <= 0:
            raise TableBorderException("Table row not found")

        colspan_counter_lower = 0
        colspan_counter_upper = 0

        for i in range(0, max_count):
            # Some default values:
            i_upper = False
            i_upper_colspan = 1
            i_lower = False
            i_lower_colspan = 1
            color_above = None
            color_below = None

            # These try to keep count of index drift caused by multi-column cells.
            index_lower = i + colspan_counter_lower
            index_upper = i + colspan_counter_upper

            # This block checks if there actually is a row above/below
            # and tries getting data from there without crashing if thing
            # this asks doesn't exist.

            if self.row_above:
                try:
                    i_upper = self.row_above.cells[index_upper].borders.bottom
                    i_upper_colspan = self.row_above.cells[index_upper].colspan
                    # upper_cont = self.row_above.cells[index_upper].content
                # If the row doesn't exist, won't assert the
                # need for a line from that direction.
                except IndexError or AttributeError as e:
                    pass
                try:
                    color_above = self.row_above.cells[index_upper].borders.color_bottom
                    # print(color_above)
                    if color_above == default_transparent_color:
                        color_above = None
                except IndexError or AttributeError as e:
                    pass
            if self.row_below:
                try:
                    i_lower = self.row_below.cells[index_lower].borders.top
                    i_lower_colspan = self.row_below.cells[index_lower].colspan
                    # lower_cont = self.row_below.cells[index_lower].content
                except IndexError or AttributeError as e:
                    pass
                try:
                    color_below = self.row_below.cells[index_lower].borders.color_top
                    # print(color_below)
                    if color_below == default_transparent_color:
                        color_below = None
                except IndexError or AttributeError as e:
                    pass

            colspan = max(i_upper_colspan, i_lower_colspan)

            # TODO: Something wrong with the logic here.
            # After multirow-multicolumn cell, following cell borders won't show correctly:
            # instead there's too many border slots added.

            # Draws the line only if either cell above or below wants one.
            if not i_upper and not i_lower:
                output += colspan * "~"
            else:
                # If border color of above cell is default or there's no row above, use color from below cell.
                if not color_above and color_below or color_above[0] == default_transparent_color and color_below:
                    color, html_color = color_below
                # Otherwise default to top color.
                else:
                    color, html_color = color_above
                # If no color, don't draw the line.
                if color == default_transparent_color:
                    output += colspan * "~"
                # Multicolumn cell counts as one cell but require multiple borderlines.
                else:
                    output += colspan * f">{{\\arrayrulecolor{format_color(color, html_color)}}}-"
            colspan_counter_lower = colspan_counter_lower + i_upper_colspan - 1
            colspan_counter_upper = colspan_counter_upper + i_lower_colspan - 1
        return output

    def __repr__(self) -> str:
        return custom_repr(self)


class Table:
    """
    Table with rows, cells in rows, and horizontal borders between rows.
    """

    def __init__(self, rows: List[Row], col_count: int = 0) -> None:
        """
        :param rows: All rows of the table in a list.
        :param col_count: The largest number of columns in any row of the table.
        """
        self.rows = rows
        self.col_count = col_count
        self.hborders = []

    def __str__(self) -> str:
        """
        :return: The complete table in LaTeX-format.
        """
        if not self.rows or len(self.rows) < 1:
            return ""
        # 'c' would be text horizontal alignment, but it's actually set elsewhere,
        # so here it tells only the highest amount of cols in the table.
        columns = "c" * self.col_count
        prefix = f"\\begin{{table}}\n" \
                 "\\resizebox{\\columnwidth}{!}{%\n" \
                 f"\\begin{{tabular}}{{{columns}}}"
        postfix = "\\end{tabular}%\n}\n\\end{table}"
        output = ""
        for i in range(0, len(self.rows)):
            output += f"\n\\hhline{{{str(self.hborders[i])}}}" \
                      f"\n{str(self.rows[i])}" \
                      f"\n\\tabularnewline[{self.rows[i].get_row_height()}]"

        output += f"\n\\hhline{{{str(self.hborders[len(self.hborders)-1])}}}"
        return f"{prefix}\n{output}\n{postfix}"

    def get_or_create_row(self, i: int) -> Row:
        """
        Returns the row in index or creates a new one with with said index.
        :param i: Row index.
        :return: The row with index i, whether it existed or not.
        """
        try:
            return self.rows[i]
        except IndexError:
            empty_row = Row(i, [])
            self.rows.append(empty_row)
            return empty_row

    def create_hborders(self) -> None:
        """
        Once all rows have been created, create the borders between them.
        :return:
        """
        self.hborders.append(HorizontalBorder(row_above=None, row_below=self.rows[0]))
        for i in range(0, len(self.rows) - 1):
            self.hborders.append(
                HorizontalBorder(
                    row_above=self.rows[i], row_below=self.rows[i + 1]
                )
            )
        self.hborders.append(
            HorizontalBorder(
                row_above=self.rows[len(self.rows) - 1],
                row_below=None
            )
        )

    def __repr__(self) -> str:
        return custom_repr(self)


def custom_repr(obj) -> str:
    """
    Returns full contents of the object and the objects it references.
    :param obj:
    :return:
    """
    return f"{str(obj.__class__)}: {str(obj.__dict__)}"


def get_content(content: str) -> str:
    """
    Converts html-elements inside the cell into LaTex.
    :param content: Text / other content in the cell.
    :return:
    """
    # TODO: Add conversion of math etc. misc. elements.
    text = content.strip()
    # Contains corresponding html and LaTeX elements.
    for (j, l) in replace_pairs:
        text = text.replace(j, l)
    # Unknown html-formattings will be displayed as question marks.
    text = re.sub(r'<.+?>', '?', text).replace('\r', '').replace('\n', '')
    return text


def get_color(item, key: str, default_color: str, default_color_html: bool) -> (str, bool):
    """
    Parses color-data into LaTeX-format.
    :param item:
    :param key: Key for color element (color, backgroundColor, etc.).
    :param default_color: Color to use if key not found.
    :param default_color_html:
    :return: The color-code / name and whether its in hex or not.
    """
    # Normal LaTex doesn't recognize some html colors, so they need to be defined in the tex-file.
    color = default_color
    color_html = default_color_html
    try:
        color = item[key]
        if "#" in color:
            color_html = True
            color = color.replace("#", "")
        else:
            color_html = False
    except KeyError:
        pass
    finally:
        return color, color_html


def get_span(item) -> (int, int):
    """
    Parses row and column span of the cell.
    If not specified, assume it's 1.
    :param item:
    :return:
    """
    try:
        colspan = item['colspan']
    except:
        colspan = default_colspan
    try:
        rowspan = item['rowspan']
    except:
        rowspan = default_rowspan
    return colspan, rowspan


def get_size(item, default_width, default_height) -> (float, float):
    """
    Parse cell width and height into LaTeX-supported format.
    :param item:
    :param default_width: Value to be used if no width-key.
    :param default_height: Value to be used if no height-key.
    :return:
    """
    # TODO: Cases with more than just a number?
    try:
        # TimTable uses measurements that are roughly thrice as large as LaTeX pts.
        width = f"{item['width']/3}pt"
    except:
        width = default_width
    try:
        height = f"{item['height']/3}pt"
    except:
        height = default_height
    return width, height


def get_font_family(item) -> str:
    """
    :param item:
    :return:
    """
    try:
        # Corresponding HTML and LaTeX codes need to be mapped here.
        ff = item['fontFamily']
        font = fonts[ff]
    except:
        font = default_font_family
    return font


def get_text_horizontal_align(item):
    """
    Parses text horizontal alignment.
    :param item:
    :return:
    """
    try:
        # Options are center, right and left, which happen to be the same in LaTeX,
        # except only first letters are used.
        a = item['textAlign'][:1]
    except:
        a = default_text_h_align
    return a


def get_font_size(item):
    """
    Gets text size if set, and uses default otherwise.
    :param item:
    :return:
    """
    try:
        a = item['fontSize']
    except:
        a = default_font_size
    return a


def get_borders(item) -> CellBorders:
    """
    Creates a CellBorder object with corresponding border-data.
    :param item:
    :return:
    """
    try:
        borders = CellBorders(True, True, True, True)
        border_data = item['border']
        if border_data:
            arg_count = border_data.count(" ")
            color = default_text_color
            color_html = False
            if arg_count == 2:
                color = border_data[border_data.rfind(" "):].strip()
                if "#" in color:
                    color_html = True
                    color = color.replace("#", "")
            borders.color_bottom = color, color_html
            borders.color_top = color, color_html
            borders.color_left = color, color_html
            borders.color_right = color, color_html

            return borders
    except:
        borders = CellBorders()
        try:
            if item['borderLeft']:
                borders.left = True
        except:
            pass
        try:
            if item['borderRight']:
                borders.right = True
        except:
            pass
        try:
            if item['borderTop']:
                borders.top = True
        except:
            pass
        try:
            if item['borderBottom']:
                borders.bottom = True
        except:
            pass
        return borders


def copy_cell(cell: Cell) -> Cell:
    """
    Properly copies Cell-object and the objects within.
    :param cell:
    :return: Copy with new object pointers.
    """
    n_cell = copy.copy(cell)
    n_cell.borders = copy.copy(cell.borders)
    return n_cell


def char_to_int(char: str) -> int:
    """
    Converts alphabets to integers. Starts from a = 0.
    :param char: Single character.
    :return:
    """
    if not char or len(char) > 1:
        raise IndexConversionError(char)
    i = ord(char.lower()) - ord('a')
    if i < 0:
        raise IndexConversionError(char)
    else:
        return i


def convert_datablock_index(index: str) -> (int, int):
    """
    Converts string like "A1" to row and cell indexes (0,0).
    :param index:
    :return: Pair of integer indexes.
    """
    try:
        row = char_to_int(index[0])
        cell = int(index[1:]) - 1
    except TypeError:
        raise IndexConversionError(index)
    else:
        return row, cell


def get_datablock(table):
    """
    Returns datablock or None, if table has no datablock element.
    :param table:
    :return:
    """
    try:
        datablock = table['datablock']['cells']
    except KeyError:
        datablock = None
    return datablock


def update_content_from_datablock(datablock, row, cell, content):
    """
    Updates the cell content based on datablock if a match is found.
    :param datablock:
    :param row: Row-index.
    :param cell: Cell-index.
    :param content: Original content.
    :return:
    """
    output = content
    try:
        datablock_index = f"{str(chr(65+cell))}{row+1}"
        # print(datablock_index, datablock[datablock_index])
        output = datablock[datablock_index]
    except TypeError or KeyError:
        pass
    finally:
        # If index conversion fails or there's no such key,
        # return the original content value.
        return output


def convert_table(table_json) -> Table:
    """
    Converts json-table into a LaTeX-table.
    :param table_json:
    :return: Table-object containing the rows and cells in LaTex.
    """
    table_rows = []
    table = Table(table_rows)
    datablock = get_datablock(table_json)

    max_cells = 0
    max_colspan = 1
    for i in range(0, len(table_json['rows'])):
        table_row = table.get_or_create_row(i)

        # If row settings found, use them as default values for cells of the row.
        # Otherwise uses global defaults.
        row_data = table_json['rows'][i]
        (row_default_bg_color, row_default_bg_color_html) = get_color(row_data, "backgroundColor",
                                                                      default_transparent_color, False)
        (row_default_text_color, row_default_text_color_html) = get_color(row_data, "color", default_text_color, False)
        (row_default_width, row_default_height) = get_size(row_data, default_width, default_height)

        for j in range(0, len(table_json['rows'][i]['row'])):
            try:
                cell_data = table_json['rows'][i]['row'][j]
                content = get_content(table_json['rows'][i]['row'][j]['cell'])
            # Cells that use simplified format (without 'cell').
            except TypeError:
                content = get_content(table_json['rows'][i]['row'][j])
            finally:
                # Set cell attributes:
                (bg_color, bg_color_html) = get_color(
                    cell_data,
                    'backgroundColor',
                    row_default_bg_color,
                    row_default_bg_color_html)
                (text_color, text_color_html) = get_color(
                    cell_data,
                    'color',
                    row_default_text_color,
                    row_default_text_color_html)
                (colspan, rowspan) = get_span(cell_data)
                (width, height) = get_size(cell_data, row_default_width, row_default_height)
                borders = get_borders(cell_data)
                text_h_align = get_text_horizontal_align(cell_data)
                font_family = get_font_family(cell_data)
                font_size = get_font_size(cell_data)

                # For estimating column count:
                max_colspan = max(max_colspan, colspan)

                c = Cell(
                    content=content,
                    font_family=font_family,
                    font_size=font_size,
                    h_align=text_h_align,
                    bg_color=bg_color,
                    bg_color_html=bg_color_html,
                    text_color=text_color,
                    text_color_html=text_color_html,
                    colspan=colspan,
                    rowspan=rowspan,
                    cell_width=width,
                    cell_height=height,
                    borders=borders
                )

                # Tries updating content from the datablock.
                if datablock:
                    c.content = update_content_from_datablock(datablock, i, j, c.content)

                # Cells with rowspan > 1:
                # Multirow-cells need to be set from bottom-up in LaTeX to properly show bg-colors, and
                # empty cells need to be placed above to avoid overlap, since LaTeX doesn't automatically
                # move cells aside.
                if rowspan > 1:
                    for y in range(0, rowspan - 1):
                        # Empty filler cell has mostly same the settings as the multirow-cell:
                        d = copy_cell(c)
                        d.content = ""
                        d.borders.color_bottom = (c.bg_color, c.bg_color_html)
                        if y > 1:
                            d.borders.color_top = (c.bg_color, c.bg_color_html)
                        d.rowspan = 1
                        table.get_or_create_row(i + y).add_cell(j, d)
                    c.borders.color_top = (c.bg_color, c.bg_color_html)
                    c.rowspan = -rowspan
                    table.get_or_create_row(i + rowspan - 1).add_cell(j, c)

                # Normal cells:
                else:
                    table_row.add_cell(j, c)
                max_cells = max(max_cells, len(table_row.cells))

    # TODO: Need to find the number of most concurrent cols more accurately.
    # Currently plays it safe by overestimating.
    table.col_count = max_cells * max_colspan
    table.create_hborders()
    return table


testi_data5 = {'rows': [{'color': 'yellow', 'backgroundColor': 'red', 'width': 100,
                        'row': [{'cell': 'kissa'}, 'rolll', 'koll', 'koukkukäsi', 'kekola',
                                {'cell': 'New column', 'width': 200},
                                {'cell': 'New column'}]}, {
                           'row': [{'cell': 'kissa'}, 'rolll', 'koll', 'koukkukäsi', 'kekola',
                                   {'cell': 'New column', 'color': 'fuchsia'},
                                   {'cell': 'New column'}]}], 'datablock': {
    'cells': {'E1': 'OMENA', 'F1': 'APPELSIINI', 'C1': 'KIIVI', 'G2': 'KOOKOS', 'B2': 'KUMKWATTI', 'A2': 'SITRUUNA',
              'G1': 'BANAANI', 'F2': 'AVOKADO', 'E2': 'MELONI', 'D1': 'CHILI', 'A1': 'PURJO', 'B1': 'PERSIMON',
              'D2': 'BASILIKA', 'C2': 'BAMBU'}, 'type': 'relative'}}
print(convert_table(testi_data5))

test_data3 = {'rows': [{'backgroundColor': 'cyan', 'row': [{'cell': 'Rami Pasanen ', 'type': 'text'},
                                                           {'cell': 'Keijo Kukkanen', 'colspan': 2,
                                                            'horizontal-align': 'right'},
                                                           {'cell': 'Visa', 'border': '10px solid red'},
                                                           {'backgroundColor': 'blue', 'cell': 'Visa'},
                                                           {'verticalAlign': 'middle', 'textAlign': 'center',
                                                            'rowspan': 2, 'cell': 'kk', 'colspan': 2},
                                                           '<strong>Kasimir</strong>']}, {
                           'row': [{'cell': '1-10', 'borderBottom': '1px solid purple'},
                                   {'cell': '1-11,5-6', 'borderBottom': '1px none white'}, 'Kukkuu',
                                   '<span class="red">1-11,5-6</span>', {'cell': '1-11,5-6'}, {'cell': '1-11,5-6'},
                                   {'backgroundColor': 'coral', 'cell': '1-11,5-6'}, {'cell': '1-11,5-6'}]}, {
                           'row': [{'verticalAlign': 'bottom', 'backgroundColor': 'blue', 'cell': '1-10'},
                                   '<div class="figure">\n<img src="/images/108/vesa640.png" alt="vesa" />\n<p class="caption">vesa</p>\n</div>',
                                   {'cell': '1-11,5-6'}, {'cell': '1-11,5-6'}, '<span class="timButton">Paina</span>',
                                   {'cell': '1-11,5-6'}, {'cell': '1-11,5-6'},
                                   {'cell': '<span class="math display">\\[\\int f(x) dx\\]</span>'}]}],
              'datablock': {'cells': {'H3': 'banaani', 'E1': 'sdjukfsdf', 'B1': 'Kissa'}, 'type': 'relative'},
              'columns': [{'style': 'exampleStyle', 'backgroundColor': 'blue', 'width': '1px',
                           'formula': '=(kurssinumero * opintopisteet)', 'column': None},
                          {'column': {'width': 50, 'name': 'kurssinumero'}},
                          {'column': {'width': 100, 'name': 'opintopisteet'}},
                          {'backgroundColor': 'yellow', 'column': None}]}

test_data4 = {'rows': [{'backgroundColor': 'cyan', 'row': [{'cell': 'Rami Pasanen ', 'type': 'text'},
                                                           {'cell': 'Keijo Kukkanen', 'colspan': 2,
                                                            'horizontal-align': 'right'},
                                                           {'cell': 'Visa', 'border': '10px solid red'},
                                                           {'backgroundColor': 'blue', 'cell': 'Visa'},
                                                           {'verticalAlign': 'middle', 'textAlign': 'center',
                                                            'rowspan': 2, 'cell': 'kk', 'colspan': 2},
                                                           '<strong>Kasimir</strong>']}, {
                           'row': [{'cell': '1-10', 'borderBottom': '1px solid purple'},
                                   {'cell': '1-11,5-6', 'borderBottom': '1px none white'}, 'Kukkuu',
                                   '1-11,5-6', {'cell': '1-11,5-6'}, {'cell': '1-11,5-6'},
                                   {'backgroundColor': 'coral', 'cell': '1-11,5-6'}, {'cell': '1-11,5-6'}]}, {
                           'row': [{'verticalAlign': 'bottom', 'backgroundColor': 'blue', 'cell': '1-10'},
                                   'vesa',
                                   {'cell': '1-11,5-6'}, {'cell': '1-11,5-6'}, 'Paina',
                                   {'cell': '1-11,5-6'}, {'cell': '1-11,5-6'},
                                   {'cell': 'f(x) dx'}]}],
              'datablock': {'cells': {'H3': 'banaani', 'E1': 'AAAAAAAAAA!', 'B1': 'Kissa'}, 'type': 'relative'},
              'columns': [{'style': 'exampleStyle', 'backgroundColor': 'blue', 'width': '1px',
                           'formula': '=(kurssinumero * opintopisteet)', 'column': None},
                          {'column': {'width': 50, 'name': 'kurssinumero'}},
                          {'column': {'width': 100, 'name': 'opintopisteet'}},
                          {'backgroundColor': 'yellow', 'column': None}]}

test_data2 = {'rows': [{'row': [{'height': 50, 'verticalAlign': 'bottom', 'backgroundColor': '#dddddd', 'width': 600,
                                 'cell': '<strong><em>Jäsen:</em></strong>', 'border': '1px solid gray',
                                 'textAlign': 'center', 'fontSize': 20},
                                {'height': 50, 'verticalAlign': 'bottom', 'backgroundColor': '#dddddd', 'width': 200,
                                 'cell': '<strong><em>Läsnä asiat blablablaaaa blaa blaaa pitkääääääää:</em></strong>',
                                 'border': '1px solid gray'},
                                {'height': 50, 'verticalAlign': 'bottom', 'backgroundColor': '#dddddd', 'width': 400,
                                 'cell': '<strong><em>Varajäsen:</em></strong>', 'fontFamily': 'sans-serif',
                                 'border': '1px solid gray', 'textAlign': 'left', 'color': 'blue',
                                 'rowspan': 2},
                                {'height': 50, 'verticalAlign': 'bottom', 'backgroundColor': '#dddddd', 'width': 200,
                                 'cell': '<strong><em>Läsnä asiat:</em></strong>', 'border': '1px solid gray'}]},
                       {'row': [{'cell': '<em>Professorit </em>', }, {'cell': 'tyhjä'}, {'cell': 'tyhjä'},
                                {'cell': 'tyhjä'}]},
                       {'row': [{'cell': 'Dekaani, TkT Pasi Tyrväinen', 'textAlign': 'right'}, {'cell': 'tyhjä'},
                                {'cell': 'tyhjä'},
                                {'cell': 'tyhjä'}]}, {
                           'row': [{'cell': 'Professori, FT Raino A.E. Mäkinen'}, {'cell': 'tyhjä'}, {'cell': 'tyhjä'},
                                   {'cell': 'tyhjä'}]},
                       {'row': [{'cell': 'Professori, FT Raino A.E. Mäkinen', 'fontFamily': 'monospace'},
                                {'cell': 'tyhjä'},
                                {'cell': ' Professori, FT Tuomo Rossi '}, {'cell': 'tyhjä'}]}, {
                           'row': [{'cell': 'Professori, FT Timo Hämäläinen '}, {'cell': 'tyhjä'},
                                   {'cell': 'Professori, FT Tapani Ristaniemi  '}, {'cell': 'tyhjä'}]}, {
                           'row': [{'cell': 'Professori, FT Tommi Kärkkäinen   '}, {'cell': 'tyhjä'},
                                   {'cell': 'Tutkimusprofessori, TkT Lauri Kettunen', 'border': '1px solid'},
                                   {'cell': 'tyhjä'}]}, {
                           'row': [{'height': 70, 'cell': '<em>Muu opetus- ja tutkimushlöstö ja muu hlöstö</em>'},
                                   {'cell': 'tyhjä'}, {'cell': 'tyhjä'}, {'cell': 'tyhjä'}]}, {
                           'row': [{'cell': 'Projektitutkija, FT Tero Tuovinen', 'border': '1px solid gray'},
                                   {'cell': 'tyhjä', 'border': '1px solid #123456'},
                                   {'cell': 'Lehtori, KTT, LitM Panu Moilanen '}, {'cell': 'tyhjä'}]}, {
                           'row': [{'cell': 'Lehtori, FT Vesa Lappalainen '}, {'cell': 'tyhjä'},
                                   {'cell': 'Yliopistonopettaja, FT Sanna Mönkölä '}, {'cell': 'tyhjä'}]}, {
                           'row': [{'cell': 'Yliopistonopettaja, FT Antti-Juhani Kaijanaho'}, {'cell': 'tyhjä'},
                                   {'cell': 'Yliopistonopettaja, FT Leena Hiltunenä  '}, {'cell': 'tyhjä'}]}, {
                           'row': [{'verticalAlign': 'bottom', 'cell': '<em>Opiskelijat</em>', 'rowspan': 3,
                                    'backgroundColor': '#eeeeee', 'border': '1px solid yellow'}, {'cell': 'tyhjä'},
                                   {'cell': 'tyhjä', 'border': '1px solid black'},
                                   {'height': 70, 'cell': 'tyhjä', 'border': '1px solid yellow'}]},
                       {'row': [{'cell': 'Eveliina Mali', 'colspan': 3, 'backgroundColor': 'red',
                                 'border': '1px solid gray'}, {'cell': 'tyhjä'},
                                {'cell': 'Renne Hirsimäki', 'border': '1px solid blue'},
                                {'cell': 'tyhjä'}]},
                       {'row': [{'cell': 'Riku Tulla'}, {'cell': 'tyhjä'}, {'cell': ' Antti Louko    '},
                                {'cell': 'tyhjä'}]},
                       {'row': [{'cell': 'Leo Toiminen'}, {'cell': 'tyhjä'}, {'cell': 'Nuutti Rantanen    '},
                                {'cell': 'tyhjä'}]},
                       {'row': [{'cell': '<em>Ulkopuoliset jäsenet</em>'}, {'cell': 'tyhjä'}, {'cell': 'tyhjä'},
                                {'cell': 'tyhjä'}]}, {
                           'row': [{'rowspan': 2, 'cell': 'Hannu Häkkinen', 'colspan': 2, 'backgroundColor': 'yellow'},
                                   {'cell': 'Ei varajäsentä'}, {'cell': 'tyhjä'}]},
                       {'row': [{'cell': 'Ari Hirvonen'}, {'cell': 'tyhjä'}]},
                       {'row': ['<em>Banaani</em>', '<strong>Omena</strong>']}]}

test_data = {'rows': [{'row': [{'height': 50, 'verticalAlign': 'bottom', 'backgroundColor': '#dddddd', 'width': 600,
                                'cell': '<strong><em>Jäsen:</em></strong>', 'border': '1px solid gray'},
                               {'height': 50, 'verticalAlign': 'bottom', 'backgroundColor': '#dddddd', 'width': 200,
                                'cell': '<strong><em>Läsnä asiat:</em></strong>', 'border': '1px solid gray'},
                               {'height': 50, 'verticalAlign': 'bottom', 'backgroundColor': '#dddddd', 'width': 400,
                                'cell': '<strong><em>Varajäsen:</em></strong>', 'border': '1px solid gray'},
                               {'height': 50, 'verticalAlign': 'bottom', 'backgroundColor': '#dddddd', 'width': 200,
                                'cell': '<strong><em>Läsnä asiat:</em></strong>', 'border': '1px solid gray'}]},
                      {'row': [{'cell': '<em>Professorit </em>'}, {'cell': ''}, {'cell': ''}, {'cell': ''}]},
                      {'row': [{'cell': 'Dekaani, TkT Pasi Tyrväinen'}, {'cell': ''}, {'cell': ''}, {'cell': ''}]}, {
                          'row': [{'cell': 'Professori, FT Raino A.E. Mäkinen'}, {'cell': ''}, {'cell': ''},
                                  {'cell': ''}]}, {'row': [{'cell': 'Professori, FT Raino A.E. Mäkinen'}, {'cell': ''},
                                                           {'cell': ' Professori, FT Tuomo Rossi '}, {'cell': ''}]}, {
                          'row': [{'cell': 'Professori, FT Timo Hämäläinen '}, {'cell': ''},
                                  {'cell': 'Professori, FT Tapani Ristaniemi  '}, {'cell': ''}]}, {
                          'row': [{'cell': 'Professori, FT Tommi Kärkkäinen   '}, {'cell': ''},
                                  {'cell': 'Tutkimusprofessori, TkT Lauri Kettunen'}, {'cell': ''}]}, {
                          'row': [{'height': 70, 'cell': '<em>Muu opetus- ja tutkimushlöstö ja muu hlöstö</em>'},
                                  {'cell': ''}, {'cell': ''}, {'cell': ''}]}, {
                          'row': [{'cell': 'Projektitutkija, FT Tero Tuovinen'}, {'cell': ''},
                                  {'cell': 'Lehtori, KTT, LitM Panu Moilanen '}, {'cell': ''}]}, {
                          'row': [{'cell': 'Lehtori, FT Vesa Lappalainen '}, {'cell': ''},
                                  {'cell': 'Yliopistonopettaja, FT Sanna Mönkölä '}, {'cell': ''}]}, {
                          'row': [{'cell': 'Yliopistonopettaja, FT Antti-Juhani Kaijanaho'}, {'cell': ''},
                                  {'cell': 'Yliopistonopettaja, FT Leena Hiltunenä  '}, {'cell': ''}]}, {
                          'row': [{'verticalAlign': 'bottom', 'cell': '<em>Opiskelijat</em>'}, {'cell': ''},
                                  {'cell': ''}, {'height': 70, 'cell': ''}]},
                      {'row': [{'cell': 'Eveliina Mali'}, {'cell': ''}, {'cell': 'Renne Hirsimäki'}, {'cell': ''}]},
                      {'row': [{'cell': 'Riku Tulla'}, {'cell': ''}, {'cell': ' Antti Louko    '}, {'cell': ''}]},
                      {'row': [{'cell': 'Leo Toiminen'}, {'cell': ''}, {'cell': 'Nuutti Rantanen    '}, {'cell': ''}]},
                      {'row': [{'cell': '<em>Ulkopuoliset jäsenet</em>'}, {'cell': ''}, {'cell': ''}, {'cell': ''}]},
                      {'row': [{'cell': 'Hannu Häkkinen'}, {'cell': ''}, {'cell': 'Ei varajäsentä'}, {'cell': ''}]},
                      {'row': [{'cell': 'Ari Hirvonen'}, {'cell': ''}, {'cell': 'Ei varajäsentä'}, {'cell': ''}]}]}

# -----------------------------------------------------------------------------
# Testing:

table = convert_table(test_data2)
print("\n" + str(table) + "\n")
print("\n", convert_table(test_data), "\n")
print("\n", convert_table(test_data4), "\n")

"""
# For checking correct indexing:
for row in table.rows:
    temp = str(row.index) + ": "
    for cell in row.cells:
        temp += str(cell.index) + ":" + repr(cell.content) + " "
    print(temp)
"""

