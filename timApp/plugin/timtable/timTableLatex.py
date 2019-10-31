"""
Converts timTable-json into LaTeX.

Visa Naukkarinen
"""

import copy
import re
from typing import List, Union, Tuple, Callable, Dict, Any

# Default values:

default_text_color = "black"
default_colspan = 1
default_rowspan = 1
default_font_size = 12
default_width = "*"  # * = auto-width
default_height = "0"  # Won't cut the first line of text even at 0pt.
default_text_h_align = "l"

# TODO: Fonts don't work inside the table if XeLaTeX is used!
default_font_family = "qpl"

# Color "none" isn't supported by LaTeX; if this value is used,
# the setting won't be added at all, making the color transparent.
default_transparent_color = "none"
default_table_width = "\\columnwidth"
default_table_height = "!"
default_table_float = "[H]"
default_minipage_width = "4cm"
wrap_cell_threshold = 50  # The content length limit for when to assume need for more than one line in cell.
# Maximum number of columns in table; overtly large count will crash LaTeX-conversion.
default_max_col_count = 250
# Pixels; 595px is the width of 72 dpi (web) a4 and minus 35px is for the margins.
resizing_px_threshold = 595 - 35
# Minimum number of columns (checked when table width is estimated by cell content width).
resizing_col_threshold = 2

# TODO: Add more.
# HTML font families and their closest corresponding LaTeX-codes:
fonts = {'monospace': 'pcr',
         'sans-serif': 'cmss',
         'times': 'ptm',
         'calibri': 'cmss'}


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
    Contains the attributes of a cell's borders.
    """

    def __init__(self, left=False, right=False, top=False, bottom=False,
                 color_bottom=(default_transparent_color, False),
                 color_top=(default_transparent_color, False),
                 color_left=(default_transparent_color, False),
                 color_right=(default_transparent_color, False)):
        """
        :param left: Whether the left-side cell border exists.
        :param right: Right border existence.
        :param top: Top border existence.
        :param bottom: Bottom border existence.
        :param color_bottom: The color of bottom border as tuple containing
               color code/name and whether it's hex or not.
        :param color_top: The color of top border.
        :param color_left: The color of left border.
        :param color_right: The color of right border.
        """
        # TODO: Add border style and thickness.
        self.left = left
        self.right = right
        self.top = top
        self.bottom = bottom
        self.color_bottom = color_bottom
        self.color_top = color_top
        self.color_left = color_left
        self.color_right = color_right

    def __repr__(self) -> str:
        return custom_repr(self)

    def set_all_borders(self, color: Tuple[str, bool]) -> None:
        """
        Set all borders visible and with same color.
        :param color: Tuple containing color name or hex and whether it's in hex.
        :return: None.
        """
        self.left = True
        self.right = True
        self.top = True
        self.bottom = True
        self.color_bottom = color
        self.color_top = color
        self.color_left = color
        self.color_right = color


class Cell:
    """
    LaTeX-table cell containing all its attributes.
    """

    def __init__(
            self, index: int = -1, content: str = "",
            colspan: int = default_colspan, rowspan: int = default_rowspan,
            text_color: Union[None, str] = default_text_color, text_color_html: Union[None, bool] = False,
            bg_color: Union[None, str] = default_transparent_color, bg_color_html: Union[None, bool] = False,
            h_align=default_text_h_align, font_size: float=default_font_size,
            cell_width=default_width, cell_height=default_height,
            line_space=0, pbox="10cm", font_family=default_font_family,
            borders: CellBorders = CellBorders(), font_weight=None):
        """

        :param index: Cell index in a row.
        :param content: Cell content (text, math-symbols, etc.).
        :param colspan: How many columns the cell spans.
        :param rowspan: How many rows the cell spans.
        :param text_color: Cell text color code or name.
        :param text_color_html: Whether the text_color-attribute is a hex code.
        :param bg_color: Cell background color code or name.
        :param bg_color_html: Whether the bg_color-attribute is a hex code.
        :param h_align: Text horizontal alignment.
        :param font_size: Font size.
        :param cell_width: Width of the cell.
        :param cell_height: Heigth of the cell.
        :param line_space: Unused attribute.
        :param pbox: Length of an element that allows linebreaks in text (unused).
        :param borders: Object containing border-data.
        """
        self.index = index
        self.content = use_default_if_none(content, "")
        self.colspan = colspan
        self.rowspan = rowspan
        self.text_color = use_default_if_none(text_color, default_text_color)
        self.text_color_html = use_default_if_none(text_color_html, False)
        self.bg_color = use_default_if_none(bg_color, default_transparent_color)
        self.bg_color_html = use_default_if_none(bg_color_html, False)
        self.h_align = use_default_if_none(h_align, default_text_h_align)
        self.cell_width = use_default_if_none(cell_width, default_width)
        self.cell_height = use_default_if_none(cell_height, default_height)
        self.font_size = use_default_if_none(font_size, default_font_size)
        if self.font_size == 0:
            self.font_size = default_font_size
        self.line_space = line_space
        self.pbox = pbox
        self.borders = borders
        self.font_family = use_default_if_none(font_family, default_font_family)
        self.font_weight = font_weight

    def __str__(self) -> str:
        """
        :return: The cell's complete LaTeX format.
        """
        # LaTeX has text-h-align and cell-v-borders in the same place:
        v_border_and_align = ""
        if self.borders.left:
            l_b_color, l_b_html = self.borders.color_left
            if not l_b_color == default_transparent_color:
                v_border_and_align += fr"!{{\color{format_color(l_b_color, l_b_html)}\vrule}}"
        v_border_and_align += self.h_align
        if self.borders.right:
            r_b_color, r_b_html = self.borders.color_right
            if not r_b_color == default_transparent_color:
                v_border_and_align += fr"!{{\color{format_color(r_b_color, r_b_html)}\vrule}}"

        # HTML-colors have an extra tag
        cell_color = format_color(self.bg_color, self.bg_color_html)
        if self.bg_color == default_transparent_color or not self.bg_color:
            cell_color = ""
        else:
            cell_color = fr"\cellcolor{cell_color}"

        content = self.content
        # Minipage doesn't handle cell width always properly even if it's given,
        # and when using auto like \linewidth it's blind to what other cells are doing.
        if "\[" in content:
            minipage_width = self.cell_width
            if "*" in str(minipage_width):
                minipage_width = default_minipage_width
            content = fr"\begin{{minipage}}{{{minipage_width}}}{content}\end{{minipage}}"

        # Font weight may be bold, bolder, lighter or number from 100 to 900.
        if self.font_weight:
            if "bold" in str(self.font_weight):
                content = rf"\textbf{{{content}}}"
            else:
                try:
                    if int(self.font_weight) > 699:
                        content = rf"\textbf{{{content}}}"
                except:
                    pass

        cell_width = self.cell_width
        if "*" not in str(cell_width):
            cell_width = f"{cell_width}pt"

        font_family_line = ""
        font_family_line_postfix = ""
        if self.font_family != default_font_family:
            font_family_line = fr"\fontfamily{{{self.font_family}}}\selectfont{{"
            font_family_line_postfix = f"}}"
        return fr"\multicolumn{{{self.colspan}}}{{{v_border_and_align}}}{{" \
               fr"\multirow{{{self.rowspan}}}{{{cell_width}}}{{" \
               f"{cell_color}" \
               fr"\fontsize{{{self.font_size}}}{{{self.line_space}}}" \
               fr"\selectfont{{\textcolor{{{self.text_color}}}{{{{{font_family_line}" \
               fr"{content}}}}}}}}}}}{font_family_line_postfix}"

    def __repr__(self) -> str:
        return custom_repr(self)


def use_default_if_none(value, default):
    """
    Checks whether the value is None and uses default if it is.
    :param value: Value to check.
    :param default: Default to use if value is None.
    :return: Value without changes or default, if value was None.
    """
    if value is None:
        return default
    else:
        return value


def format_color(color: str, html_color: bool) -> str:
    """
    Converts color to LaTeX-format depending on whether it's
    html or normal color.
    :param color: Color name or hex-code.
    :param html_color: Whether the color is in hex or not.
    :return: Just the color name, or HTML-option and hex code.
    """
    if html_color:
        return f"[HTML]{{{color}}}"
    else:
        return f"{{{color}}}"


class Row:
    """
    LaTeX-table row.
    """

    def __init__(self, index: int, cells: List[Cell], height: Union[float, None] = None) -> None:
        """
        :param index: Row index.
        :param cells: A list of the cells this row contains.
        """
        self.index = index
        self.cells = cells
        self.height = height

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

    def get_row_height(self) -> Union[int, float]:
        """
        Gives the largest cell height to be used as row height or row's height attribute, if it is taller.
        Note: sseparate cell heights aren't supported.
        :return: Row height.
        """
        height = 0
        try:
            for i in range(0, len(self.cells)):
                height = max(float(height), float(self.cells[i].cell_height))
        except:
            pass
        if self.height and self.height > height:
            return self.height
        return height

    def add_cell(self, i: int, cell: Cell) -> None:
        """
        Adds a cell to index in row and uses the first free index.
        :param i: Row index of the cell to add.
        :param cell: The Cell-object to add to the row.
        :return: None.
        """
        # index:rowspan in html/json-format:
        # 0:1  1:2  2:1  3:1  4:2
        # 0:1       1:1  2:1
        #
        # index:rowspan in LaTeX-format:
        # 0:1  1:1  2:1  3:1  4:1
        # 0:1  1:-2 2:1  3:1  4:-2

        # Sorting the cells by index is necessary.
        self.cells = sorted(self.cells, key=lambda c: c.index)

        for j in range(i, len(self.cells)):
            if i == self.cells[j].index:
                i = i + 1
        cell.index = i
        self.cells.append(cell)

    def get_colspan(self) -> int:
        """
        Get the sum of row's cells' colpans.
        :return: Sum of cell's colspans.
        """
        i = 0
        for cell in self.cells:
            i += cell.colspan
        return i

    def get_cell(self, index: int) -> Union[Cell, None]:
        """
        Gives cell with the index number (which may be different from list index).
        :param index: Cell index number in the table.
        :return: Cell or None, if not found.
        """
        for cell in self.cells:
            if cell.index == index:
                return cell
        return None

    def __repr__(self):
        return custom_repr(self)


class HorizontalBorder:
    """
    Horizontal line between rows.
    """

    def __init__(self, row_above: Row = None, row_below: Row = None) -> None:
        """
        In LaTeX there can't be duplicate h-lines, so the line needs to be a
        composite of all cell-borders from rows above and below.
        :param row_above: The row above the line.
        :param row_below: The row below the line.
        """
        self.row_above = row_above
        self.row_below = row_below

    def __str__(self) -> str:
        """
        Draws line "-" between cells if there's a border coming from above or below,
        otherwise no line "~".
        :return: LaTeX \hhline with colored lines.
        """
        output = ""

        # TODO: Use cells to determine borders.
        # More specifically, this doesn't know the source cell of the border, and therefore may draw border
        # into wrong places when there's colspan.

        # Get cell-count from the rows.
        try:
            above_count = self.row_above.get_colspan()
        # If there's no row:
        except AttributeError:
            above_count = 0
        try:
            below_count = self.row_below.get_colspan()
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
                except (IndexError, AttributeError):
                    pass
                try:
                    color_above = self.row_above.cells[index_upper].borders.color_bottom
                    if color_above == default_transparent_color:
                        color_above = None
                except (IndexError, AttributeError):
                    pass
            if self.row_below:
                try:
                    i_lower = self.row_below.cells[index_lower].borders.top
                    i_lower_colspan = self.row_below.cells[index_lower].colspan
                    # lower_cont = self.row_below.cells[index_lower].content
                except (IndexError, AttributeError):
                    pass
                try:
                    color_below = self.row_below.cells[index_lower].borders.color_top
                    if color_below == default_transparent_color:
                        color_below = None
                except (IndexError, AttributeError):
                    pass

            colspan = max(i_upper_colspan, i_lower_colspan)

            # Draws the line only if either cell above or below wants one.
            if not i_upper and not i_lower:
                output += colspan * "~"
            else:
                # If border color of above cell is default or there's no row above,
                # use color from below cell.
                if not color_above and color_below or \
                        color_above[0] == default_transparent_color and color_below:
                    color, html_color = color_below
                # Otherwise default to top color.
                else:
                    color, html_color = color_above
                # If no color, don't draw the line.
                if color == default_transparent_color or not color:
                    output += colspan * "~"
                # Multicolumn cell counts as one cell but require multiple borderlines.
                else:
                    output += colspan * fr">{{\arrayrulecolor{format_color(color, html_color)}}}-"
            colspan_counter_lower = colspan_counter_lower + i_upper_colspan - 1
            colspan_counter_upper = colspan_counter_upper + i_lower_colspan - 1
        return output

    def __repr__(self) -> str:
        return custom_repr(self)


def estimate_cell_width(cell):
    """
    Give estimation of cell width based on content and font size.
    :param cell: Cell to estimate.
    :return: Width of cell.
    """
    try:
        return len(cell.content) * float(cell.font_size) / 2
    except (TypeError, ValueError):
        # If for some reason font size is not a number, use default.
        return len(cell.content) * default_font_size / 2


def estimate_cell_height(cell, width_constraint):
    """
    Gives estimation of cell height with width as constraint.
    :param cell: Cell to estimate.
    :param width_constraint: The width is locked before estimating height.
    :return: Height of cell.
    """
    # The formula is just guesswork.
    if width_constraint == 0:
        width_constraint = 1
    return (cell.font_size + 70) * len(cell.content) / width_constraint


def estimate_table_width(self) -> Tuple[float, bool]:
    """
    Get total width of the table (i.e. width of longest row).
    :return: Width.
    """
    width = 0
    estimate = False
    for row in self.rows:
        for cell in row.cells:
            try:
                cell_width = float(parse_size_attribute(cell.cell_width))
            except:
                estimate = True
                cell_width = estimate_cell_width(cell)
            else:
                if is_close(0, cell_width):
                    estimate = True
                    cell_width = estimate_cell_width(cell)
            width += cell_width
    return width, estimate


def estimate_col_widths(rows):
    """
    Takes the most large set width of the column's cells,
    or estimation of their needed content size, if all are automatic.
    :param rows: Table rows.
    :return: Estimation of column widths.
    """
    # TODO: Take colspan into account.
    widths = []
    for i in range(0, len(rows)):
        i_widths = []
        max_content_size = 0
        for j in range(0, default_max_col_count):
            try:
                cell = rows[j].cells[i]
            except IndexError:
                break
            else:
                # Cells with lots of content are expected to be divided on separate lines.
                # Magic number factors are there to balance things out.
                content_size = estimate_cell_width(cell) * 0.8
                if len(cell.content) > wrap_cell_threshold:
                    content_size = content_size * 0.2
                width = cell.cell_width
                if content_size > max_content_size:
                    max_content_size = content_size
                if width != default_width:
                    i_widths.append(width)

        if i_widths:
            widths.append(max(i_widths))
        else:
            widths.append(max_content_size)
    return widths


class Table:
    """
    Table with rows, cells in rows, and horizontal borders between rows.
    """

    def __init__(self, rows: List[Row], width=default_table_width,
                 height=default_table_height, fit_to_page_width: bool = False) -> None:
        """
        :param rows: List of the rows of the table.
        :param width: Table width.
        :param height: Table height.
        """
        self.rows = rows

        self.width = width
        self.height = height
        self.hborders = []
        self.col_count = None
        self.largest_content_len = None
        self.largest_col_count = None
        self.fit_to_page_width = fit_to_page_width

    def __str__(self) -> str:
        """
        :return: The complete table in LaTeX-format.
        """
        if not self.rows:
            return ""
        # 'c' would be text horizontal alignment, but it's actually set elsewhere,
        # so here it tells only the highest amount of cols in the table.
        # Always uses max amount because this doesn't cause problems and avoids errors caused by having too few.
        columns = "c" * default_max_col_count
        prefix = f"\\begin{{table}}{default_table_float}\n" \
                 "\centering\n" \
                 f"\\begin{{tabular}}{{{columns}}}"
        postfix = "\\end{tabular}\n\\end{table}"
        resize = self.fit_to_page_width
        if resize:
            prefix = f"\\begin{{table}}{default_table_float}\n" \
                     f"\\resizebox{{{self.width}}}{{{self.height}}}{{%\n" \
                     f"\\begin{{tabular}}{{{columns}}}"
            postfix = "\\end{tabular}%\n}\n\\end{table}"
        output = ""
        for i in range(0, len(self.rows)):
            output += "\n" + fr"\hhline{{{str(self.hborders[i])}}}" \
                             "\n" + f"{str(self.rows[i])}" \
                                    "\n" + fr"\tabularnewline[{self.rows[i].get_row_height()}pt]"

        output += "\n" + fr"\hhline{{{str(self.hborders[-1])}}}"
        return f"{prefix}\n{output}\n{postfix}"

    def get_or_create_row(self, i: int) -> Row:
        """
        Returns the row in index or creates a new one with said index.
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
        :return: None.
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

    def auto_size_cells(self) -> None:
        """
        Try to set row heights automatically based on cell content length.
        :return: None.
        """
        # If table has only few cells and little content, don't resize.
        if self.get_largest_content_len() <= wrap_cell_threshold and \
                self.get_largest_col_count() <= resizing_col_threshold:
            return

        # TODO: A word which isn't broken by spaces and is considerably longer than title
        # row may overflow from the cell boundaries.
        if not self.rows:
            return
        try:
            widths = estimate_col_widths(self.rows)
            # # Alternative way to do this, based on the first row.
            # for cell in self.rows[0].cells:
            #     cell_width = cell.cell_width
            #     if cell_width is default_width:
            #         cell_width = estimate_cell_width(cell)
            #     widths.append(cell_width)
            for i in range(0, len(self.rows)):
                max_height = 0  # Tallest estimated height in the cells of the row.
                for j in range(0, len(self.rows[i].cells)):
                    cell = self.rows[i].cells[j]
                    # Don't change width of the first row (because it is in most cases titles).
                    try:
                        if i != 0:
                            cell.cell_width = widths[j]
                        height = estimate_cell_height(cell, widths[j])
                    except IndexError:
                        height = estimate_cell_height(cell, estimate_cell_width(cell))
                    # Row height will be decided by the tallest cell.
                    if height > max_height:
                        max_height = height
                if not is_close(float(max_height), float(default_height)):
                    # Column widths are based on first row so it's more exact and needs less buffer space.
                    if i == 0:
                        max_height = max_height * 0.3
                    # Row height is later on compared with cell heights and the tallest is chosen,
                    # so this is effectively the minimal height.
                    self.rows[i].height = max_height
        except:
            # If auto-sizing fails, skip it.
            return

    def save_largest(self) -> None:
        """
        Add largest row column count and cell content length to table attributes.
        :return: None.
        """
        table_max_col_count = 0
        table_max_content_len = 0
        for row in self.rows:
            row_col_count = 0
            row_max_content_len = 0
            for cell in row.cells:
                row_col_count += cell.colspan
                cell_content_len = len(cell.content)
                if cell_content_len > row_max_content_len:
                    row_max_content_len = cell_content_len
            if row_col_count > table_max_col_count:
                table_max_col_count = row_col_count
            if row_max_content_len > table_max_content_len:
                table_max_content_len = row_max_content_len
        self.largest_content_len = table_max_content_len
        self.largest_col_count = table_max_col_count

    def get_largest_content_len(self) -> int:
        """
        Get largest content length in the table.
        :return: Content length of the longest cell.
        """
        if self.largest_content_len:
            return self.largest_content_len
        else:
            self.save_largest()
            return self.largest_content_len

    def get_largest_col_count(self) -> int:
        """
        Get largest total row colspan in the table.
        :return: Colspan of the widest row.
        """
        if self.largest_col_count:
            return self.largest_col_count
        else:
            self.save_largest()
            return self.largest_col_count


def get_column_span(item):
    """
    Get column span value.
    :param item: Column data.
    :return: Span.
    """
    try:
        return int(item["span"])
    except:
        return 1


def get_column_color_list(key, table_data):
    """
    Reads all the columns of the table and makes a list of their color formattings.
    :param key: Key for color type.
    :param table_data: Table JSON.
    :return: List of column colors.
    """
    l = []
    try:
        columns_data = table_data['columns']
    except:
        # Add empty entries as a quick fix for index out of bounds error.
        return [(None, None)] * default_max_col_count
    for i in range(0, len(columns_data)):
        span = get_column_span(columns_data[i])
        for j in range(0, span):
            l.append(get_color(columns_data[i], key))
    for k in range(0, default_max_col_count):
        l.append((None, None))
    return l


def get_column_width_list(table_data):
    """
    Forms a list of column widths from the columns data.
    :param table_data: Table JSON.
    :return: List of column widths.
    """
    l = []
    try:
        try:
            columns_data = table_data['columnstex']
        except:
            columns_data = table_data['columns']
    except:
        return [None] * default_max_col_count
    for i in range(0, len(columns_data)):
        span = get_column_span(columns_data[i])
        for j in range(0, span):
            try:
                l.append(get_size(columns_data[i], "width"))
            except:
                l.append(None)
    for k in range(0, default_max_col_count):
        l.append(None)
    return l


def get_column_style_list(table_data, key):
    """
    Forms a list of styles corresponding to the key from the columns data.
    :param table_data: Table JSON.
    :param key: Style key.
    :return: List of column styles.
    """
    l = []
    try:
        columns_data = table_data['columns']
    except:
        return [None] * default_max_col_count
    for i in range(0, len(columns_data)):
        column_data = columns_data[i]
        span = get_column_span(column_data)
        for j in range(0, span):
            try:
                l.append(column_data[key])
            except:
                l.append(None)
    for k in range(0, default_max_col_count):
        l.append(None)
    return l


def get_column_format_list(table_data, f: Callable[[Dict, Any], Any]):
    """
    Forms a list of font families from the columns data.
    :param table_data:
    :param f: Function to get the format values from the column data.
    :return: List of column formats.
    """
    l = []
    try:
        columns_data = table_data['columns']
    except:
        return [None] * default_max_col_count
    for i in range(0, len(columns_data)):
        column_data = columns_data[i]
        span = get_column_span(column_data)
        for j in range(0, span):
            try:
                l.append(f(column_data, None))
            except:
                l.append(None)
    for k in range(0, default_max_col_count):
        l.append(None)
    return l


def custom_repr(obj) -> str:
    """
    Extended repr that displays all contents of the object.
    :param obj: The object to repr.
    :return: Full contents of the object and the objects it references.
    """
    return f"{str(obj.__class__)}: {str(obj.__dict__)}"


def get_content(cell_data) -> str:
    """
    Gets content from a cell.
    :param cell_data: Cell JSON.
    :return: Cell content.
    """
    try:
        return str(cell_data['cell']).strip()
    # Cells that use simplified format (without 'cell').
    except TypeError:
        return str(cell_data)


def get_color(item, key: str, default_color=None, default_color_html=None) -> (str, bool):
    """
    Parses color-data into LaTeX-format.
    :param item:
    :param key: Key for color element (color, backgroundColor, etc.).
    :param default_color: Color to use if key not found.
    :param default_color_html: Whether color is in hex or not.
    :return: Tuple with the color-code / name and whether its in hex or not.
    """
    # Normal LaTex doesn't recognize some html colors,
    # so they need to be defined in the tex file (colors.txt in timApp/static/tex).
    color = default_color
    color_html = default_color_html
    try:
        color = item[key]
        if "#" in color:
            color_html = True
            color = parse_hex_color(color)
        else:
            color_html = False
    except KeyError:
        pass
    finally:
        return color, color_html


def get_datablock_cell_data(datablock, row: int, cell: int):
    """
    Returns data from datablock index.
    :param datablock: Datablock JSON.
    :param row: Row index.
    :param cell: Cell index.
    :return: Datablock data for a cell, if it exists.
    """
    if not datablock:
        return None
    try:
        datablock_index = f"{int_to_datablock_index(cell)}{row+1}"
        return datablock[datablock_index]
    except:
        return None


def convert_datablock_index(datablock_index) -> Tuple[int, int]:
    """
    A 1 -> 0, 0
    ZZ13 -> 51, 12
    :param datablock_index: Index in format "A1".
    :return: Integer tuple (cell_index, row_index).
    """
    letters = re.sub(r'[0-9]+', '', datablock_index)
    numbers = re.sub(r'[A-Z]+', '', datablock_index)
    cell = (ord(letters[0]) - ord("A")) + (ord("Z") - ord("A") + 1) * (len(letters) - 1)
    row = int(numbers) - 1
    return cell, row


def add_missing_elements(table_json, datablock):
    """
    Add cells and rows only present in datablock.
    :param table_json: Table data.
    :param datablock: Datablock data.
    :return: table_json with datablock-only cells added.
    """
    max_row_count = 0
    max_cell_count = 0

    if not datablock:
        return table_json

    for item in datablock:
        cell_index, row_index = convert_datablock_index(item)
        row_count = row_index + 1
        cell_count = cell_index + 1
        if row_count > max_row_count:
            max_row_count = row_count
        if cell_count > max_cell_count:
            max_cell_count = cell_count

    empty_cell = {'cell': ''}

    # TODO: If table has only datablocks, crashes here.
    try:
        table_row_count = len(table_json['rows'])
    except:
        table_row_count = 0
        table_json['rows'] = []
    # Add missing rows.
    for i in range(0, max_row_count - table_row_count):
        table_json['rows'].append({'row': [empty_cell]})

    # Add missing cells to existing rows.
    for i in range(0, len(table_json['rows'])):
        row_json = table_json['rows'][i]
        row_cell_count = len(row_json['row'])
        for j in range(row_cell_count, max_cell_count):
            row_json['row'].append(empty_cell)
    return table_json


def get_span(item, default=None) -> (int, int):
    """
    Parses row and column span of the cell.
    If not specified, assume it's 1.
    :param item: Cell data.
    :param default: Default used when not found.
    :return: Colspan and rowspan in a tuple.
    """
    try:
        colspan = item['colspan']
    except:
        colspan = default
    try:
        rowspan = item['rowspan']
    except:
        rowspan = default
    return colspan, rowspan


def get_size(item, key: str, default=None) -> Union[str, None]:
    """
    Parse width or height into LaTeX-supported format.
    :param item: Cell data.
    :param key: Width or heigth.
    :param default: Value to be used if key wasn't found.
    :return: Cell width or height.
    """
    try:
        size = parse_size_attribute(item[key])
        if size == 'auto':
            return None
        return size
    except:
        return default


def get_font_family(item, default: Union[str, None] = default_font_family) -> Union[str, None]:
    """
    :param item: Cell or row data.
    :param default: Font family to use in case none set.
    :return: Set font family or default.
    """
    try:
        # Corresponding HTML and LaTeX codes need to be mapped in the 'fonts'.
        ff = item['fontFamily'].lower()
        font = fonts[ff]
    except:
        font = default
    return font


def get_text_horizontal_align(item, default):
    """
    Parses text horizontal alignment.
    :param item: Table, row or cell data.
    :param default: Value to be used if no set align.
    :return: Set align or default.
    """
    try:
        # Options are center, right and left, which happen to be the same in LaTeX,
        # except only first letters are used.
        a = str(item['textAlign']).strip()[:1]
    except:
        a = default
    return a


def get_font_size(item, default_size):
    """
    Gets text size if set, and uses default otherwise.
    :param item: Cell data item.
    :param default_size: Size to be used if no set font size.
    :return: Font size or default font size.
    """
    return get_size(item, 'fontSize', default_size)


def get_key_value(item, key, default=None):
    """
    Returns a value from dictionary or default if key doesn't exist.
    :param item: Dictionary (JSON).
    :param default: Value that's used in case key cannot be found.
    :param key: Key.
    :return: Value or default.
    """
    try:
        a = item[key]
    except:
        a = default
    return a


def parse_hex_color(color, default_color=None) -> Union[str, None]:
    """
    Removes non-hex characters and checks if result is valid.
    :param color: Color string.
    :param default_color: Color returned in case valid hex can't be parsed.
    :return: A hex color code of six characters.
    """
    color = re.sub('[^a-fA-F0-9]+', '', color)
    if len(color) != 6:
        color = default_color
    return color


def get_border_color(border_data) -> Tuple[str, bool]:
    """
    Parses border color from HTML border format.
    :param border_data: HTML border format with line thickness, style, color.
    :return: Border color as tuple containing color name/code and whether its a hex.
    """
    arg_count = border_data.count(" ")
    color = default_text_color
    color_html = False
    if arg_count == 2:
        color = border_data[border_data.rfind(" "):].strip()
        # Hex codes don't work in TimTable.
        # if "#" in color:
        #     color_html = True
        #     color = parse_hex_color(color, default_text_color)
    return color, color_html


def get_borders(item, default_borders=CellBorders()) -> CellBorders:
    """
    Creates a CellBorder object with corresponding border-data.
    :param item: Cell or row data.
    :param default_borders: Borders to be used in case none found.
    :return: CellBorders object for the item.
    """
    try:
        border_data = item['border']
        if border_data:
            borders = CellBorders()
            borders.set_all_borders(get_border_color(border_data))
            return borders
    except:
        borders = copy.copy(default_borders)
        try:
            border_data = item['borderLeft']
            if border_data:
                borders.left = True
                (color, color_html) = get_border_color(border_data)
                borders.color_left = color, color_html
        except:
            pass
        try:
            border_data = item['borderRight']
            if border_data:
                borders.right = True
                (color, color_html) = get_border_color(border_data)
                borders.color_right = color, color_html
        except:
            pass
        try:
            border_data = item['borderTop']
            if border_data:
                borders.top = True
                (color, color_html) = get_border_color(border_data)
                borders.color_top = color, color_html
        except:
            pass
        try:
            border_data = item['borderBottom']
            if border_data:
                borders.bottom = True
                (color, color_html) = get_border_color(border_data)
                borders.color_bottom = color, color_html
        except:
            pass
        return borders


def copy_cell(cell: Cell) -> Cell:
    """
    Properly copies Cell-object and the objects within.
    :param cell: Cell object to copy.
    :return: Copy with new CellBorders-object pointers.
    """
    n_cell = copy.copy(cell)
    n_cell.borders = copy.copy(cell.borders)
    return n_cell


def get_datablock(table_json):
    """
    Looks for and returns datablock or None, if table has no tabledatablock element.
    :param table_json: Table data as json string.
    :return: Datablock or None.
    """
    try:
        datablock = table_json['tabledatablock']['cells']
    except KeyError:
        datablock = None
    return datablock


def int_to_datablock_index(i: int) -> str:
    """
    Converts an index integer to corresponding capital letter(s).
    For example: 0 -> A, 25 -> Z, 26 -> AA, 27 -> BB.
    :param i: Index starting from 0.
    :return: The index in capital letter format.
    """
    if i < 0:
        raise IndexConversionError(i)
    a, b = divmod(i, ord("Z") - ord("A") + 1)
    return (a + 1) * str(chr(ord("A") + b))


def parse_size_attribute(attribute: str) -> str:
    """
    Converts numeric attributes to pts and removes the unit sign.
    :param attribute: Size attribute.
    :return: Parsed string.
    """
    # All units are converted to pt.
    conv_to_pt = {'mm': 2.83464566929,
                  'cm': 28.3464566929,
                  'in': 72,
                  'px': 1.33333333333,
                  'pt': 1,
                  'pc': 12,
                  'ex': 4.30554,
                  'em': 10.00002}
    if not attribute:
        return "0"
    for key, value in conv_to_pt.items():
        if key in attribute:
            try:
                return str(round(float(str(attribute).replace(key, '').strip())*value, 2))
            except Exception as e:
                # TODO: Tell user about conversion errors.
                pass
    # If not recognized, return zero.
    return "0"


def get_table_size(table_data):
    """
    Sets table size attributes and uses default values if not found.
    :param table_data: Table data dictionary.
    :return: Table width and height as a tuple.
    """
    try:
        width = parse_size_attribute(table_data['width'])
    except KeyError:
        width = default_table_width
    try:
        height = parse_size_attribute(table_data['height'])
    except KeyError:
        height = default_table_height
    return width, height


def get_table_resize(table_data, table_width_estimation, col_count) -> bool:
    """
    Whether table should be resized to fit the page width.
    If the attribute isn't set, automatically decide whether to resize.
    :param table_data: Table JSON.
    :param table_width_estimation: Table width and whether it's an estimation as tuple.
    :param col_count Max number of columns (including colspans) in the table.
    :return: Table scaling true or false.
    """
    table_width = table_width_estimation[0]
    estimate = table_width_estimation[1]
    resize = False
    # If forced.
    try:
        resize = table_data['texFitToPage']
    # Otherwise check automatically.
    except:
        # If table size is estimation and col count is low, don't resize
        # (because width estimation isn't reliable currently).
        if estimate and col_count <= resizing_col_threshold:
            return False
        if table_width >= resizing_px_threshold:
            return True
    return resize


def decide_format_size(format_levels):
    """
    Decides which size (column, row, cell, datablock) to use by taking the longest one.
    :param format_levels: Table, column, row, cell, datablock.
    :return: Largest size.
    """
    final_size = 0
    for level in format_levels:
        if level and default_width not in level:
            try:
                size = float(level)
                if size > final_size:
                    final_size = size
            except ValueError:
                continue
    if is_close(final_size, 0):
        return default_width
    else:
        return final_size


def decide_format_tuple(format_levels):
    """
    Goes through a list of formats and returns the last non-empty one.
    The idea is to stack table, column, row and cell formats and take the
    topmost format.
    :param format_levels: Table, column, row, cell, datablock.
    :return: Last non-empty format.
    """
    final_format = (None, None)
    for level in format_levels:
        if level[0]:
            final_format = level
    return final_format


def decide_format(format_levels):
    """
    Decides which format to use by taking the latest non-empty one.
    :param format_levels: Table, column, row, cell, datablock.
    :return: Last non-empty value or None if all are empty.
    """
    final_format = None
    for level in format_levels:
        if level:
            final_format = level
    return final_format


def is_close(a, b, rel_tol=1e-09, abs_tol=0.0) -> bool:
    """
    Compares floats and returns true if they are almost same.
    Source: https://stackoverflow.com/questions/5595425/
    what-is-the-best-way-to-compare-floats-for-almost-equality-in-python
    :param a: Number a.
    :param b: Number b.
    :param rel_tol: Relative tolerance.
    :param abs_tol: Absolute tolerance.
    :return: True if floats are very close to each other.
    """
    return abs(a - b) <= max(rel_tol * max(abs(a), abs(b)), abs_tol)


def decide_colspan_rowspan(cell_colspan, cell_rowspan, datablock_colspan, datablock_rowspan):
    colspan = cell_colspan
    rowspan = cell_rowspan
    if datablock_colspan:
        colspan = datablock_colspan
    if not colspan:
        colspan = default_colspan
    if datablock_rowspan:
        rowspan = datablock_rowspan
    if not rowspan:
        rowspan = default_rowspan
    return colspan, rowspan


def convert_table(table_json, draw_html_borders: bool = False) -> Table:
    """
    Converts TimTable-json into LaTeX-compatible object.
    Note: for correct functioning all the other modules should use this.
    :param table_json: Table data as json dictionary with
        'rows', 'tabledatablock', etc. at the first level.
    :param draw_html_borders Add light gray default borders around cells similarly to HTML-table.
    :return: Table-object containing the rows and cells in LaTex.
    """
    table_rows = []
    table = Table(table_rows)
    datablock = get_datablock(table_json)
    table_json = add_missing_elements(table_json, datablock)
    # TODO: Make the table size work with correct logic.
    # These may stretch the table until unreadable or outside the page.
    # Also, even if the same value is set horizontally and vertically,
    # it may not be a square like it should be.
    # HTML sets cell widths to match the table width, but this currently
    # just stretches the table.

    # Commented off, because doesn't work the intended way.
    # (table_width, table_height) = get_table_size(table_json)
    # table.width = table_width
    # table.height = table_height

    # Table settings will be used as defaults, if set.
    # Each level's format is saved in a variable, which will be empty, if
    # that level doesn't have any formattings.
    (table_bg_color, table_bg_color_html) = \
        get_color(table_json,
                  "backgroundColor",
                  default_transparent_color,
                  False)
    (table_text_color, table_text_color_html) = \
        get_color(table_json,
                  "color",
                  default_text_color,
                  False)
    table_font_family = get_font_family(table_json, default_font_family)
    table_borders = get_borders(table_json, CellBorders())
    table_font_size = get_font_size(table_json, default_font_size)
    table_h_align = get_text_horizontal_align(table_json, default_text_h_align)
    table_font_weight = get_key_value(table_json, "fontWeight", None)

    # Add light borders around every cell like in HTML-table.
    if draw_html_borders:
        table_borders = CellBorders()
        table_borders.set_all_borders(("lightgray", False))

    # Get column formattings:
    column_bg_color_list = get_column_color_list("backgroundColor", table_json)
    column_text_color_list = get_column_color_list("color", table_json)
    column_width_list = get_column_width_list(table_json)
    column_font_size_list = get_column_style_list(table_json, "fontSize")
    column_h_align_list = get_column_format_list(table_json, f=get_text_horizontal_align)
    column_font_family_list = get_column_format_list(table_json, f=get_font_family)

    # Get default (applied to all of same type) attributes:
    table_default_row_data = table_json.get('defrows')
    table_default_col_data = table_json.get('defcols')
    table_default_cell_data = table_json.get('defcells')

    table_default_row_height = get_size(table_default_row_data, key="height", default=None)
    table_default_col_height = get_size(table_default_col_data, key="width", default=None)
    table_default_cell_bgcolor = get_color(table_default_cell_data, key="backgroundColor")
    table_default_cell_textcolor = get_color(table_default_cell_data, key="color")

    table_json_rows = table_json.get('rows')
    if not table_json_rows:
        return
    for i in range(0, len(table_json_rows)):
        table_row = table.get_or_create_row(i)
        row_data = table_json_rows[i]

        (row_bg_color, row_bg_color_html) = get_color(row_data, "backgroundColor")
        (row_text_color, row_text_color_html) = get_color(row_data, "color")
        row_width = get_size(row_data, key="width", default=None)
        row_height = get_size(row_data, key="height", default=None)
        row_font_size = get_font_size(row_data, None)
        row_h_align = get_text_horizontal_align(row_data, None)
        row_font_family = get_font_family(row_data, None)
        row_font_weight = get_key_value(row_data, "fontWeight", None)

        # TODO: Change the logic: in HTML these go around the whole row, not each cell!
        row_borders = get_borders(row_data, table_borders)

        skip_index = 0
        for j in range(0, len(table_json_rows[i]['row'])):
            # Skips following cells based on previous cell's colspan.
            if skip_index > 0:
                skip_index -= 1
                continue
            # Skip cells that have been added because of rowspan.
            if table_row.get_cell(j):
                continue

            cell_data = table_json_rows[i]['row'][j]

            content = get_content(cell_data)

            # Get cell attributes:
            (cell_bg_color, cell_bg_color_html) = get_color(cell_data, 'backgroundColor')
            (cell_text_color, cell_text_color_html) = get_color(cell_data, 'color')
            cell_height = get_size(cell_data, key="height")
            cell_width = get_size(cell_data, key="width")
            cell_h_align = get_text_horizontal_align(cell_data, None)
            cell_font_family = get_font_family(cell_data, None)
            cell_font_size = get_font_size(cell_data, None)
            cell_font_weight = get_key_value(cell_data, "fontWeight", None)
            (cell_colspan, cell_rowspan) = get_span(cell_data)
            borders = get_borders(cell_data, row_borders)

            # Get datablock formats:
            datablock_cell_data = get_datablock_cell_data(datablock, i, j)

            # Check being None instead of 'not datablock_cell_data' because need to
            # also replace when this is an empty string.
            if datablock_cell_data is None:
                pass
            else:
                content = get_content(datablock_cell_data)

            (datablock_bg_color, datablock_bg_color_html) = get_color(datablock_cell_data, 'backgroundColor')
            (datablock_text_color, datablock_text_color_html) = get_color(datablock_cell_data, 'color')
            datablock_cell_height = get_size(datablock_cell_data, key="height")
            datablock_cell_width = get_size(datablock_cell_data, key="width")
            datablock_font_family = get_font_family(datablock_cell_data, None)
            datablock_font_size = get_font_size(datablock_cell_data, None)
            datablock_h_align = get_text_horizontal_align(datablock_cell_data, None)
            datablock_font_weight = get_key_value(datablock_cell_data, "fontWeight", None)
            datablock_colspan, datablock_rowspan = get_span(datablock_cell_data)

            # Decide which styles to use (from table, column, row, cell or datablock)
            (bg_color, bg_color_html) = decide_format_tuple([
                (table_bg_color, table_bg_color_html),
                table_default_cell_bgcolor,
                column_bg_color_list[j],
                (row_bg_color, row_bg_color_html),
                (cell_bg_color, cell_bg_color_html),
                (datablock_bg_color, datablock_bg_color_html),
            ])
            (text_color, text_color_html) = decide_format_tuple([
                (table_text_color, table_text_color_html),
                table_default_cell_textcolor,
                column_text_color_list[j],
                (row_text_color, row_text_color_html),
                (cell_text_color, cell_text_color_html),
                (datablock_text_color, datablock_text_color_html),
            ])
            height = decide_format_size([
                table_default_row_height,
                row_height,
                cell_height,
                datablock_cell_height])
            width = decide_format_size([
                table_default_col_height,
                column_width_list[j],
                row_width,
                cell_width,
                datablock_cell_width])
            h_align = decide_format([
                table_h_align,
                column_h_align_list[j],
                row_h_align,
                cell_h_align,
                datablock_h_align])
            font_family = decide_format([
                table_font_family,
                column_font_family_list[j],
                row_font_family,
                cell_font_family,
                datablock_font_family])
            font_size = decide_format([
                table_font_size,
                column_font_size_list[j],
                row_font_size,
                cell_font_size,
                datablock_font_size])
            font_weight = decide_format([
                table_font_weight,
                row_font_weight,
                cell_font_weight,
                datablock_font_weight])
            colspan, rowspan = decide_colspan_rowspan(cell_colspan, cell_rowspan, datablock_colspan, datablock_rowspan)

            c = Cell(
                content=content,
                font_family=font_family,
                font_size=font_size,
                h_align=h_align,
                bg_color=bg_color,
                bg_color_html=bg_color_html,
                text_color=text_color,
                text_color_html=text_color_html,
                colspan=colspan,
                rowspan=rowspan,
                cell_width=width,
                cell_height=height,
                borders=borders,
                font_weight=font_weight
            )

            # TODO: Cells that are replaced in html by rowspan or colspan are left in some cases and
            # TODO: may break multicol and -row cells and their borders.
            # Cells with rowspan > 1:
            # Multirow-cells need to be set from bottom-up in LaTeX to
            # properly show bg-colors, and empty cells need to be placed
            # above to avoid overlap, since LaTeX doesn't automatically
            # move cells aside.
            if rowspan > 1:
                # Take multicol-cells messing up indices into account with this:
                cell_index = table_row.get_colspan()
                for y in range(0, rowspan - 1):
                    # Empty filler cell has mostly same the settings as the multirow-cell:
                    d = copy_cell(c)
                    d.content = ""
                    d.borders.color_bottom = (c.bg_color, c.bg_color_html)
                    if y > 1:
                        d.borders.color_top = (c.bg_color, c.bg_color_html)
                    d.rowspan = 1
                    table.get_or_create_row(i + y).add_cell(cell_index, d)
                c.borders.color_top = (c.bg_color, c.bg_color_html)
                c.rowspan = -rowspan
                table.get_or_create_row(i + rowspan - 1).add_cell(cell_index, c)

            # Normal cells:
            else:
                table_row.add_cell(j, c)

            skip_index = colspan - 1

    # Set row and column sizes according to cell contents.
    if get_key_value(table_json, "texAutoSize", True):
        table.auto_size_cells()
    # Whether table should be fit to page.
    table.fit_to_page_width = get_table_resize(table_json, estimate_table_width(table), table.get_largest_col_count())
    # Create horizontal border objects.
    table.create_hborders()

    return table
