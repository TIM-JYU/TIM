"""
Converts timTable-json into LaTeX.
"""
__authors__ = ["Visa Naukkarinen"]


import copy
import dataclasses
import re
from dataclasses import dataclass, field
from typing import Callable, Any, TypeVar, Literal

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
fonts = {"monospace": "pcr", "sans-serif": "cmss", "times": "ptm", "calibri": "cmss"}


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


@dataclass(slots=True)
class CellColor:
    color: str
    is_hex: bool

    @staticmethod
    def default() -> "CellColor":
        return CellColor(default_transparent_color, False)

    def to_latex_str(self) -> str:
        """
        Converts color to LaTeX-format depending on whether it's
        html or normal color.
        :return: Just the color name, or HTML-option and hex code.
        """
        if self.is_hex:
            return f"[HTML]{{{self.color}}}"
        else:
            return f"{{{self.color}}}"


@dataclass(slots=True)
class CellBorders:
    """
    Contains the attributes of a cell's borders.
    """

    left: bool = False
    """Left border existence."""
    right: bool = False
    """Right border existence."""
    top: bool = False
    """Top border existence."""
    bottom: bool = False
    """Bottom border existence."""
    color_bottom: CellColor = field(default_factory=CellColor.default)
    """The color of bottom border."""
    color_top: CellColor = field(default_factory=CellColor.default)
    """The color of top border."""
    color_left: CellColor = field(default_factory=CellColor.default)
    """The color of right border."""
    color_right: CellColor = field(default_factory=CellColor.default)
    """The color of right border."""

    def set_all_borders(self, color: CellColor) -> None:
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


@dataclass(slots=True)
class Cell:
    """
    LaTeX-table cell containing all its attributes.
    """

    index: int = -1
    """Cell index in a row."""
    content: str = ""
    """Cell content (text, math-symbols, etc.)."""
    colspan: int = default_colspan
    """How many columns the cell spans."""
    rowspan: int = default_rowspan
    """How many rows the cell spans."""
    text_color: CellColor = field(
        default_factory=lambda: CellColor(default_text_color, False)
    )
    """Cell text color code or name."""
    bg_color: CellColor = field(default_factory=CellColor.default)
    """Cell background color."""
    h_align: str = default_text_h_align
    """Text horizontal alignment."""
    cell_width: str = default_width
    """Width of the cell."""
    cell_height: str = default_height
    """Height of the cell."""
    line_space: int = 0
    """Unused attribute."""
    pbox: str = "10cm"
    """Length of an element that allows linebreaks in text (unused)."""
    borders: CellBorders = field(default_factory=CellBorders)
    """Object containing border-data."""
    font_family: str = default_font_family
    """Font family."""
    font_size: float = default_font_size
    """Font size."""
    font_weight: str | None = None
    """Font weight."""

    def __post_init__(self) -> None:
        if self.font_size == 0:
            self.font_size = default_font_size

    def estimate_width(self) -> float:
        """
        Give estimation of cell width based on content and font size.

        :return: Width of cell.
        """
        try:
            return len(self.content) * float(self.font_size) / 2
        except (TypeError, ValueError):
            # If for some reason font size is not a number, use default.
            return len(self.content) * default_font_size / 2

    def estimate_height(self, width_constraint: float | None = None) -> float:
        """
        Gives estimation of cell height with width as constraint.

        :param width_constraint: The width is locked before estimating height.
        :return: Height of cell.
        """
        # The formula is just guesswork.
        if width_constraint is None:
            width_constraint = self.estimate_width()
        if width_constraint == 0:
            width_constraint = 1
        return (self.font_size + 70) * len(self.content) / width_constraint

    def __str__(self) -> str:
        """
        :return: The cell's complete LaTeX format.
        """
        # LaTeX has text-h-align and cell-v-borders in the same place:
        v_border_and_align = ""
        if self.borders.left:
            if self.borders.color_left.color != default_transparent_color:
                v_border_and_align += (
                    rf"!{{\color{self.borders.color_left.to_latex_str()}\vrule}}"
                )
        v_border_and_align += self.h_align
        if self.borders.right:
            if self.borders.color_right.color != default_transparent_color:
                v_border_and_align += (
                    rf"!{{\color{self.borders.color_right.to_latex_str()}\vrule}}"
                )

        # HTML-colors have an extra tag
        cell_color = self.bg_color.to_latex_str() if self.bg_color else ""
        if not self.bg_color or self.bg_color.color == default_transparent_color:
            cell_color = ""
        else:
            cell_color = rf"\cellcolor{cell_color}"

        content = self.content
        # Minipage doesn't handle cell width always properly even if it's given,
        # and when using auto like \linewidth it's blind to what other cells are doing.
        if r"\[" in content:
            minipage_width = self.cell_width
            if "*" in str(minipage_width):
                minipage_width = default_minipage_width
            content = (
                rf"\begin{{minipage}}{{{minipage_width}}}{content}\end{{minipage}}"
            )

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
            font_family_line = rf"\fontfamily{{{self.font_family}}}\selectfont{{"
            font_family_line_postfix = f"}}"
        return (
            rf"\multicolumn{{{self.colspan}}}{{{v_border_and_align}}}{{"
            rf"\multirow{{{self.rowspan}}}{{{cell_width}}}{{"
            f"{cell_color}"
            rf"\fontsize{{{self.font_size}}}{{{self.line_space}}}"
            rf"\selectfont{{\textcolor{self.text_color.to_latex_str()}{{{{{font_family_line}"
            rf"{content}}}}}}}}}}}{font_family_line_postfix}"
        )


T = TypeVar("T")


def use_default_if_none(value: T | None, default: T) -> T:
    """
    Checks whether the value is None and uses default if it is.

    :param value: Value to check.
    :param default: Default to use if value is None.
    :return: Value without changes or default, if value was None.
    """
    return value if value is not None else default


@dataclass(slots=True)
class Row:
    """
    LaTeX-table row.
    """

    index: int
    cells: list[Cell]
    height: float | None = None

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

    def get_row_height(self) -> float:
        """
        Gives the largest cell height to be used as row height or row's height attribute, if it is taller.
        Note: sseparate cell heights aren't supported.
        :return: Row height.
        """
        height = 0.0
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

    def get_cell(self, index: int) -> Cell | None:
        """
        Gives cell with the index number (which may be different from list index).

        :param index: Cell index number in the table.
        :return: Cell or None, if not found.
        """
        for cell in self.cells:
            if cell.index == index:
                return cell
        return None


@dataclass(slots=True)
class HorizontalBorder:
    """
    Horizontal line between rows.
    """

    row_above: Row | None = None
    """The row above the line."""
    row_below: Row | None = None
    """The row below the line."""

    def _try_get_neighbour(
        self, index: int, check_dir: Literal["top", "bottom"]
    ) -> tuple[bool, int, CellColor | None]:
        row_to_check: Row | None = (
            self.row_above if check_dir == "top" else self.row_below
        )
        is_present = False
        colspan = 1
        color = None

        if not row_to_check:
            return is_present, colspan, color

        try:
            row_cell = row_to_check.cells[index]
            borders = row_cell.borders
            colspan = row_cell.colspan
            is_present = borders.bottom if check_dir == "top" else borders.top
            color = borders.color_bottom if check_dir == "top" else borders.color_top
            if color.color == default_transparent_color:
                color = None
        except (IndexError, AttributeError):
            pass

        return is_present, colspan, color

    def __str__(self) -> str:
        r"""
        Draws line "-" between cells if there's a border coming from above or below,
        otherwise no line "~".
        :return: LaTeX \hhline with colored lines.
        """
        output = ""

        # TODO: Use cells to determine borders.
        # More specifically, this doesn't know the source cell of the border, and therefore may draw border
        # into wrong places when there's colspan.

        # Get cell-count from the rows.
        above_count = self.row_above.get_colspan() if self.row_above else 0
        below_count = self.row_below.get_colspan() if self.row_below else 0

        max_count = max(above_count, below_count)
        if max_count <= 0:
            raise TableBorderException("Table row not found")

        colspan_counter_lower = 0
        colspan_counter_upper = 0

        for i in range(0, max_count):
            index_lower = i + colspan_counter_lower
            index_upper = i + colspan_counter_upper
            i_upper, i_upper_colspan, color_above = self._try_get_neighbour(
                index_upper, "top"
            )
            i_lower, i_lower_colspan, color_below = self._try_get_neighbour(
                index_lower, "bottom"
            )

            colspan = max(i_upper_colspan, i_lower_colspan)

            # Draws the line only if either cell above or below wants one.
            if not i_upper and not i_lower:
                output += colspan * "~"
            else:
                # If border color of above cell is default or there's no row above,
                # use color from below cell.
                if color_below and (
                    not color_above or color_above.color == default_transparent_color
                ):
                    color = color_below
                # Otherwise default to top color.
                elif color_above:
                    color = color_above
                else:
                    color = None
                # If no color, don't draw the line.
                if not color or color.color == default_transparent_color:
                    output += colspan * "~"
                # Multicolumn cell counts as one cell but require multiple borderlines.
                else:
                    output += colspan * rf">{{\arrayrulecolor{color.to_latex_str()}}}-"
            colspan_counter_lower = colspan_counter_lower + i_upper_colspan - 1
            colspan_counter_upper = colspan_counter_upper + i_lower_colspan - 1
        return output


@dataclass(slots=True)
class Table:
    """
    Table with rows, cells in rows, and horizontal borders between rows.
    """

    rows: list[Row]
    """List of the rows of the table."""
    width: str = default_table_width
    """Table width."""
    height: str = default_table_height
    """Table height."""
    fit_to_page_width: bool = False
    """Whether to fit the table to the page width."""

    hborders: list[HorizontalBorder] = field(default_factory=list)
    largest_content_len: int | None = None
    largest_col_count: int | None = None

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
        prefix = (
            f"\\begin{{table}}{default_table_float}\n"
            "\\centering\n"
            f"\\begin{{tabular}}{{{columns}}}"
        )
        postfix = "\\end{tabular}\n\\end{table}"
        resize = self.fit_to_page_width
        if resize:
            prefix = (
                f"\\begin{{table}}{default_table_float}\n"
                f"\\resizebox{{{self.width}}}{{{self.height}}}{{%\n"
                f"\\begin{{tabular}}{{{columns}}}"
            )
            postfix = "\\end{tabular}%\n}\n\\end{table}"
        output = ""
        for i in range(0, len(self.rows)):
            output += (
                "\n" + rf"\hhline{{{str(self.hborders[i])}}}"
                "\n" + f"{str(self.rows[i])}"
                "\n" + rf"\tabularnewline[{self.rows[i].get_row_height()}pt]"
            )

        output += "\n" + rf"\hhline{{{str(self.hborders[-1])}}}"
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
                HorizontalBorder(row_above=self.rows[i], row_below=self.rows[i + 1])
            )
        self.hborders.append(
            HorizontalBorder(row_above=self.rows[len(self.rows) - 1], row_below=None)
        )

    def estimate_width(self) -> tuple[float, bool]:
        """
        Get total width of the table (i.e. width of longest row).
        :return: Width.
        """
        width = 0.0
        estimate = False
        for row in self.rows:
            for cell in row.cells:
                try:
                    # TODO: Instead of this kind of num->pt hack, make cell_width typed (number with unit, auto)
                    if cell.cell_width != default_width:
                        cell_width = float(parse_size_attribute(f"{cell.cell_width}pt"))
                    else:
                        cell_width = 0
                except:
                    estimate = True
                    cell_width = cell.estimate_width()
                else:
                    if is_close(0, cell_width):
                        estimate = True
                        cell_width = cell.estimate_width()
                width += cell_width
        return width, estimate

    def estimate_col_widths(self) -> list[float]:
        """
        Takes the most large set width of the column's cells,
        or estimation of their needed content size, if all are automatic.

        :return: Estimation of column widths.
        """
        # TODO: Take colspan into account.
        widths = []
        for i in range(0, len(self.rows)):
            i_widths = []
            max_content_size = 0.0
            for j in range(0, default_max_col_count):
                try:
                    cell = self.rows[j].cells[i]
                except IndexError:
                    break
                else:
                    # Cells with lots of content are expected to be divided on separate lines.
                    # Magic number factors are there to balance things out.
                    content_size = cell.estimate_width() * 0.8
                    if len(cell.content) > wrap_cell_threshold:
                        content_size = content_size * 0.2
                    if content_size > max_content_size:
                        max_content_size = content_size
                    width = cell.cell_width
                    if width != default_width:
                        # TODO: Instead of this kind of num->pt hack, make cell_width typed (number with unit, auto)
                        i_widths.append(parse_size_attribute(f"{width}pt"))

            if i_widths:
                widths.append(max(i_widths))
            else:
                widths.append(max_content_size)
        return widths

    def auto_size_cells(self) -> None:
        """
        Try to set row heights automatically based on cell content length.

        :return: None.
        """
        # If table has only few cells and little content, don't resize.
        if (
            self.get_largest_content_len() <= wrap_cell_threshold
            and self.get_largest_col_count() <= resizing_col_threshold
        ):
            return

        # TODO: A word which isn't broken by spaces and is considerably longer than title
        # row may overflow from the cell boundaries.
        if not self.rows:
            return
        try:
            widths = self.estimate_col_widths()
            # # Alternative way to do this, based on the first row.
            # for cell in self.rows[0].cells:
            #     cell_width = cell.cell_width
            #     if cell_width is default_width:
            #         cell_width = estimate_cell_width(cell)
            #     widths.append(cell_width)
            for i in range(0, len(self.rows)):
                max_height = 0.0  # Tallest estimated height in the cells of the row.
                for j in range(0, len(self.rows[i].cells)):
                    cell = self.rows[i].cells[j]
                    # Don't change width of the first row (because it is in most cases titles).
                    try:
                        if i != 0:
                            cell.cell_width = str(widths[j])
                        height = cell.estimate_height(widths[j])
                    except IndexError:
                        height = cell.estimate_height()
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
            assert self.largest_content_len is not None
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
            assert self.largest_col_count is not None
            return self.largest_col_count


def get_column_span(item: dict[str, Any]) -> int:
    """
    Get column span value.

    :param item: Column data.
    :return: Span.
    """
    try:
        return int(item["span"])
    except:
        return 1


def get_column_color_list(
    key: str, table_data: dict[str, Any]
) -> list[CellColor | None]:
    """
    Reads all the columns of the table and makes a list of their color formattings.

    :param key: Key for color type.
    :param table_data: Table JSON.
    :return: List of column colors.
    """
    result: list[CellColor | None] = []
    try:
        columns_data = table_data["columns"]
    except:
        # Add empty entries as a quick fix for index out of bounds error.
        return [None] * default_max_col_count
    if not columns_data:
        return [None] * default_max_col_count
    for i in range(0, len(columns_data)):
        span = get_column_span(columns_data[i])
        for j in range(0, span):
            result.append(get_color(columns_data[i], key))
    for k in range(0, default_max_col_count):
        result.append(None)
    return result


def get_column_width_list(table_data: dict[str, Any]) -> list[float | None]:
    """
    Forms a list of column widths from the columns data.

    :param table_data: Table JSON.
    :return: List of column widths.
    """
    result = []
    try:
        try:
            columns_data = table_data["columnstex"]
        except:
            columns_data = table_data["columns"]
    except:
        return [None] * default_max_col_count
    if not columns_data:
        return [None] * default_max_col_count
    for i in range(0, len(columns_data)):
        span = get_column_span(columns_data[i])
        for j in range(0, span):
            try:
                result.append(get_size(columns_data[i], "width"))
            except:
                result.append(None)
    for k in range(0, default_max_col_count):
        result.append(None)
    return result


def get_column_style_list(table_data: dict[str, Any], key: str) -> list[Any | None]:
    """
    Forms a list of styles corresponding to the key from the columns data.

    :param table_data: Table JSON.
    :param key: Style key.
    :return: List of column styles.
    """
    result = []
    try:
        columns_data = table_data["columns"]
    except:
        return [None] * default_max_col_count
    if not columns_data:
        return [None] * default_max_col_count
    for i in range(0, len(columns_data)):
        column_data = columns_data[i]
        span = get_column_span(column_data)
        for j in range(0, span):
            try:
                result.append(column_data[key])
            except:
                result.append(None)
    for k in range(0, default_max_col_count):
        result.append(None)
    return result


def get_column_format_list(
    table_data: dict[str, Any], f: Callable[[dict, Any], Any]
) -> list[Any | None]:
    """
    Forms a list of font families from the columns data.

    :param table_data:
    :param f: Function to get the format values from the column data.
    :return: List of column formats.
    """
    result = []
    try:
        columns_data = table_data["columns"]
    except:
        return [None] * default_max_col_count
    if not columns_data:
        return [None] * default_max_col_count
    for i in range(0, len(columns_data)):
        column_data = columns_data[i]
        span = get_column_span(column_data)
        for j in range(0, span):
            try:
                result.append(f(column_data, None))
            except:
                result.append(None)
    for k in range(0, default_max_col_count):
        result.append(None)
    return result


def get_content(cell_data: Any) -> str:
    """
    Gets content from a cell.

    :param cell_data: Cell JSON.
    :return: Cell content.
    """
    try:
        cell = cell_data["cell"]
        if not cell:
            return ""
        return str(cell).strip()
    # Cells that use simplified format (without 'cell').
    except TypeError:
        return str(cell_data)


def get_color(
    item: dict[str, Any],
    key: str,
    default_color: CellColor | None = None,
) -> CellColor | None:
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
    color = item.get(key, None)
    if color is not None and isinstance(color, str):
        color = color.strip()
        if color.startswith("#"):
            color = parse_hex_color(color, default_transparent_color)
            if color != default_transparent_color:
                return CellColor(color, True)
        else:
            return CellColor(color, False)
    return default_color


def get_datablock_cell_data(
    datablock: dict[str, Any] | None, row: int, cell: int
) -> dict[str, Any] | str:
    """
    Returns data from datablock index.

    :param datablock: Datablock JSON.
    :param row: Row index.
    :param cell: Cell index.
    :return: Datablock data for a cell, if it exists.
    """
    from timApp.plugin.timtable.timTable import cell_coordinate

    if not datablock:
        return {}
    return datablock.get(cell_coordinate(row, cell), {})


def convert_datablock_index(datablock_index: str) -> tuple[int, int]:
    """
    A 1 -> 0, 0
    ZZ13 -> 51, 12

    :param datablock_index: Index in format "A1".
    :return: Integer tuple (cell_index, row_index).
    """
    letters = re.sub(r"[0-9]+", "", datablock_index)
    numbers = re.sub(r"[A-Z]+", "", datablock_index)
    cell = (ord(letters[0]) - ord("A")) + (ord("Z") - ord("A") + 1) * (len(letters) - 1)
    row = int(numbers) - 1
    return cell, row


def add_missing_elements(
    table_json: dict[str, Any], datablock: dict[str, Any]
) -> dict[str, Any]:
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

    empty_cell = {"cell": ""}

    # TODO: If table has only datablocks, crashes here.
    try:
        table_row_count = len(table_json["rows"])
    except:
        table_row_count = 0
        table_json["rows"] = []
    # Add missing rows.
    for i in range(0, max_row_count - table_row_count):
        table_json["rows"].append({"row": [empty_cell]})

    # Add missing cells to existing rows.
    for i in range(0, len(table_json["rows"])):
        row_json = table_json["rows"][i]
        row_cell_count = len(row_json["row"])
        for j in range(row_cell_count, max_cell_count):
            row_json["row"].append(empty_cell)
    return table_json


def get_span(
    item: dict[str, Any], default: int | None = None
) -> tuple[int | None, int | None]:
    """
    Parses row and column span of the cell.
    If not specified, assume it's 1.

    :param item: Cell data.
    :param default: Default used when not found.
    :return: Colspan and rowspan in a tuple.
    """
    colspan = item.get("colspan", default)
    rowspan = item.get("rowspan", default)
    try:
        colspan = int(colspan)
    except:
        colspan = default
    try:
        rowspan = int(rowspan)
    except:
        rowspan = default
    return colspan, rowspan


def get_size(
    item: dict[str, Any], key: str, default: float | None = None
) -> float | None:
    """
    Parse width or height into LaTeX-supported format.

    :param item: Cell data.
    :param key: Width or height.
    :param default: Value to be used if key wasn't found.
    :return: Cell width or height.
    """
    try:
        return parse_size_attribute(item.get(key, ""))
    except:
        return default


def get_font_family(
    item: dict[str, Any], default: str | None = default_font_family
) -> str | None:
    """
    :param item: Cell or row data.
    :param default: Font family to use in case none set.
    :return: Set font family or default.
    """
    ff = item.get("fontFamily", None)
    if ff is None or not isinstance(ff, str):
        return default
    return fonts.get(ff, default)


def get_text_horizontal_align(item: dict[str, Any], default: str | None) -> str | None:
    """
    Parses text horizontal alignment.

    :param item: Table, row or cell data.
    :param default: Value to be used if no set align.
    :return: Set align or default.
    """
    align = item.get("textAlign", None)
    if align is None:
        return default
    return str(align).strip()[:1]


def get_font_size(item: dict[str, Any], default_size: float | None) -> float | None:
    """
    Gets text size if set, and uses default otherwise.

    :param item: Cell data item.
    :param default_size: Size to be used if no set font size.
    :return: Font size or default font size.
    """
    return get_size(item, "fontSize", default_size)


def get_key_value(item: dict[str, Any], key: str, default: Any) -> Any:
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


def parse_hex_color(color: str, default_color: str) -> str:
    """
    Removes non-hex characters and checks if result is valid.

    :param color: Color string.
    :param default_color: Color returned in case valid hex can't be parsed.
    :return: A hex color code of six characters.
    """
    color = re.sub("[^a-fA-F0-9]+", "", color)
    if len(color) != 6:
        color = default_color
    return color


def get_border_color(border_data: str) -> CellColor:
    """
    Parses border color from HTML border format.

    :param border_data: HTML border format with line thickness, style, color.
    :return: Border color as tuple containing color name/code and whether its a hex.
    """
    arg_count = border_data.count(" ")
    color = default_text_color
    color_html = False
    if arg_count == 2:
        color = border_data[border_data.rfind(" ") :].strip()
        # Hex codes don't work in TimTable.
        # if "#" in color:
        #     color_html = True
        #     color = parse_hex_color(color, default_text_color)
    return CellColor(color, color_html)


def get_borders(
    item: dict[str, Any], default_borders: CellBorders | None = None
) -> CellBorders:
    """
    Creates a CellBorder object with corresponding border-data.

    :param item: Cell or row data.
    :param default_borders: Borders to be used in case none found.
    :return: CellBorders object for the item.
    """
    border_data = item.get("border", None)
    if border_data is not None and isinstance(border_data, str):
        borders = CellBorders()
        borders.set_all_borders(get_border_color(border_data))
        return borders

    borders = dataclasses.replace(default_borders) if default_borders else CellBorders()
    for d in ("top", "right", "bottom", "left"):
        border_data = item.get(f"border{d.capitalize()}", None)
        if border_data is not None and isinstance(border_data, str):
            setattr(borders, d, True)
            setattr(borders, f"color_{d}", get_border_color(border_data))
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


def get_datablock(table_json: dict[str, Any]) -> dict[str, Any]:
    """
    Looks for and returns datablock or None, if table has no tabledatablock element.

    :param table_json: Table data as json string.
    :return: Datablock or None.
    """
    tabledatablock = table_json.get("tabledatablock", {})
    if not isinstance(tabledatablock, dict):
        tabledatablock = {}
    datablock = tabledatablock.get("cells", None)
    return datablock if datablock is not None and isinstance(datablock, dict) else {}


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


def parse_size_attribute(attribute: Any) -> float:
    """
    Converts numeric CSS attributes to pts and removes the unit sign.

    :param attribute: Size attribute.
    :return: Parsed string.
    """
    # All units are converted to pt.
    conv_to_pt = {
        "mm": 2.83464566929,
        "cm": 28.3464566929,
        "in": 72,
        "px": 1.33333333333,
        "pt": 1,
        "pc": 12,
        "ex": 4.30554,
        "em": 10.00002,
    }
    if not attribute:
        return 0
    for key, value in conv_to_pt.items():
        if key in attribute:
            try:
                return round(float(str(attribute).replace(key, "").strip()) * value, 2)
            except:
                # TODO: Tell user about conversion errors.
                pass
    # If not recognized, return zero.
    return 0


def get_table_size(table_data: dict[str, Any]) -> tuple[str, str]:
    """
    Sets table size attributes and uses default values if not found.

    :param table_data: Table data dictionary.
    :return: Table width and height as a tuple.
    """
    try:
        width = str(parse_size_attribute(table_data["width"]))
    except KeyError:
        width = default_table_width
    try:
        height = str(parse_size_attribute(table_data["height"]))
    except KeyError:
        height = default_table_height
    return width, height


def get_table_resize(
    table_data: dict[str, Any],
    table_width_estimation: tuple[float, bool],
    col_count: int,
) -> bool:
    """
    Whether table should be resized to fit the page width.
    If the attribute isn't set, automatically decide whether to resize.

    :param table_data: Table JSON.
    :param table_width_estimation: Table width and whether it's an estimation as tuple.
    :param col_count Max number of columns (including colspans) in the table.
    :return: Table scaling true or false.
    """
    table_width, estimate = table_width_estimation
    resize = False
    # If forced.
    try:
        resize = table_data["texFitToPage"]
    # Otherwise check automatically.
    except:
        # If table size is estimation and col count is low, don't resize
        # (because width estimation isn't reliable currently).
        if estimate and col_count <= resizing_col_threshold:
            return False
        if table_width >= resizing_px_threshold:
            return True
    return resize


def decide_format_size(format_levels: list[float | None]) -> str | float:
    """
    Decides which size (column, row, cell, datablock) to use by taking the longest one.

    :param format_levels: Table, column, row, cell, datablock.
    :return: Largest size.
    """
    final_size = 0.0
    for level in format_levels:
        if level:
            size = level
            if size > final_size:
                final_size = size
    if is_close(final_size, 0):
        return default_width
    else:
        return final_size


def decide_format_colors(format_levels: list[CellColor | None]) -> CellColor:
    """
    Goes through a list of color formats and returns the last non-empty one.
    The idea is to stack table, column, row and cell formats and take the
    topmost format.

    :param format_levels: Possible values for cell colors.
    :return: Last non-empty format.
    """
    final_color = None
    for level in format_levels:
        if level:
            final_color = level
    return final_color or CellColor.default()


FT = TypeVar("FT")


def decide_format(format_levels: list[FT]) -> FT | None:
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


def is_close(a: float, b: float, rel_tol: float = 1e-09, abs_tol: float = 0.0) -> bool:
    """
    Compares floats and returns true if they are almost same.
    Source: https://stackoverflow.com/questions/5595425/what-is-the-best-way-to-compare-floats-for-almost-equality-in-python

    :param a: Number a.
    :param b: Number b.
    :param rel_tol: Relative tolerance.
    :param abs_tol: Absolute tolerance.
    :return: True if floats are very close to each other.
    """
    return abs(a - b) <= max(rel_tol * max(abs(a), abs(b)), abs_tol)


@dataclass(slots=True)
class StyleOptions:
    bg_color: CellColor | None
    text_color: CellColor | None
    width: float | None
    height: float | None
    font_size: float | None
    h_align: str | None
    font_family: str | None
    font_weight: str | None
    span: tuple[int | None, int | None]
    borders: CellBorders

    @staticmethod
    def from_dict(d: dict[str, Any], default_borders: CellBorders) -> "StyleOptions":
        bg_color = get_color(d, "backgroundColor")
        text_color = get_color(d, "color")
        width = get_size(d, "width", None)
        height = get_size(d, "height", None)
        font_size = get_font_size(d, None)
        h_align = get_text_horizontal_align(d, None)
        font_family = get_font_family(d, None)
        font_weight = get_key_value(d, "fontWeight", None)
        span = get_span(d)
        borders = get_borders(d, default_borders)

        return StyleOptions(
            bg_color,
            text_color,
            width,
            height,
            font_size,
            h_align,
            font_family,
            font_weight,
            span,
            borders,
        )


def decide_colspan_rowspan(options: list[StyleOptions]) -> tuple[int, int]:
    colspan_final = default_colspan
    rowspan_final = default_rowspan
    for o in options:
        colspan, rowspan = o.span
        if colspan:
            colspan_final = colspan
        if rowspan:
            rowspan_final = rowspan
    return colspan_final, rowspan_final


def convert_table(
    table_json: dict[str, Any],
    user_data: dict[str, Any] | None = None,
    draw_html_borders: bool = False,
) -> Table:
    """
    Converts TimTable-json into LaTeX-compatible object.
    Note: for correct functioning all the other modules should use this.

    :param table_json: Table data as json dictionary with
        'rows', 'tabledatablock', etc. at the first level.
    :param draw_html_borders Add light gray default borders around cells similarly to HTML-table.
    :return: Table-object containing the rows and cells in LaTex.
    """
    table_rows: list[Row] = []
    table = Table(table_rows)
    datablock = get_datablock(table_json)
    table_json = add_missing_elements(table_json, datablock)
    if user_data:
        table_json = add_missing_elements(table_json, user_data)
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
    # that level doesn't have any formatting.

    table_bg_color = get_color(table_json, "backgroundColor", CellColor.default())
    table_text_color = get_color(
        table_json, "color", CellColor(default_text_color, False)
    )
    table_font_family = get_font_family(table_json, default_font_family)
    table_borders = get_borders(table_json, CellBorders())
    table_font_size = get_font_size(table_json, default_font_size)
    table_h_align = get_text_horizontal_align(table_json, default_text_h_align)
    table_font_weight = get_key_value(table_json, "fontWeight", None)

    # Add light borders around every cell like in HTML-table.
    if draw_html_borders:
        table_borders = CellBorders()
        table_borders.set_all_borders(CellColor("lightgray", False))

    # Get column formattings:
    column_bg_color_list = get_column_color_list("backgroundColor", table_json)
    column_text_color_list = get_column_color_list("color", table_json)
    column_width_list = get_column_width_list(table_json)
    column_font_size_list = get_column_style_list(table_json, "fontSize")
    column_h_align_list = get_column_format_list(
        table_json, f=get_text_horizontal_align
    )
    column_font_family_list = get_column_format_list(table_json, f=get_font_family)

    # Get default (applied to all of same type) attributes:
    table_default_row_data = table_json.get("defrows", {})
    table_default_col_data = table_json.get("defcols", {})
    table_default_cell_data = table_json.get("defcells", {})

    table_default_row_height = get_size(
        table_default_row_data, key="height", default=None
    )
    table_default_col_height = get_size(
        table_default_col_data, key="width", default=None
    )
    table_default_cell_bgcolor = get_color(table_default_cell_data, "backgroundColor")
    table_default_cell_textcolor = get_color(table_default_cell_data, "color")

    table_json_rows = table_json.get("rows")
    if not table_json_rows:
        return table
    for i, row_data in enumerate(table_json_rows):
        table_row = table.get_or_create_row(i)

        # TODO: Change the logic for borers: in HTML these go around the whole row, not each cell!
        row_options = StyleOptions.from_dict(row_data, table_borders)

        skip_index = 0
        for j, _ in enumerate(row_data["row"]):
            # Skips following cells based on previous cell's colspan.
            if skip_index > 0:
                skip_index -= 1
                continue
            # Skip cells that have been added because of rowspan.
            if table_row.get_cell(j):
                continue

            cell_data = row_data["row"][j]
            content = get_content(cell_data)
            if not isinstance(cell_data, dict):
                cell_data = {}

            cell_options = StyleOptions.from_dict(cell_data, row_options.borders)

            # Get datablock formats:
            datablock_cell_data = get_datablock_cell_data(datablock, i, j)

            if datablock_cell_data or datablock_cell_data == "":
                content = get_content(datablock_cell_data)
            if not isinstance(datablock_cell_data, dict):
                datablock_cell_data = {}

            datablock_options = StyleOptions.from_dict(
                datablock_cell_data, row_options.borders
            )

            # Get user_data
            user_cell_data = get_datablock_cell_data(user_data, i, j)

            if user_cell_data or user_cell_data == "":
                content = get_content(user_cell_data)
            if not isinstance(user_cell_data, dict):
                user_cell_data = {}

            user_options = StyleOptions.from_dict(user_cell_data, row_options.borders)

            # Decide which styles to use (from table, column, row, cell or datablock)
            bg_color = decide_format_colors(
                [
                    table_bg_color,
                    table_default_cell_bgcolor,
                    column_bg_color_list[j],
                    row_options.bg_color,
                    cell_options.bg_color,
                    datablock_options.bg_color,
                    user_options.bg_color,
                ]
            )
            text_color = decide_format_colors(
                [
                    table_text_color,
                    table_default_cell_textcolor,
                    column_text_color_list[j],
                    row_options.text_color,
                    cell_options.text_color,
                    datablock_options.text_color,
                    user_options.text_color,
                ]
            )
            height = decide_format_size(
                [
                    table_default_row_height,
                    row_options.height,
                    cell_options.height,
                    datablock_options.height,
                    user_options.height,
                ]
            )
            width = decide_format_size(
                [
                    table_default_col_height,
                    column_width_list[j],
                    row_options.width,
                    cell_options.width,
                    datablock_options.width,
                    user_options.width,
                ]
            )
            h_align = decide_format(
                [
                    table_h_align,
                    column_h_align_list[j],
                    row_options.h_align,
                    cell_options.h_align,
                    datablock_options.h_align,
                    user_options.h_align,
                ]
            )
            font_family = decide_format(
                [
                    table_font_family,
                    column_font_family_list[j],
                    row_options.font_family,
                    cell_options.font_family,
                    datablock_options.font_family,
                    user_options.font_family,
                ]
            )
            font_size = decide_format(
                [
                    table_font_size,
                    column_font_size_list[j],
                    row_options.font_size,
                    cell_options.font_size,
                    datablock_options.font_size,
                    user_options.font_size,
                ]
            )
            font_weight = decide_format(
                [
                    table_font_weight,
                    row_options.font_weight,
                    cell_options.font_weight,
                    datablock_options.font_weight,
                    user_options.font_weight,
                ]
            )
            colspan, rowspan = decide_colspan_rowspan(
                [row_options, cell_options, datablock_options, user_options]
            )

            c = Cell(
                content=content,
                font_family=font_family or default_font_family,
                font_size=font_size or default_font_size,
                h_align=h_align or default_text_h_align,
                bg_color=bg_color,
                text_color=text_color,
                colspan=colspan or default_colspan,
                rowspan=rowspan or default_rowspan,
                cell_width=str(width) if width else default_width,
                cell_height=str(height) if height else default_height,
                borders=user_options.borders,
                font_weight=font_weight,
            )

            # TODO: Cells that are replaced in html by rowspan or colspan are left in some cases and
            # TODO: may break multicol and -row cells and their borders.
            # Cells with rowspan > 1:
            # Multirow-cells need to be set from bottom-up in LaTeX to
            # properly show bg-colors, and empty cells need to be placed
            # above to avoid overlap, since LaTeX doesn't automatically
            # move cells aside.
            if rowspan and rowspan > 1:
                # Take multicol-cells messing up indices into account with this:
                cell_index = table_row.get_colspan()
                for y in range(0, rowspan - 1):
                    # Empty filler cell has mostly same the settings as the multirow-cell:
                    d = copy_cell(c)
                    d.content = ""
                    d.borders.color_bottom = dataclasses.replace(c.bg_color)
                    if y > 1:
                        d.borders.color_top = dataclasses.replace(c.bg_color)
                    d.rowspan = 1
                    table.get_or_create_row(i + y).add_cell(cell_index, d)
                c.borders.color_top = dataclasses.replace(c.bg_color)
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
    table.fit_to_page_width = get_table_resize(
        table_json, table.estimate_width(), table.get_largest_col_count()
    )
    # Create horizontal border objects.
    table.create_hborders()

    return table
