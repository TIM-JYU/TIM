"""
Functions related to document partitioning.
"""
from dataclasses import dataclass
from pathlib import Path
from typing import Union

from flask import json, Request
from lxml import html

from timApp.document.docinfo import DocInfo
from timApp.document.prepared_par import PreparedPar
from timApp.document.randutils import hashfunc
from timApp.document.viewcontext import default_view_ctx
from timApp.util.utils import Range

INCLUDE_IN_PARTS_CLASS_NAME = (
    "includeInParts"  # Preamble pars with this class get inserted to each doc part.
)


def int_or_none(s: str):
    try:
        return int(s)
    except (ValueError, TypeError):
        return None


RangeParam = Union[int, str]


@dataclass
class IndexedViewRange:
    b: int
    e: int
    par_count: int

    @property
    def is_full(self):
        return self.b == 0 and self.e == self.par_count

    def to_json(self, name: str | None = None):
        r = {"b": self.b, "e": self.e, "is_full": self.is_full}
        if name:
            r["name"] = name
        return r

    @property
    def is_restricted(self):
        return not self.is_full

    @property
    def start_index(self):
        return self.b

    @property
    def end_index(self):
        return self.e

    @property
    def starts_from_beginning(self):
        return self.b == 0


@dataclass
class RequestedViewRange:
    b: RangeParam | None
    e: RangeParam | None
    size: int | None

    @property
    def is_full(self):
        return self.b is None and self.e is None

    @property
    def is_restricted(self):
        return not self.is_full

    @property
    def start_index(self):
        return int_or_none(self.b)

    @property
    def end_index(self):
        return int_or_none(self.e)

    @property
    def start_par_id(self):
        if self.start_index:
            return None
        return self.b

    @property
    def end_par_id(self):
        if self.end_index:
            return None
        return self.e


def partition_texts(
    texts: list[PreparedPar], view_range: IndexedViewRange, preamble_count
):
    """
    Partition document with preambles taken into account.

    :param texts: List of processed paragraphs.
    :param view_range: Range of normal paragraphs to include.
    :param preamble_count: Number of preamble paragraphs to insert.
    :return: List of included paragraphs.
    """
    i = 0
    partitioned = []
    b = view_range.start_index + preamble_count
    e = view_range.end_index + preamble_count
    for text in texts:
        if i >= e:
            break
        if i < preamble_count or i >= b:
            partitioned.append(text)
        i += 1
    return partitioned


def get_doc_version_hash(doc_info: DocInfo) -> str:
    """
    Gets version numbers from document and its preambles and creates a hash from them.

    :param doc_info: Document.
    :return: Version number hash as a string.
    """
    version = str(doc_info.document.get_version())
    for preamble in doc_info.get_preamble_docs():
        version += str(preamble.document.get_version())
    return hashfunc(version)


def load_index(file_path: Path) -> dict | None:
    """
    Load headers from a file.

    :param file_path: Cache file path.
    :return: Parsed contents or None.
    """
    try:
        with file_path.open("r", encoding="utf-8") as file:
            return json.load(file)
    except (FileNotFoundError, ValueError, OSError, TypeError):
        return None


def save_index(index, file_path: Path):
    """
    Save document header data as json.

    :param index: Headers.
    :param file_path: File path.
    """
    file_path.parent.mkdir(parents=True, exist_ok=True)
    with file_path.open("w", encoding="utf-8") as f:
        json.dump(index, f)


def get_index_with_header_id(doc_info: DocInfo, header_id: str) -> int | None:
    """
    Returns first index containing the given HTML header id.

    :param doc_info: Document.
    :param header_id: HTML header id.
    :return: Index of the corresponding paragraph or None if not found.
    """
    pars = doc_info.document.get_dereferenced_paragraphs(default_view_ctx)
    for i, par in enumerate(pars):
        if par:
            par_elements = html.fragment_fromstring(
                par.get_html(default_view_ctx), create_parent=True
            )
            for element in par_elements.iterdescendants():
                html_id = element.attrib.get("id")
                if html_id and header_id == html_id:
                    return i
    return None


def get_piece_size_from_cookie(request: Request) -> int | None:
    """
    Reads piece size from cookie, if it exists.

    :param request: Request.
    :return: Piece size integer or None, if cookie not found.
    """
    r = request.cookies.get("r")
    try:
        return int(r)
    except (TypeError, ValueError):
        return None


def get_preamble_count(d: DocInfo) -> int:
    """
    Get the amount of preambles in the document.

    :param d: Document.
    :return: Preamble count; zero if none were found.
    """
    preambles = d.document.insert_preamble_pars()
    return len(preambles)


def get_document_areas(doc: DocInfo) -> list[Range]:
    """
    Get a list of areas in the document.

    .. note:: Areas inside areas are ignored.

    :param doc: Document.
    :return: List of area ranges.
    """
    pars = doc.document.get_paragraphs()
    areas = []
    area = {"index": None, "name": None}
    for i, par in enumerate(pars):
        area_begin = par.get_attr("area")
        area_end = par.get_attr("area_end")
        if area["name"] is None and area_begin is not None:
            area = {"index": i, "name": area_begin}
        if area_end is not None and area_end == area["name"]:
            areas.append((area["index"], i))
            area = {"index": None, "name": None}
    return areas


def get_area_range(doc: DocInfo, name: str) -> Range | None:
    """
    Gets the range of the area in the document
    :param doc: Document
    :param name: Named area to search for
    :return: Area range if area was found and not broken, else None
    """
    pars = doc.document.get_paragraphs()
    begin = None
    end = None
    for i, par in enumerate(pars):
        area_begin = par.get_attr("area")
        area_end = par.get_attr("area_end")
        if area_begin == name:
            begin = i
        if area_end == name:
            end = i
            break
    if begin is not None and end is not None:
        return begin, end
    return None


def adjust_to_areas(areas: list[Range], b: int, e: int) -> Range:
    """
    Ensure range doesn't cut any areas.

    :param areas: List of areas.
    :param b: Begin index.
    :param e: End index.
    :return: b and e adjusted to ranges.
    """
    for area in areas:
        area_b = area[0]
        # TODO: Check if all ranges related to partitioning use same logic (i.e. is end index included or not).
        area_e = area[1] + 1  # Include area end in the range.
        if area_b <= b <= area_e <= e:
            # Don't return, since e may possibly still cut a later area.
            b, e = area_b, e
        elif b <= area_b <= e <= area_e:
            return b, area_e
        elif area_b <= b <= e <= area_e:
            # If range is wholly inside the area, return area.
            return area_b, area_e
    return b, e


def decide_view_range(
    doc_info: DocInfo,
    preferred_set_size: int,
    index: int = 0,
    forwards: bool = True,
    areas: list[Range] | None = None,
    min_set_size_modifier: float = 0.5,
) -> IndexedViewRange:
    """
    Decide begin and end indices of paragraph set based on preferred size.
    Avoids making the current part shorter than the piece size due to proximity to begin or end
    of the document. Also combines remaining paragraphs at the start or end, if they'd be smaller than allowed.
    For example: With set_size = 50 and modifier = 0.5 this will combine neighboring set if its size is 25 or less.

    :param doc_info: Document.
    :param preferred_set_size: User defined set size. May change depending on document.
    :param index: Begin or end index (depending on direction).
    :param forwards: Begin index is the start index if true, end index if false (i.e. True = next, False = previous).
    :param areas: List of known areas.
    :param min_set_size_modifier: Smallest allowed neighboring set compared to set size.
    :return: Adjusted indices for view range.
    """
    par_count = len(doc_info.document.get_paragraphs())
    if areas is None:
        areas = get_document_areas(doc_info)
    min_piece_size = round(min_set_size_modifier * preferred_set_size)
    if forwards:
        b = index
        e = min(preferred_set_size + index, par_count)
        # Avoid too short range when the start index is near the end of the document.
        if (e - b) <= min_piece_size:
            b = max(min(e - min_piece_size, b), 0)
        # Round the end index to last index when the remaining part is short.
        if par_count - e <= min_piece_size:
            e = par_count
    else:
        b = max(index - preferred_set_size, 0)
        e = index
        # Avoid too short range when the end index is near the beginning of the document.
        if (e - b) <= min_piece_size:
            e = min(e + min_piece_size, par_count)
        # Round the start index to zero when the remaining part is short.
        if b <= min_piece_size:
            b = 0
    fb, fe = adjust_to_areas(areas, b, e)
    return IndexedViewRange(b=fb, e=fe, par_count=par_count)
