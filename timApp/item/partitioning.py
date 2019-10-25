"""
Functions related to document partitioning.
"""
from flask import json, Request

from timApp.document.docinfo import DocInfo
from timApp.util.logger import log_error
from timApp.util.utils import Range, get_error_message
from lxml import html
from pathlib import Path
from typing import Optional, List

INCLUDE_IN_PARTS_CLASS_NAME = "includeInParts" # Preamble pars with this class get inserted to each doc part.


def partition_texts(texts, view_range: Range, preamble_count):
    """
    Partition document with preambles taken into account.
    :param texts: List of processed paragraphs.
    :param view_range: Range of normal paragraphs to include.
    :param preamble_count: Number of preamble paragraphs to insert.
    :return: List of included paragraphs.
    """
    i = 0
    partitioned = []
    b = view_range[0] + preamble_count
    e = view_range[1] + preamble_count
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
    return str(hash(version))


def load_index(folder_path: Path(), file_name: str) -> Optional[str]:
    """
    Load headers from a file.
    :param folder_path: Cache folder path.
    :param file_name: File name.
    :return: Parsed contents or None.
    """
    try:
        with (folder_path / file_name).open("r", encoding='utf-8') as file:
            contents = json.load(file)
            return contents
    except FileNotFoundError:
        return None
    except Exception as e:
        log_error(get_error_message(e))
        return None


def save_index(index, folder_path: Path(), file_name: str):
    """
    Save document header data as json.
    :param index: Headers.
    :param folder_path: Save folder path.
    :param file_name: File name.
    """
    folder_path.mkdir(parents=True, exist_ok=True)
    with (folder_path / file_name).open("w", encoding='utf-8') as f:
        json.dump(index, f)


def get_index_with_header_id(doc_info: DocInfo, header_id: str) -> Optional[int]:
    """
    Returns first index containing the given HTML header id.
    :param doc_info: Document.
    :param header_id: HTML header id.
    :return: Index of the corresponding paragraph or None if not found.
    """
    pars = doc_info.document.get_paragraphs()
    for i, par in enumerate(pars):
        if par:
            try:
                par_elements = html.fragment_fromstring(par.get_html(), create_parent=True)
                for element in par_elements.iterdescendants():
                    html_id = element.attrib.get("id")
                    if html_id and header_id == html_id:
                        return i
            except AssertionError:
                continue
    return None


def get_piece_size_from_cookie(request: Request) -> Optional[int]:
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


def get_document_areas(doc: DocInfo) -> List[Range]:
    """
    Get a list of areas in the document.
    :param doc: Document.
    :return: List of area ranges.
    """
    # TODO: Can areas overlap?
    pars = doc.document.get_paragraphs()
    areas = []
    first = None
    for i, par in enumerate(pars):
        if par.is_area():
            if not first:
                first = i
            else:
                areas.append((first, i))
                first = None
    if first is not None:
        areas.append((first, len(pars)))
    return areas


def adjust_to_areas(areas: List[Range], b: int, e: int) -> Range:
    """
    Ensure range doesn't cut any areas.
    :param areas: List of areas.
    :param b: Begin index.
    :param e: End index.
    :return: b and e adjusted to ranges.
    """
    for area in areas:
        area_b = area[0]
        area_e = area[1]
        if area_b <= b <= area_e <= e:
            # Don't return, since e may possibly still cut a later area.
            b, e = area_b, e
        elif b <= area_b <= e <= area_e:
            return b, area_e
        elif area_b <= b <= e <= area_e:
            # If range is wholly inside the area, return area.
            return area_b, area_e
    return b, e


def decide_view_range(doc_info: DocInfo, preferred_set_size: int, index: int = 0, par_count: Optional[int] = None,
                      forwards: bool = True, areas: Optional[List[Range]] = None,
                      min_set_size_modifier: float = 0.5) -> Optional[Range]:
    """
    Decide begin and end indices of paragraph set based on preferred size.
    Avoids making the current part shorter than the piece size due to proximity to begin or end
    of the document. Also combines remaining paragraphs at the start or end, if they'd be smaller than allowed.
    For example: With set_size = 50 and modifier = 0.5 this will combine neighboring set if its size is 25 or less.

    :param doc_info: Document.
    :param preferred_set_size: User defined set size. May change depending on document.
    :param index: Begin or end index (depending on direction).
    :param par_count: Paragraph count, if it is available.
    :param forwards: Begin index is the start index if true, end index if false (i.e. True = next, False = previous).
    :param areas: List of known areas.
    :param min_set_size_modifier: Smallest allowed neighboring set compared to set size.
    :return: Adjusted indices for view range.
    """
    if not par_count:
        par_count = len(doc_info.document.get_paragraphs())
    if areas is None: # An empty area list is a separate case.
        areas = get_document_areas(doc_info)
    try:
        min_piece_size = round(min_set_size_modifier * preferred_set_size)
        if forwards:
            b = index
            e = min(preferred_set_size + index, par_count)
            # Avoid too short range when the start index is near the end of the document.
            if (e - b) <= min_piece_size:
                b = max(min(e - min_piece_size, b), 0)
            # Round the end index to last index when the remaining part is short.
            if par_count-e <= min_piece_size:
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
    except TypeError:
        return None
    else:
        return adjust_to_areas(areas, b, e)
