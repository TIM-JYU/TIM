"""Routes for searching."""
import os
import re
import sre_constants
import subprocess
import time
from dataclasses import dataclass
from datetime import datetime
from enum import Enum
from io import StringIO
from pathlib import Path
from re import Pattern
from typing import Match

from flask import Blueprint, json, Request, Response
from flask import request
from sqlalchemy.orm import joinedload, lazyload, defaultload

from timApp.auth.accesshelper import has_view_access, verify_admin, has_edit_access
from timApp.auth.accesstype import AccessType
from timApp.auth.auth_models import BlockAccess
from timApp.auth.sessioninfo import get_current_user_object
from timApp.document.docentry import DocEntry, get_documents
from timApp.document.docinfo import DocInfo
from timApp.folder.folder import Folder
from timApp.item.block import Block
from timApp.item.routes import get_document_relevance
from timApp.item.routes_tags import get_tags
from timApp.item.tag import Tag
from timApp.timdb.dbaccess import get_files_path
from timApp.timdb.exceptions import InvalidReferenceException
from timApp.user.user import User
from timApp.util.flask.requesthelper import (
    get_option,
    use_model,
    RouteException,
    NotExist,
)
from timApp.util.flask.responsehelper import json_response
from timApp.util.logger import log_error, log_warning, log_info
from timApp.util.utils import get_error_message, cache_folder_path

search_routes = Blueprint("search", __name__, url_prefix="/search")

WHITE_LIST = ["c#"]  # Ignore query length limitations
MIN_QUERY_LENGTH = 3  # For word and title search. Tags have no limitations.
MIN_WHOLE_WORDS_QUERY_LENGTH = 1  # For whole word search.
PREVIEW_LENGTH = 40  # Before and after the search word separately.
PREVIEW_MAX_LENGTH = 160
SEARCH_CACHE_FOLDER = cache_folder_path / "searchcache"
PROCESSED_CONTENT_FILE_PATH = SEARCH_CACHE_FOLDER / "content_all_processed.log"
PROCESSED_TITLE_FILE_PATH = SEARCH_CACHE_FOLDER / "titles_all_processed.log"
PROCESSED_PATHS_FILE_PATH = SEARCH_CACHE_FOLDER / "paths_all_processed.log"
PROCESSED_TAGS_FILE_PATH = SEARCH_CACHE_FOLDER / "tags_all_processed.log"
RAW_CONTENT_FILE_PATH = SEARCH_CACHE_FOLDER / "all.log"
DEFAULT_RELEVANCE = 10


@dataclass
class GetFoldersModel:
    folder: str


@search_routes.get("getFolders")
@use_model(GetFoldersModel)
def get_subfolders(m: GetFoldersModel):
    """
    Returns subfolders of the starting folder.
    :return: Response containing a list of subfolder paths.
    """
    root_path = m.folder
    if root_path == "":
        return json_response([])
    folders = Folder.query.filter(Folder.location.like(root_path + "%")).limit(50)
    folders_viewable = [root_path]
    for folder in folders:
        if has_view_access(folder):
            folders_viewable.append(folder.path)
    return json_response(folders_viewable)


def get_common_search_params(req: Request) -> tuple[str, str, bool, bool, bool, bool]:
    """
    Picks parameters that are common in the search routes from a request.

    :param req: Request.
    :return: A tuple with six values.
    """
    query = req.args.get("query", "")
    case_sensitive = get_option(req, "caseSensitive", default=False, cast=bool)
    folder = req.args.get("folder", "")
    regex = get_option(req, "regex", default=False, cast=bool)
    search_owned_docs = get_option(req, "searchOwned", default=False, cast=bool)
    search_whole_words = get_option(req, "searchWholeWords", default=False, cast=bool)
    return query, folder, regex, case_sensitive, search_whole_words, search_owned_docs


def log_search_error(
    error: str,
    query: str,
    doc: str,
    tag: str = "",
    par: str = "",
    title: bool = False,
    path: bool = False,
) -> None:
    """
    Forms an error report and sends it to timLog.

    :param error: The error's message
    :param query: Search word.
    :param doc: Document identifier.
    :param tag: Tag name.
    :param par: Par id.
    :param title: If error was in title search.
    :param path: If error was in path search.
    :return: None.
    """
    if not error:
        error = "Unknown error"
    common_part = f"'{error}' while searching '{query}' in document {doc}"
    tag_part = ""
    par_part = ""
    title_part = ""
    path_part = ""
    if tag:
        tag_part = f" tag {tag}"
    if par:
        par_part = f" paragraph {par}"
    if title:
        title_part = " title"
    if path:
        path_part = " path"
    log_error(common_part + tag_part + par_part + title_part + path_part)


def preview_result(
    md: str,
    query: str,
    m: Match[str],
    snippet_length: int = PREVIEW_LENGTH,
    max_length: int = PREVIEW_MAX_LENGTH,
) -> str:
    """
    Forms preview of the match paragraph.

    :param md: Paragraph markdown to preview.
    :param query: Search word.
    :param m: Match object.
    :param snippet_length: The length of preview before and after search word.
    :param max_length: The maximum allowed length of the preview.
    :return: Preview with set amount of characters around search word.
    """
    start_index = m.start() - snippet_length
    end_index = m.end() + snippet_length
    # If the match is longer than given threshold, limit its size.
    if end_index - start_index > max_length:
        end_index = m.start() + len(query) + snippet_length
    prefix = "..."
    postfix = "..."
    if start_index < 0:
        start_index = 0
        prefix = ""
    if end_index > len(md):
        end_index = len(md)
        postfix = ""
    return prefix + md[start_index:end_index] + postfix


class WordResult:
    """
    One match word with location and match word.
    """

    def __init__(self, match_word: str, match_start: int, match_end: int):
        """
        Title or paragraph word result object constructor.

        :param match_word: String that matched query.
        :param match_start: Match start index.
        :param match_end: Match end index.
        """
        self.match_word = match_word
        self.match_start = match_start
        self.match_end = match_end

    def to_json(self):
        """
        :return: A dictionary containing object data, suitable for JSON-conversion.
        """
        return {
            "match_word": self.match_word,
            "match_start": self.match_start,
            "match_end": self.match_end,
        }


class ParResult:
    """
    Paragraph search results.
    """

    def __init__(
        self, par_id: str = "", preview: str = "", word_results=None, alt_num_results=0
    ):
        """
        Paragraph result object constructor.

        :param par_id: Paragrapg id.
        :param preview: A snippet from paragraph markdown.
        :param word_results: List of word search results in the paragraph.
        :param alt_num_results: Alternative to listing word results.
        """
        if word_results is None:
            word_results = []
        self.par_id = par_id
        self.preview = preview
        self.word_results = word_results
        self.alt_num_results = alt_num_results

    def add_result(self, result: WordResult) -> None:
        """
        Add new word result.

        :param result: New word result from paragraph markdown.
        :return: None.
        """
        self.word_results.append(result)

    def has_results(self) -> bool:
        """
        :return: True if the object contains results.
        """
        return len(self.word_results) > 0 or self.alt_num_results > 0

    def to_json(self):
        """
        :return: A dictionary of attributes and derived attributes.
        """
        results_dicts = []
        for r in self.word_results:
            results_dicts.append(r)
        return {
            "par_id": self.par_id,
            "preview": self.preview,
            "results": results_dicts,
            "num_results": self.get_match_count(),
        }

    def get_match_count(self) -> int:
        """
        :return: How many matches there are in this paragraph.
        """
        if len(self.word_results) > 0:
            return len(self.word_results)
        else:
            return self.alt_num_results


class TitleResult:
    """
    Title search result containing a list of match data.
    """

    def __init__(self, word_results=None, alt_num_results: int = 0):
        """
        Title result object constructor.

        :param word_results: List of word results from the title string.
        :param alt_num_results: Alternative to listing word results.
        """
        if word_results is None:
            word_results = []
        self.word_results = word_results
        self.alt_num_results = alt_num_results

    def add_result(self, result: WordResult) -> None:
        """
        Add new result to the list.

        :param result: New word result.
        :return: None.
        """
        self.word_results.append(result)

    def has_results(self) -> bool:
        """
        :return: Whether the object contains any results.
        """
        return len(self.word_results) > 0 or self.alt_num_results > 0

    def to_json(self):
        """
        :return: A dictionary of attributes and derived attributes, suitable for JSON-conversion.
        """
        results = []
        for r in self.word_results:
            results.append(r)
        return {"results": results, "num_results": self.get_match_count()}

    def get_match_count(self) -> int:
        """
        :return: How many match words the title has.
        """
        if len(self.word_results) > 0:
            return len(self.word_results)
        else:
            return self.alt_num_results


class DocResult:
    """
    Contains one document's title and word search information.
    """

    def __init__(
        self, doc_info: DocInfo, par_results=None, title_results=None, incomplete=False
    ):
        if par_results is None:
            par_results = []
        if title_results is None:
            title_results = []
        self.doc_info = doc_info
        self.par_results = par_results
        self.title_results = title_results
        self.incomplete = incomplete

    def add_par_result(self, result: ParResult) -> None:
        """
        Add new paragraph search result to the list.

        :param result: New paragraph result.
        :return: None.
        """
        self.par_results.append(result)

    def add_title_result(self, result: TitleResult) -> None:
        """
        Add new title search result to the list.

        :param result: New title result.
        :return: None.
        """
        self.title_results.append(result)

    def has_results(self) -> bool:
        """
        :return: Whether the document has any results in it.
        """
        return len(self.par_results) > 0 or len(self.title_results) > 0

    def to_json(self):
        """
        :return: A dictionary of the object, suitable for JSON-conversion.
        """
        par_result_dicts = []
        for r in self.par_results:
            par_result_dicts.append(r)
        title_result_dicts = []
        for r in self.title_results:
            title_result_dicts.append(r)
        return {
            "doc": self.doc_info,
            "incomplete": self.incomplete,
            "title_results": title_result_dicts,
            "num_title_results": self.get_title_match_count(),
            "par_results": par_result_dicts,
            "num_par_results": self.get_par_match_count(),
        }

    def get_par_match_count(self) -> int:
        """
        :return: Total document count for paragraph word matches.
        """
        count = 0
        for p in self.par_results:
            count += p.get_match_count()
        return count

    def get_title_match_count(self) -> int:
        """
        :return: Total document count for title matches.
        """
        count = 0
        for p in self.title_results:
            count += p.get_match_count()
        return count


def result_response(
    results,
    title_result_count: int = 0,
    word_result_count: int = 0,
    incomplete_search_reason="",
):
    """
    Formats result data for JSON-response.
    :param results: List of result dictionaries.
    :param title_result_count: Number of title results.
    :param word_result_count: Number of paragraph word results.
    :param incomplete_search_reason: Whether search was cut short.
    :return: Dictionary containing search results.
    """
    return {
        "title_result_count": title_result_count,
        "word_result_count": word_result_count,
        "errors": [],
        "incomplete_search_reason": incomplete_search_reason,
        "results": results,
    }


def validate_query(query: str, search_whole_words: bool) -> None:
    """
    Abort if query is too short.

    :param query: Search word(s).
    :param search_whole_words: Whole words search has different limits.
    :return: None.
    """
    if len(query.strip()) < MIN_QUERY_LENGTH and not search_whole_words:
        if query.strip().lower() not in WHITE_LIST:
            raise RouteException(
                f"Search text must be at least {MIN_QUERY_LENGTH} character(s) long with whitespace stripped."
            )
    if len(query.strip()) < MIN_WHOLE_WORDS_QUERY_LENGTH and search_whole_words:
        raise RouteException(
            f"Whole word search text must be at least {MIN_WHOLE_WORDS_QUERY_LENGTH} character(s) "
            f"long with whitespace stripped."
        )


# Query options for loading DocEntry relevance eagerly; it should speed up search cache processing because
# we know we'll need relevance.
docentry_eager_relevance_opt = (
    defaultload(DocEntry._block).joinedload(Block.relevance),
)


def add_doc_info_title_line(doc_id: int) -> str | None:
    """
    Forms a JSON-compatible string with doc id, title and path.

    :param doc_id: Document id.
    :return: String with doc data.
    """
    doc_info = DocEntry.find_by_id(
        doc_id, docentry_load_opts=docentry_eager_relevance_opt
    )
    if not doc_info:
        return None
    doc_relevance = get_document_relevance(doc_info)
    return (
        json.dumps(
            {"doc_id": doc_id, "d_r": doc_relevance, "doc_title": doc_info.title},
            ensure_ascii=False,
        )
        + "\n"
    )


def add_doc_info_content_line(
    doc_id: int, par_data, remove_deleted_pars: bool = True, add_title: bool = False
) -> str | None:
    """
    Forms a JSON-compatible string with doc_id and list of paragraph data with id and md attributes.

    :param doc_id: Document id.
    :param par_data: List of paragraph dictionaries.
    :param remove_deleted_pars: Check paragraph existence and leave deleted ones out.
    :param add_title Add document title.
    :return: String with paragraph data grouped under a document.
    """
    if not par_data:
        return None
    doc_info = DocEntry.find_by_id(
        doc_id, docentry_load_opts=docentry_eager_relevance_opt
    )
    if not doc_info:
        return None
    par_json_list = []

    doc_relevance = get_document_relevance(doc_info)

    for par in par_data:
        par_dict = json.loads(f"{{{par}}}")
        par_id = par_dict["id"]
        if remove_deleted_pars:
            # If par can't be found (deleted), don't add it.
            if not doc_info.document.has_paragraph(par_id):
                continue
        # Resolve the markdown in full (including references) for better search
        doc_par = doc_info.document.get_paragraph(par_id)
        par_md_buf = StringIO()
        if doc_par.is_par_reference() or doc_par.is_area_reference():
            try:
                ref_pars = doc_par.get_referenced_pars()
            except InvalidReferenceException:
                par_md_buf.write(doc_par.md)
            else:
                for p in ref_pars:
                    par_md_buf.write(f"{p.md}\n")
        else:
            par_md_buf.write(doc_par.md)

        par_md = par_md_buf.getvalue().replace("\r", " ").replace("\n", " ")
        # Cherry pick attributes, because others are unnecessary for the search.
        par_attrs = par_dict["attrs"]
        par_json_list.append({"id": par_id, "attrs": par_attrs, "md": par_md})
    if add_title:
        doc_title = doc_info.title
        return (
            json.dumps(
                {
                    "doc_id": doc_id,
                    "d_r": doc_relevance,
                    "doc_title": doc_title,
                    "pars": par_json_list,
                },
                ensure_ascii=False,
            )
            + "\n"
        )
    else:
        return (
            json.dumps(
                {"doc_id": doc_id, "d_r": doc_relevance, "pars": par_json_list},
                ensure_ascii=False,
            )
            + "\n"
        )


def add_doc_info_path_line(doc_id: int) -> str | None:
    """
    Forms a JSON-compatible string with doc id, relevance and path.

    :param doc_id: Document id.
    :return: String with doc data.
    """
    target = "path"
    doc_info = DocEntry.find_by_id(
        doc_id, docentry_load_opts=docentry_eager_relevance_opt
    )
    if not doc_info:
        return None
    doc_relevance = get_document_relevance(doc_info)
    return (
        json.dumps(
            {
                "doc_id": doc_id,
                "d_r": doc_relevance,
                "doc_path": doc_info.path
                if target == "path"
                else doc_info.title
                if target == "title"
                else doc_info,
            },
            ensure_ascii=False,
        )
        + "\n"
    )


def add_doc_info_tags_line(doc_id: int) -> str | None:
    """
    Forms a JSON-compatible string with doc id, relevance and tags.

    :param doc_id: Document id.
    :return: String with doc data.
    """
    doc_info = DocEntry.find_by_id(
        doc_id, docentry_load_opts=docentry_eager_relevance_opt
    )
    if not doc_info:
        return None
    doc_relevance = get_document_relevance(doc_info)
    doc_tags = " ".join([tag.name for tag in doc_info.block.tags])
    if doc_tags:
        return (
            json.dumps(
                {"doc_id": doc_id, "d_r": doc_relevance, "doc_tags": doc_tags},
                ensure_ascii=False,
            )
            + "\n"
        )
    else:
        return None


def add_doc_info_metadata_line(doc_id: int, target: str) -> str | None:
    """
    Forms a JSON-compatible string with doc id, relevance and metadata.

    :param doc_id: Document id.
    :param target: Search target (title, path, tags)
    :return: String with doc data.
    """
    doc_info = DocEntry.find_by_id(
        doc_id, docentry_load_opts=docentry_eager_relevance_opt
    )
    if not doc_info:
        return None
    doc_relevance = get_document_relevance(doc_info)

    match target:
        case "title":
            metadata = doc_info.title
        case "path":
            metadata = doc_info.path
        case "tags":
            metadata = " ".join([tag.name for tag in doc_info.block.tags])
        case _:
            metadata = None

    if metadata is not None:
        return (
            json.dumps(
                {"doc_id": doc_id, "d_r": doc_relevance, f"doc_{target}": metadata},
                ensure_ascii=False,
            )
            + "\n"
        )
    else:
        return None


def get_doc_par_id(line: str) -> tuple[int, str, str] | None:
    """
    Takes doc id, par id and par data from one grep search result line.

    :param line: Tim pars grep search result line.
    :return: Triple containing ids and par data.
    """
    if line and len(line) > 10:
        temp = line[2:].split("/", 2)
        doc_id = int(temp[0])
        par_id = temp[1]
        par_data = temp[2].replace("current:", "", 1)
        par_data = par_data[1:-2]
        return doc_id, par_id, par_data
    else:
        return None


def create_search_files(remove_deleted_pars=True) -> tuple[int, str]:
    """
    Groups all TIM-paragraphs under documents and combines them into a single file.
    Creates also a similar file for title searches and a raw file without grouping.

    :param remove_deleted_pars: Check paragraph existence before adding.
    :return: Status code and a message confirming success of file creation.
    """

    temp_content_file_name = (
        SEARCH_CACHE_FOLDER / f"temp_{PROCESSED_CONTENT_FILE_PATH.name}"
    )
    temp_title_file_name = (
        SEARCH_CACHE_FOLDER / f"temp_{PROCESSED_TITLE_FILE_PATH.name}"
    )
    temp_paths_file_name = (
        SEARCH_CACHE_FOLDER / f"temp_{PROCESSED_PATHS_FILE_PATH.name}"
    )
    temp_tags_file_name = SEARCH_CACHE_FOLDER / f"temp_{PROCESSED_TAGS_FILE_PATH.name}"
    index_log_file_name = SEARCH_CACHE_FOLDER / "index_log.log"
    f: Path = RAW_CONTENT_FILE_PATH.parent
    f.mkdir(exist_ok=True)

    try:
        subprocess.Popen(
            f'grep -R "" --include="current" . > {RAW_CONTENT_FILE_PATH} 2>&1',
            cwd=(get_files_path() / "pars"),
            shell=True,
        ).communicate()
    except Exception as e:
        return (
            400,
            f"Failed to create preliminary file {RAW_CONTENT_FILE_PATH}: {get_error_message(e)}",
        )
    try:
        raw_file = RAW_CONTENT_FILE_PATH.open("r", encoding="utf-8")
    except FileNotFoundError:
        return 400, f"Failed to open preliminary file {RAW_CONTENT_FILE_PATH}"
    try:
        with raw_file, temp_content_file_name.open(
            "w+", encoding="utf-8"
        ) as temp_content_file, temp_title_file_name.open(
            "w+", encoding="utf-8"
        ) as temp_title_file, temp_paths_file_name.open(
            "w+", encoding="utf-8"
        ) as temp_paths_file, temp_tags_file_name.open(
            "w+", encoding="utf-8"
        ) as temp_tags_file, index_log_file_name.open(
            "w+", encoding="utf-8"
        ) as index_log_file:

            current_doc, current_pars = None, []

            for line in raw_file:
                try:
                    doc_id, par_id, par = get_doc_par_id(line)
                    if not doc_id:
                        continue
                    if current_doc is None:
                        current_doc = doc_id
                    # If same doc as previous line or the first, just add par data to list.
                    if current_doc == doc_id:
                        current_pars.append(par)
                    # Otherwise save the previous one and empty par data.
                    else:
                        new_content_line = add_doc_info_content_line(
                            current_doc, current_pars, remove_deleted_pars
                        )
                        new_title_line = add_doc_info_title_line(current_doc)
                        new_paths_line = add_doc_info_path_line(current_doc)
                        new_tags_line = add_doc_info_tags_line(current_doc)
                        if new_content_line:
                            temp_content_file.write(new_content_line)
                        if new_title_line:
                            temp_title_file.write(new_title_line)
                        if new_paths_line:
                            temp_paths_file.write(new_paths_line)
                        if new_tags_line:
                            temp_tags_file.write(new_tags_line)
                        current_doc = doc_id
                        current_pars.clear()
                        current_pars.append(par)
                except Exception as e:
                    err = f"SEARCH_INDEX: '{get_error_message(e)}' while writing search file line '{line}'"
                    index_log_file.write(f"{err}\n")
                    print(err)

            # Write the last line separately, because loop leaves it unsaved.
            if current_doc and current_pars:
                new_content_line = add_doc_info_content_line(
                    current_doc, current_pars, remove_deleted_pars
                )
                new_title_line = add_doc_info_title_line(current_doc)
                new_paths_line = add_doc_info_path_line(current_doc)
                new_tags_line = add_doc_info_tags_line(current_doc)
                if new_content_line:
                    temp_content_file.write(new_content_line)
                if new_title_line:
                    temp_title_file.write(new_title_line)
                if new_paths_line:
                    temp_paths_file.write(new_paths_line)
                if new_tags_line:
                    temp_tags_file.write(new_tags_line)

            temp_content_file.flush()
            temp_title_file.flush()
            temp_paths_file.flush()
            temp_tags_file.flush()
            os.fsync(temp_content_file)
            os.fsync(temp_title_file)
            os.fsync(temp_paths_file)
            os.fsync(temp_tags_file)

        temp_content_file_name.rename(PROCESSED_CONTENT_FILE_PATH)
        temp_title_file_name.rename(PROCESSED_TITLE_FILE_PATH)
        temp_paths_file_name.rename(PROCESSED_PATHS_FILE_PATH)
        temp_tags_file_name.rename(PROCESSED_TAGS_FILE_PATH)

        return (
            200,
            f"Combined and processed index files created to \n"
            f"  {PROCESSED_CONTENT_FILE_PATH}, \n"
            f"  {PROCESSED_TITLE_FILE_PATH}, \n"
            f"  {PROCESSED_PATHS_FILE_PATH} and \n"
            f"  {PROCESSED_TAGS_FILE_PATH}",
        )
    except Exception as e:
        return (
            400,
            f"Creating files to \n"
            f"  {PROCESSED_CONTENT_FILE_PATH}, \n"
            f"  {PROCESSED_TITLE_FILE_PATH}, \n"
            f"  {PROCESSED_PATHS_FILE_PATH} and \n"
            f"  {PROCESSED_TAGS_FILE_PATH} \n"
            f" failed: {get_error_message(e)}!",
        )


@search_routes.get("createContentFile")
def create_search_files_route():
    """
    Route for grouping all TIM-paragraphs under documents and combining them into a single file.
    Creates also a similar file for title searches and a raw file without grouping.
    Note: may take several minutes, so timeout settings need to be lenient.

    :return: A message confirming success of file creation.
    """
    verify_admin()

    # 'removeDeletedPars' checks paragraph existence before adding at the cost of taking more time.
    status, msg = create_search_files(
        get_option(request, "removeDeletedPars", default=True, cast=bool)
    )
    return json_response(status_code=status, jsondata=msg)


@search_routes.get("/titles")
def title_search():
    """
    Performs search on document titles.
    :return: Title search results.
    """
    (
        query,
        folder,
        regex,
        case_sensitive,
        search_whole_words,
        search_owned_docs,
    ) = get_common_search_params(request)

    results = []
    title_result_count = 0

    validate_query(query, search_whole_words)

    if search_owned_docs:
        custom_filter = DocEntry.id.in_(
            get_current_user_object()
            .get_personal_group()
            .accesses.filter_by(type=AccessType.owner.value)
            .with_entities(BlockAccess.block_id)
        )
    else:
        custom_filter = None
    docs = get_documents(
        filter_user=get_current_user_object(),
        filter_folder=folder,
        query_options=lazyload(DocEntry._block),
        custom_filter=custom_filter,
        search_recursively=True,
    )

    if not docs:
        if not folder:
            folder = "root"
        raise RouteException(f"Folder '{folder}' not found or not accessible")

    term_regex = compile_regex(query, regex, case_sensitive, search_whole_words)

    if term_regex:
        for d in docs:
            current_doc = d.path
            try:
                title_result = TitleResult()
                d_title = d.title
                matches = list(term_regex.finditer(d_title))
                if matches:
                    title_result.alt_num_results = len(matches)
                    # for m in matches:
                    #     result = WordResult(match_word=m.group(0),
                    #                         match_start=m.start(),
                    #                         match_end=m.end())
                    #     title_result.add_result(result)

                if title_result.has_results():
                    r = DocResult(d)
                    r.add_title_result(title_result)
                    results.append(r)
                    title_result_count += r.get_title_match_count()
            except Exception as e:
                log_search_error(get_error_message(e), query, current_doc, par="")

    return json_response(result_response(results, title_result_count))


@search_routes.get("/tags")
def tag_search():
    """
    A route for document tag search.
    :return: Tag search results response.
    """
    (
        query,
        folder,
        regex,
        case_sensitive,
        search_whole_words,
        search_owned_docs,
    ) = get_common_search_params(request)
    relevance_threshold = get_option(request, "relevanceThreshold", default=1, cast=int)
    results = []

    # PostgreSQL doesn't support regex directly, so as workaround get all tags below the search folder
    # from the database and then apply regex search on them.
    any_tag = "%%"
    if search_owned_docs:
        custom_filter = DocEntry.id.in_(
            get_current_user_object()
            .get_personal_group()
            .accesses.filter_by(type=AccessType.owner.value)
            .with_entities(BlockAccess.block_id)
        ) & DocEntry.id.in_(
            Tag.query.filter(
                Tag.name.ilike(any_tag)
                & ((Tag.expires > datetime.now()) | (Tag.expires == None))
            ).with_entities(Tag.block_id)
        )
    else:
        custom_filter = DocEntry.id.in_(
            Tag.query.filter(
                Tag.name.ilike(any_tag)
                & ((Tag.expires > datetime.now()) | (Tag.expires == None))
            ).with_entities(Tag.block_id)
        )
    query_options = joinedload(DocEntry._block).joinedload(Block.tags)
    docs = get_documents(
        filter_user=get_current_user_object(),
        filter_folder=folder,
        search_recursively=True,
        custom_filter=custom_filter,
        query_options=query_options,
    )

    tag_result_count = 0
    error_list = []
    current_doc = "before search"
    current_tag = "before search"
    term_regex = compile_regex(query, regex, case_sensitive, search_whole_words)

    try:
        for d in docs:
            try:
                if is_excluded(get_document_relevance(d), relevance_threshold):
                    continue
                current_doc = d.path
                m_tags = []
                m_num = 0
                for tag in d.block.tags:
                    current_tag = tag.name
                    matches = list(term_regex.finditer(tag.name))
                    if matches:
                        match_count = len(matches)
                        if not query:
                            match_count = 1
                        m_num += match_count
                        tag_result_count += match_count
                        m_tags.append(tag)

                if m_num > 0:
                    results.append(
                        {"doc": d, "matching_tags": m_tags, "num_results": m_num}
                    )
            except Exception as e:
                error = get_error_message(e)
                error_list.append(
                    {"error": error, "doc_path": current_doc, "tag_name": current_tag}
                )
                log_search_error(error, query, current_doc, tag=current_tag)
    except Exception as e:
        raise RouteException(get_error_message(e))
    else:
        try:
            return json_response(
                {
                    "results": results,
                    "incomplete_search_reason": "",
                    "tag_result_count": tag_result_count,
                    "errors": error_list,
                }
            )
        except MemoryError:
            raise RouteException("MemoryError: results too large")
        except Exception as e:
            raise RouteException(
                f"Error encountered while formatting JSON-response: {e}"
            )


def compile_regex(
    query: str, regex: bool, case_sensitive: bool, search_whole_words: bool
):
    """
    Set flags and compile regular expression. Abort if invalid or empty regex.

    :param query: Search word.
    :param regex: Regex search.
    :param case_sensitive: Distinguish between upper and lower case in search.
    :param search_whole_words: Search words separated by spaces, commas etc.
    :return: Compiled regex.
    """
    if case_sensitive:
        flags = re.DOTALL
    else:
        flags = re.DOTALL | re.IGNORECASE
    if regex:
        term = query
    else:
        term = re.escape(query)
    if search_whole_words:
        term = rf"\b{term}\b"
    try:
        term_regex = re.compile(term, flags)
    except sre_constants.error as e:
        raise RouteException(f"Invalid regex: {str(e)}")
    else:
        if not term_regex:
            raise RouteException(f"Regex compiling failed")
        return term_regex


@search_routes.get("paths")
def path_search():
    """
    Document path search. Path results are treated as title results and use
    TitleResult objects and content & title search interface.

    :return: Path results.
    """
    (
        query,
        folder,
        regex,
        case_sensitive,
        search_whole_words,
        search_owned_docs,
    ) = get_common_search_params(request)
    validate_query(query, search_whole_words)
    term_regex = compile_regex(query, regex, case_sensitive, search_whole_words)
    relevance_threshold = get_option(request, "relevanceThreshold", default=1, cast=int)

    db_term = f"%{query}%"
    custom_filter = None
    if regex:
        if search_owned_docs:
            custom_filter = DocEntry.id.in_(
                get_current_user_object()
                .get_personal_group()
                .accesses.filter_by(type=AccessType.owner.value)
                .with_entities(BlockAccess.block_id)
            )
    else:
        if case_sensitive:
            if search_owned_docs:
                custom_filter = DocEntry.id.in_(
                    get_current_user_object()
                    .get_personal_group()
                    .accesses.filter_by(type=AccessType.owner.value)
                    .with_entities(BlockAccess.block_id)
                ) & DocEntry.name.like(db_term)
            else:
                custom_filter = DocEntry.name.like(db_term)
        else:
            if search_owned_docs:
                custom_filter = DocEntry.id.in_(
                    get_current_user_object()
                    .get_personal_group()
                    .accesses.filter_by(type=AccessType.owner.value)
                    .with_entities(BlockAccess.block_id)
                ) & DocEntry.name.ilike(db_term)
            else:
                custom_filter = DocEntry.name.ilike(db_term)

    docs = get_documents(
        filter_user=get_current_user_object(),
        filter_folder=folder,
        query_options=lazyload(DocEntry._block),
        custom_filter=custom_filter,
        search_recursively=True,
    )
    path_results = []
    path_result_count = 0
    for d in docs:
        current_doc = d.path
        try:
            if is_excluded(get_document_relevance(d), relevance_threshold):
                continue
            path_result = TitleResult()
            matches = list(term_regex.finditer(d.path))
            if matches:
                path_result.alt_num_results = len(matches)

            if path_result.has_results():
                r = DocResult(d)
                r.add_title_result(path_result)
                path_results.append(r)
                path_result_count += r.get_title_match_count()
        except Exception as e:
            log_search_error(get_error_message(e), query, current_doc, par="")

    return json_response(
        {
            "title_result_count": path_result_count,
            "word_result_count": 0,
            "errors": [],
            "incomplete_search_reason": "",
            "title_results": path_results,
            "content_results": [],
        }
    )


def is_excluded(relevance: int, relevance_threshold: int) -> bool:
    """
    Exclude if relevance is less than relevance threshold.

    :param relevance: Document relevance value.
    :param relevance_threshold: Min included relevance.
    :return: True if document relevance is less than relevance threshold.
    """
    if relevance < relevance_threshold:
        return True
    return False


def is_timeouted(start_time: float, timeout: float) -> bool:
    """
    Compares elapsed time and timeout limit.

    :param start_time: The time comparison starts from.
    :param timeout: Maximum allowed elapsed time.
    :return: True if timeout has been passed, false if not.
    """
    elapsed_time = time.time() - start_time
    return elapsed_time > timeout


def parse_search_items(output_file: list) -> dict | str:
    """
    Parses a list of search items

    :param output_file: list of search items
    :return: dictionary of the search items, or an error message
    """
    output_items = {}
    for line in output_file:
        if line and len(line) > 10:
            try:
                line = json.loads(line)
                output_items[line["doc_id"]] = line
            except Exception as e:
                return f"Exception while parsing output items: {get_error_message(e)}"
                # return get_error_message(e)
    return output_items


def fetch_search_items(search_items: dict, search_folder: str) -> list[DocInfo]:
    """
    Fetches entries from the database based on the keys of search items.

    :param search_items: dictionary of the search items
    :param search_folder: folder path that the search is limited to
    :return: list of DocInfo objects
    """
    doc_infos: list[DocInfo] = DocEntry.query.filter(
        (DocEntry.id.in_(search_items.keys()))
        & (DocEntry.name.like(search_folder + "%"))
    ).options(joinedload(DocEntry._block).joinedload(Block.relevance))
    return doc_infos


def check_with_common_search_params(
    doc: DocInfo,
    search_owned: bool,
    user: User,
    search_items: dict,
    ignore_relevance: bool,
    relevance_threshold: int,
) -> bool:
    """
    Checks for common search parameters/options

    :param doc: The current DocInfo object being checked
    :param search_owned: Whether search should only search in the user's own documents
    :param user: The current user
    :param search_items: Dictionary of pre-indexed search items
    :param ignore_relevance: Whether documents' relevance values should be ignored
    :param relevance_threshold: Minimum relevance value for the document to be included in search results
    :return: Boolean indicating whether all checks passed, or timeout message
    """
    # If not allowed to view, continue to the next one.
    if not has_view_access(doc):
        return False
    # Skip if searching only owned and it's not owned.
    if search_owned:
        if not user.has_ownership(doc, allow_admin=False):
            return False
    # If relevance is ignored or not found from search file, skip check.
    line_info = search_items[doc.id]
    if not ignore_relevance:
        try:
            relevance = line_info["d_r"]
        except KeyError:
            pass
        else:
            # Leave documents with excluded relevance out of the results.
            if is_excluded(relevance, relevance_threshold):
                return False
    return True


def grep_search_file(
    cmd: list[str], search_file_path: str, search_type: str
) -> list[str]:
    """
    Helper function to grep the specified (text) file

    :param cmd: command to process, along with its arguments
    :param search_file_path: (text) file to process
    :param search_type: type of search to perform, dictated by a Request from the UI
    :return: list of matches
    """
    if not search_file_path.exists():
        raise NotExist(
            f"Combined {search_type} file '{search_file_path}' not found, unable to perform {search_type} search!"
        )
    try:
        s = subprocess.Popen(cmd + [search_file_path], stdout=subprocess.PIPE)
        output_str = s.communicate()[0].decode("utf-8").strip()
        return output_str.splitlines()
    except Exception as e:
        raise RouteException(get_error_message(e))


@search_routes.get("")
def search():
    """
    Perform document word search on a combined and grouped par file using grep.

    :return: Document paragraph search results with total result count.
    """
    # If the file containing all TIM content doesn't exist, give warning immediately.
    content_search_file_path = PROCESSED_CONTENT_FILE_PATH
    title_search_file_path = PROCESSED_TITLE_FILE_PATH
    tags_search_file_path = PROCESSED_TAGS_FILE_PATH
    paths_search_file_path = PROCESSED_PATHS_FILE_PATH

    (
        query,
        folder,
        regex,
        case_sensitive,
        search_whole_words,
        search_owned_docs,
    ) = get_common_search_params(request)
    # max_results = get_option(request, "maxResults", default=1000000, cast=int)
    # max_doc_results = get_option(request, "maxDocResults", default=10000, cast=int)
    # ignore_plugins = get_option(request, "ignorePlugins", default=False, cast=bool)
    should_search_titles = get_option(request, "searchTitles", default=True, cast=bool)
    should_search_content = get_option(
        request, "searchContent", default=True, cast=bool
    )
    should_search_tags = get_option(request, "searchTags", default=True, cast=bool)
    should_search_paths = get_option(request, "searchPaths", default=True, cast=bool)
    # search_attrs = get_option(request, "searchAttrs", default=False, cast=bool)
    # relevance_threshold = get_option(request, "relevanceThreshold", default=1, cast=int)
    # ignore_relevance = get_option(request, "ignoreRelevance", default=False, cast=bool)
    timeout = get_option(request, "timeout", default=120, cast=int)

    start_time = time.time()
    total_search_time = time.time_ns()

    validate_query(query, search_whole_words)

    incomplete_search_reason = ""
    # current_doc = ""
    # current_par = ""
    content_output = []
    title_output = []
    tags_output = []
    paths_output = []
    content_results = []
    title_results = []
    tags_results = []
    paths_results = []
    word_result_count = 0
    title_result_count = 0
    tags_result_count = 0
    paths_result_count = 0
    user = get_current_user_object()

    term_regex = compile_regex(query, regex, case_sensitive, search_whole_words)

    cmd = ["grep"]
    if not case_sensitive:
        cmd.append("-i")
    if regex:
        cmd.append("-E")
    else:
        cmd.append("-F")
    if search_whole_words:
        cmd.append("-sw")
    cmd.append("--")
    cmd.append(query)

    if should_search_content:
        content_output = grep_search_file(cmd, content_search_file_path, "content")

    if should_search_titles:
        title_output = grep_search_file(cmd, title_search_file_path, "title")

    if should_search_tags:
        tags_output = grep_search_file(cmd, tags_search_file_path, "tags")

    if should_search_paths:
        paths_output = grep_search_file(cmd, paths_search_file_path, "paths")

    if not content_output and not title_output and not tags_output and not paths_output:
        return json_response(
            {
                "title_result_count": title_result_count,
                "word_result_count": word_result_count,
                "tags_result_count": tags_result_count,
                "paths_result_count": paths_result_count,
                "errors": [],
                "incomplete_search_reason": incomplete_search_reason,
                "title_results": title_results,
                "content_results": content_results,
                "tags_results": tags_results,
                "paths_results": paths_results,
            }
        )

    if should_search_titles:
        title_results, title_result_count, incomplete_search_reason = search_metadata(
            request,
            title_output,
            "title",
            start_time,
            timeout,
            user,
            term_regex,
        )
    if should_search_content:
        (
            content_results,
            word_result_count,
            incomplete_search_reason,
        ) = search_content(
            request, content_output, start_time, timeout, user, term_regex
        )
    if should_search_tags:
        tags_results, tags_result_count, incomplete_search_reason = search_metadata(
            request,
            tags_output,
            "tags",
            start_time,
            timeout,
            user,
            term_regex,
        )
    if should_search_paths:
        paths_results, paths_result_count, incomplete_search_reason = search_metadata(
            request,
            paths_output,
            "path",
            start_time,
            timeout,
            user,
            term_regex,
        )

    log_info(
        f"Search for '{query}' took {(total_search_time - time.time_ns()) / 1000000} milliseconds."
    )

    return json_response(
        {
            "content_results": content_results,
            "errors": [],
            "incomplete_search_reason": incomplete_search_reason,
            "paths_result_count": paths_result_count,
            "paths_results": paths_results,
            "tags_result_count": tags_result_count,
            "tags_results": tags_results,
            "title_result_count": title_result_count,
            "title_results": title_results,
            "word_result_count": word_result_count,
        }
    )


def search_metadata(
    req: Request,
    grep_output: list,
    target: str,
    start_time: float,
    timeout: float,
    user: User,
    term_regex: Pattern[str],
) -> (list[DocResult], int, str):
    """
    Collates search results for a type of search in a search index

    :param req: search request containing search options
    :param grep_output: temporary in-memory search index
    :param target: type of search ("content", "title", "path" or "tags")
    :param start_time: start time of the search process in seconds
    :param timeout: timeout limit for the search process
    :param user: current user object
    :param term_regex: regular expression pattern to use for the search
    :return: search results as a tuple: results, result count and search abort reason, if search was aborted or timed out
    """

    (
        query,
        folder,
        regex,
        case_sensitive,
        search_whole_words,
        search_owned_docs,
    ) = get_common_search_params(req)

    relevance_threshold = get_option(req, "relevanceThreshold", default=1, cast=int)
    ignore_relevance = get_option(req, "ignoreRelevance", default=False, cast=bool)
    current_doc = ""
    incomplete_search_reason = ""
    search_result_count = 0
    search_results = []

    search_items = parse_search_items(grep_output)
    if search_items.__class__ is str:
        log_search_error(
            search_items,
            query,
            current_doc,
            title=(target == "title"),
            path=(target == "path"),
        )

    doc_infos: list[DocInfo] = fetch_search_items(search_items, folder)

    for doc_info in doc_infos:
        current_doc = doc_info.title
        try:
            if is_timeouted(start_time, timeout):
                incomplete_search_reason = (
                    f"{target} search exceeded the timeout ({timeout} seconds)."
                )
                raise TimeoutError(f"{target} search timeout")

            if not check_with_common_search_params(
                doc_info,
                search_owned_docs,
                user,
                search_items,
                ignore_relevance,
                relevance_threshold,
            ):
                continue
            line_info = search_items[doc_info.id]

            doc_result = DocResult(doc_info)

            # Join tag names in collating function (add_doc_info_tags_line) so we don't have to have this special case
            # line_item = line_info[f"doc_{target}"]
            # if target == "tags":
            #     line_item = " ".join(line_info["doc_tags"])

            search_matches = list(term_regex.finditer(line_info[f"doc_{target}"]))
            if search_matches:
                search_match_count = len(search_matches)
                doc_result.add_title_result(
                    TitleResult(alt_num_results=search_match_count)
                )
                search_result_count += search_match_count

            if doc_result.has_results():
                search_results.append(doc_result)

        except TimeoutError as e:
            log_search_error(
                get_error_message(e),
                query,
                current_doc,
                title=(target == "title"),
                path=(target == "path"),
            )
            return search_results, search_result_count, incomplete_search_reason

        except Exception as e:
            log_search_error(
                get_error_message(e),
                query,
                current_doc,
                title=(target == "title"),
                path=(target == "path"),
            )

    return search_results, search_result_count, incomplete_search_reason


def search_titles(
    req: Request,
    title_output: list,
    start_time: float,
    timeout: float,
    user: User,
    term_regex: Pattern[str],
) -> (list[DocResult], int, str):
    (
        query,
        folder,
        regex,
        case_sensitive,
        search_whole_words,
        search_owned_docs,
    ) = get_common_search_params(req)
    relevance_threshold = get_option(req, "relevanceThreshold", default=1, cast=int)
    ignore_relevance = get_option(req, "ignoreRelevance", default=False, cast=bool)
    current_doc = ""
    incomplete_search_reason = ""
    title_result_count = 0
    title_results = []

    title_items = parse_search_items(title_output)
    if title_items.__class__ is str:
        log_search_error(title_items, query, current_doc, title=True)

    doc_infos: list[DocInfo] = fetch_search_items(title_items, folder)

    for doc_info in doc_infos:
        current_doc = doc_info.title
        try:
            if is_timeouted(start_time, timeout):
                incomplete_search_reason = (
                    f"title search exceeded the timeout ({timeout} seconds)"
                )
                raise TimeoutError("title search timeout")

            if not check_with_common_search_params(
                doc_info,
                search_owned_docs,
                user,
                title_items,
                ignore_relevance,
                relevance_threshold,
            ):
                continue
            line_info = title_items[doc_info.id]

            doc_result = DocResult(doc_info)

            title_matches = list(term_regex.finditer(line_info["doc_title"]))
            if title_matches:
                title_match_count = len(title_matches)
                doc_result.add_title_result(
                    TitleResult(alt_num_results=title_match_count)
                )
                title_result_count += title_match_count

            if doc_result.has_results():
                title_results.append(doc_result)

        except TimeoutError as e:
            log_search_error(get_error_message(e), query, current_doc, title=True)
            return json_response(
                {
                    "title_result_count": title_result_count,
                    "errors": [],
                    "incomplete_search_reason": incomplete_search_reason,
                    "title_results": title_results,
                }
            )
        except Exception as e:
            log_search_error(get_error_message(e), query, current_doc, title=True)

    return title_results, title_result_count, incomplete_search_reason


def search_content(
    req: Request,
    content_output: list,
    start_time: float,
    timeout: float,
    user: User,
    term_regex: Pattern[str],
) -> (list[DocResult], int, str):
    """
    Collates search results for document content search

    :param req: search request containing search options
    :param content_output: temporary in-memory content search index
    :param start_time: start time of the search process in seconds
    :param timeout: timeout limit for the search process
    :param user: current user object
    :param term_regex: regular expression pattern to use for the search
    :return: search results as a tuple: results, result count and search abort reason, if search was aborted or timed out
    """

    (
        query,
        folder,
        regex,
        case_sensitive,
        search_whole_words,
        search_owned_docs,
    ) = get_common_search_params(req)
    max_results = get_option(request, "maxResults", default=1000000, cast=int)
    max_doc_results = get_option(request, "maxDocResults", default=10000, cast=int)
    ignore_plugins = get_option(request, "ignorePlugins", default=False, cast=bool)
    search_attrs = get_option(request, "searchAttrs", default=False, cast=bool)
    relevance_threshold = get_option(req, "relevanceThreshold", default=1, cast=int)
    ignore_relevance = get_option(req, "ignoreRelevance", default=False, cast=bool)
    current_doc = ""
    current_par = ""
    incomplete_search_reason = ""
    word_result_count = 0
    content_results = []

    content_items = {}
    for line in content_output:
        if line and len(line) > 10:
            try:
                line = json.loads(line)
                content_items[line["doc_id"]] = line
            except Exception as e:
                log_search_error(get_error_message(e), query, current_doc)

    # doc_infos: list[DocInfo] = DocEntry.query.filter(
    #     (DocEntry.id.in_(content_items.keys())) & (DocEntry.name.like(folder + "%")) & DocEntry.
    # ).options(joinedload(DocEntry._block).joinedload(Block.relevance))

    doc_infos: list[DocInfo] = fetch_search_items(content_items, folder)
    # filter content search documents by view access and ownership (if that option was specified) here
    # instead of doing database accesses inside the search loop
    if search_owned_docs:
        doc_infos = list(
            doc_info
            for doc_info in doc_infos
            if has_view_access(doc_info)
            and user.has_ownership(doc_info, allow_admin=False)
        )
    else:
        doc_infos = list(filter(has_view_access, doc_infos))

    for doc_info in doc_infos:
        current_doc = doc_info.title
        try:
            if is_timeouted(start_time, timeout):
                incomplete_search_reason = (
                    f"content search exceeded the timeout ({timeout} seconds)"
                )
                raise TimeoutError("content search timeout")

            # If not allowed to view, continue to the next one.
            # if not has_view_access(doc_info):
            #     continue

            # Skip if searching only owned and it's not owned.
            # if search_owned_docs:
            #     if not user.has_ownership(doc_info, allow_admin=False):
            #         continue

            # If relevance is ignored or not found from search file, skip check.
            line_info = content_items[doc_info.id]
            if not ignore_relevance:
                relevance = line_info.get("d_r")
                if relevance is not None and is_excluded(
                    relevance, relevance_threshold
                ):
                    continue
            pars = line_info["pars"]

            doc_result = DocResult(doc_info)
            edit_access = None

            def check_edit_acc():
                nonlocal edit_access
                if edit_access:
                    return edit_access
                edit_access = has_edit_access(doc_info)
                return edit_access

            for i, par in enumerate(pars):
                par_id = par["id"]
                md = par["md"]

                # If par has visibility condition and user can't see markdown (lower than edit), skip it.
                try:
                    visibility = par["attrs"]["visible"]
                    if visibility and not check_edit_acc():
                        continue
                except KeyError:
                    pass

                # Tries to get plugin and settings key values from dict;
                # if par isn't either, gives KeyError and does nothing.
                try:
                    plugin = par["attrs"]["plugin"]
                    # If ignore_plugins or no edit access, leave out plugin and setting results.
                    if ignore_plugins or not check_edit_acc():
                        continue
                except KeyError:
                    pass
                try:
                    settings = par["attrs"]["settings"]
                    if ignore_plugins or not check_edit_acc():
                        continue
                except KeyError:
                    pass
                if search_attrs and check_edit_acc() and not ignore_plugins:
                    try:
                        rd = str(par["attrs"])
                        md = rd.replace("'", '"') + " " + md
                    except KeyError:
                        pass

                par_result = ParResult(par_id)
                par_matches = list(term_regex.finditer(md))

                if par_matches:
                    # Word results aren't used for anything currently,
                    # so to save time and bandwidth they are replaced by a number.
                    par_result.alt_num_results = len(par_matches)
                    # for m in matches:
                    #     result = WordResult(match_word=m.group(0),
                    #                         match_start=m.start(),
                    #                         match_end=m.end())
                    #     par_result.add_result(result)
                    par_result.preview = preview_result(md, query, par_matches[0])

                # Don't add empty par result (in error cases).
                if par_result.has_results():
                    doc_result.add_par_result(par_result)

                # End paragraph match search if limit has been reached, but
                # don't break and mark as incomplete if this was the last paragraph.
                if (
                    doc_result.get_par_match_count() > max_doc_results
                    and i != len(pars) - 1
                ):
                    incomplete_search_reason = (
                        f"one or more document has over the maximum "
                        f"of {max_doc_results} results"
                    )
                    doc_result.incomplete = True
                    break

            # If no valid paragraph results, skip document.
            if doc_result.has_results():
                word_result_count += doc_result.get_par_match_count()
                content_results.append(doc_result)

            # End search if the limit is reached.
            if word_result_count > max_results:
                incomplete_search_reason = f"more than maximum of {max_results} results"
                break
        except TimeoutError as e:
            log_search_error(get_error_message(e), query, current_doc)
            return json_response(
                {
                    "word_result_count": word_result_count,
                    "errors": [],
                    "incomplete_search_reason": incomplete_search_reason,
                    "content_results": content_results,
                }
            )
        except Exception as e:
            log_search_error(get_error_message(e), query, current_doc, par=current_par)

    return content_results, word_result_count, incomplete_search_reason


def search_tags(
    req: Request,
    tags_output: list,
    start_time: float,
    timeout: float,
    user: User,
    term_regex: Pattern[str],
) -> (list[DocResult], int, str):
    (
        query,
        folder,
        regex,
        case_sensitive,
        search_whole_words,
        search_owned_docs,
    ) = get_common_search_params(req)
    relevance_threshold = get_option(req, "relevanceThreshold", default=1, cast=int)
    ignore_relevance = get_option(req, "ignoreRelevance", default=False, cast=bool)
    current_doc = ""
    incomplete_search_reason = ""
    tags_results = []
    tags_result_count = 0

    tags_items = parse_search_items(tags_output)
    if tags_items.__class__ is str:
        log_search_error(tags_items, query, current_doc)

    doc_infos: list[DocInfo] = fetch_search_items(tags_items, folder)

    for doc_info in doc_infos:
        current_doc = doc_info.title
        try:
            if is_timeouted(start_time, timeout):
                incomplete_search_reason = (
                    f"tags search exceeded the timeout ({timeout} seconds)"
                )
                raise TimeoutError("tags search timeout")

            if not check_with_common_search_params(
                doc_info,
                search_owned_docs,
                user,
                tags_items,
                ignore_relevance,
                relevance_threshold,
            ):
                continue
            line_info = tags_items[doc_info.id]
            doc_result = DocResult(doc_info)
            tags_matches = list(term_regex.finditer(" ".join(line_info["doc_tags"])))
            if tags_matches:
                tags_match_count = len(tags_matches)
                doc_result.add_title_result(
                    TitleResult(alt_num_results=tags_match_count)
                )
                tags_result_count += tags_match_count

            if doc_result.has_results():
                tags_results.append(doc_result)

        except TimeoutError as e:
            log_search_error(get_error_message(e), query, current_doc, tag=True)
            return json_response(
                {
                    "tags_result_count": tags_result_count,
                    "errors": [],
                    "incomplete_search_reason": incomplete_search_reason,
                    "tags_results": tags_results,
                }
            )
        except Exception as e:
            log_search_error(get_error_message(e), query, current_doc, tag=True)

    return tags_results, tags_result_count, incomplete_search_reason


def search_paths(
    req: Request,
    paths_output: list,
    start_time: float,
    timeout: float,
    user: User,
    term_regex: Pattern[str],
) -> (list[DocResult], int, str):
    (
        query,
        folder,
        regex,
        case_sensitive,
        search_whole_words,
        search_owned_docs,
    ) = get_common_search_params(req)
    relevance_threshold = get_option(req, "relevanceThreshold", default=1, cast=int)
    ignore_relevance = get_option(req, "ignoreRelevance", default=False, cast=bool)
    current_doc = ""
    incomplete_search_reason = ""
    paths_results = []
    paths_result_count = 0

    paths_items = parse_search_items(paths_output)
    if paths_items.__class__ is str:
        log_search_error(paths_items, query, current_doc)

    doc_infos: list[DocInfo] = fetch_search_items(paths_items, folder)

    for doc_info in doc_infos:
        current_doc = doc_info.title
        try:
            if is_timeouted(start_time, timeout):
                incomplete_search_reason = (
                    f"paths search exceeded the timeout ({timeout} seconds)"
                )
                raise TimeoutError("paths search timeout")

            if not check_with_common_search_params(
                doc_info,
                search_owned_docs,
                user,
                paths_items,
                ignore_relevance,
                relevance_threshold,
            ):
                continue
            line_info = paths_items[doc_info.id]
            doc_result = DocResult(doc_info)

            paths_matches = list(term_regex.finditer(line_info["doc_path"]))
            if paths_matches:
                paths_match_count = len(paths_matches)
                doc_result.add_title_result(
                    TitleResult(alt_num_results=paths_match_count)
                )
                paths_result_count += paths_match_count

            if doc_result.has_results():
                paths_results.append(doc_result)

        except TimeoutError as e:
            log_search_error(get_error_message(e), query, current_doc, path=True)
            return json_response(
                {
                    "path_result_count": paths_result_count,
                    "errors": [],
                    "incomplete_search_reason": incomplete_search_reason,
                    "path_results": paths_results,
                }
            )
        except Exception as e:
            log_search_error(get_error_message(e), query, current_doc, path=True)

    return paths_results, paths_result_count, incomplete_search_reason
