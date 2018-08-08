"""Routes for searching."""
import re
import sre_constants
import subprocess
from datetime import datetime
from pathlib import Path
from typing import Match, List, Union, Tuple

from elasticsearch.helpers import bulk
from elasticsearch_dsl import connections, Document, Text, Keyword, Integer, Search
from flask import Blueprint, json, stream_with_context, Response
from flask import abort
from flask import request
from sqlalchemy.orm import joinedload

from timApp.auth.accesshelper import has_view_access, verify_admin, has_edit_access
from timApp.auth.accesstype import AccessType
from timApp.auth.auth_models import BlockAccess
from timApp.auth.sessioninfo import get_current_user_object
from timApp.document.docentry import DocEntry, get_documents
from timApp.document.docinfo import DocInfo
from timApp.folder.folder import Folder
from timApp.item.block import Block
from timApp.item.tag import Tag
from timApp.tim_app import app
from timApp.timdb.exceptions import TimDbException
from timApp.util.flask.requesthelper import get_option
from timApp.util.flask.responsehelper import json_response, ok_response
from timApp.util.logger import log_error

es = connections.create_connection(hosts=['elasticsearch'], timeout=20)

search_routes = Blueprint('search',
                          __name__,
                          url_prefix='/search')

WHITE_LIST = ["c#"]  # Ignore query length limitations
MIN_QUERY_LENGTH = 3  # For word and title search. Tags have no limitations.
MIN_WHOLE_WORDS_QUERY_LENGTH = 1  # For whole word search.
PREVIEW_LENGTH = 40  # Before and after the search word separately.
PREVIEW_MAX_LENGTH = 160
PROCESSED_CONTENT_FILE_NAME = "all_processed.log"
RAW_CONTENT_FILE_NAME = "all.log"
MIN_CONTENT_FILE_LINE_LENGTH = 70  # Excludes empty documents.


@search_routes.route('getFolders')
def get_subfolders():
    """
    Returns subfolders of the starting folder.
    Options:
    folder = Starting folder.
    :return: Response containing a list of subfolder paths.
    """
    root_path = request.args.get('folder', '')
    folders = Folder.query.filter(Folder.location.like(root_path + '%'))
    folders_viewable = [root_path]
    for folder in folders:
        if has_view_access(folder):
            folders_viewable.append(folder.path)
    return json_response(folders_viewable)


def get_common_search_params(request) -> Tuple[str, str, bool, bool, bool, bool]:
    """
    Picks parameters that are common in the search routes from a request.
    :param request:
    :return: A tuple with six values.
    """
    query = request.args.get('query', '')
    case_sensitive = get_option(request, 'caseSensitive', default=False, cast=bool)
    folder = request.args.get('folder', '')
    regex = get_option(request, 'regex', default=False, cast=bool)
    search_owned_docs = get_option(request, 'searchOwned', default=False, cast=bool)
    search_whole_words = get_option(request, 'searchWholeWords', default=False, cast=bool)
    return query, folder, regex, case_sensitive, search_whole_words, search_owned_docs


@search_routes.route('/tags')
def tag_search():
    """
    A route for document tag search.
    :return: Tag search results response.
    """
    (query, folder, regex, case_sensitive, search_whole_words, search_owned_docs) = get_common_search_params(request)
    results = []

    # PostgreSQL doesn't support regex directly, so as workaround get all tags below the search folder
    # from the database and then apply regex search on them.
    any_tag = "%%"
    if search_owned_docs:
        custom_filter = DocEntry.id.in_(get_current_user_object().get_personal_group().accesses.
                                        filter_by(type=AccessType.owner.value)
                                        .with_entities(BlockAccess.block_id)) & \
                        DocEntry.id.in_(Tag.query.
                                        filter(Tag.name.ilike(any_tag) & ((Tag.expires > datetime.now()) |
                                                                          (Tag.expires == None))).
                                        with_entities(Tag.block_id))
    else:
        custom_filter = DocEntry.id.in_(Tag.query.
                                        filter(Tag.name.ilike(any_tag) & ((Tag.expires > datetime.now()) |
                                                                          (Tag.expires == None))).
                                        with_entities(Tag.block_id))
    query_options = joinedload(DocEntry._block).joinedload(Block.tags)
    docs = get_documents(filter_user=get_current_user_object(), filter_folder=folder,
                         search_recursively=True, custom_filter=custom_filter,
                         query_options=query_options)

    tag_result_count = 0
    error_list = []
    current_doc = "before search"
    current_tag = "before search"
    if case_sensitive:
        flags = re.DOTALL
    else:
        flags = re.DOTALL | re.IGNORECASE
    if regex:
        term = query
    else:
        term = re.escape(query)
    if search_whole_words:
        term = fr"\b{term}\b"
    try:
        term_regex = re.compile(term, flags)
        for d in docs:
            try:
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
                    results.append({'doc': d, 'matching_tags': m_tags, 'num_results': m_num})
            except Exception as e:
                error = get_error_message(e)
                error_list.append({
                    'error': error,
                    'doc_path': current_doc,
                    'tag_name': current_tag
                })
                log_search_error(error, query, current_doc, tag=current_tag)
    except sre_constants.error as e:
        abort(400, f"Invalid regex: {str(e)}")
    except Exception as e:
        abort(400, get_error_message(e))
    else:
        try:
            return json_response({'results': results,
                                  'incomplete_search_reason': "",
                                  'tag_result_count': tag_result_count,
                                  'errors': error_list
                                  })
        except MemoryError:
            abort(400, "MemoryError: results too large")
        except Exception as e:
            abort(400, f"Error encountered while formatting JSON-response: {e}")


def log_search_error(error: str, query: str, doc: str, tag: str = "", par: str = "") -> None:
    """
    Forms an error report and sends it to timLog.
    :param error: The error's message
    :param query: Search word.
    :param doc: Document identifier.
    :param tag: Tag name.
    :param par: Par id.
    :return: None.
    """
    if not error:
        error = "Unknown error"
    common_part = f"'{error}' while searching '{query}' in document {doc}"
    tag_part = ""
    par_part = ""
    if tag:
        tag_part = f" tag {tag}"
    if par:
        par_part = f" paragraph {par}"
    log_error(common_part + tag_part + par_part)


def get_documents_by_access_type(access: AccessType) -> List[DocEntry]:
    """
    Return all documents that user has certain access type to.
    :param access: Access type, for example: AccessType.owner or AccessType.view.
    :return: List of documents the user has the set type of access to.
    """
    block_query = get_current_user_object().get_personal_group().accesses.filter_by(type=access.value).with_entities(
        BlockAccess.block_id)
    docs = DocEntry.query.filter(DocEntry.id.in_(block_query)).all()
    return docs


def preview_result(md: str, query, m: Match[str], snippet_length: int = PREVIEW_LENGTH,
                   max_length: int = PREVIEW_MAX_LENGTH) -> str:
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
        return {'match_word': self.match_word, 'match_start': self.match_start, 'match_end': self.match_end}


class ParResult:
    """
    Paragraph search results.
    """

    def __init__(self, par_id: str = "", preview: str = "", word_results=None):
        """
        Paragraph result object constructor.
        :param par_id: Paragrapg id.
        :param preview: A snippet from paragraph markdown.
        :param word_results: List of word search results in the paragraph.
        """
        if word_results is None:
            word_results = []
        self.par_id = par_id
        self.preview = preview
        self.word_results = word_results

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
        return len(self.word_results) > 0

    def to_json(self):
        """
        :return: A dictionary of attributes and derived attributes.
        """
        results_dicts = []
        for r in self.word_results:
            results_dicts.append(r)
        return {'par_id': self.par_id,
                'preview': self.preview,
                'results': results_dicts, 'num_results': self.get_match_count()}

    def get_match_count(self) -> int:
        """
        :return: How many matches there are in this paragraph.
        """
        return len(self.word_results)


class TitleResult:
    """
    Title search result containing a list of match data.
    """

    def __init__(self, word_results=None):
        """
        Title result object constructor.
        :param word_results: List of word results from the title string.
        """
        if word_results is None:
            word_results = []
        self.word_results = word_results

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
        return len(self.word_results) > 0

    def to_json(self):
        """
        :return: A dictionary of attributes and derived attributes, suitable for JSON-conversion.
        """
        results = []
        for r in self.word_results:
            results.append(r)
        return {'results': results, 'num_results': self.get_match_count()}

    def get_match_count(self) -> int:
        """
        :return: How many match words the paragraph has.
        """
        return len(self.word_results)


class DocResult:
    """
    Contains one document's title and word search information.
    """

    def __init__(self, doc_info: DocInfo, par_results=None, title_results=None, incomplete=False):
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
            'doc': self.doc_info, 'incomplete': self.incomplete,
            'title_results': title_result_dicts, 'num_title_results': self.get_title_match_count(),
            'par_results': par_result_dicts, 'num_par_results': self.get_par_match_count()}

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


def in_doc_list(doc_info: DocInfo, docs: List[DocEntry], shorten_list: bool = True) -> bool:
    """
    Check whether a document is in document entry list.
    :param doc_info: Document to search from the list.
    :param docs: List of documents.
    :param shorten_list: Remove found documents from the list to save time on later loops.
    :return: Whether the document is in list.
    """
    for d in docs:
        if doc_info.id == d.id:
            if shorten_list:
                docs.remove(d)
            return True
    return False


def add_doc_info_line(doc_id: int, par_data, remove_deleted_pars: bool = True, add_title: bool = True) \
        -> Union[str, None]:
    """
    Forms a JSON-compatible string with doc_id and list of paragraph data with id and md attributes.
    :param doc_id: Document id.
    :param par_data: List of paragraph dictionaries.
    :param remove_deleted_pars: Check paragraph existence and leave deleted ones out.
    :param add_title Add document title.
    :return: String with paragraph data grouped under a document.
    """
    doc_info = None
    if not par_data:
        return None
    if remove_deleted_pars:
        doc_info = DocEntry.find_by_id(doc_id)
        if not doc_info:
            return None
    par_json_list = []
    for par in par_data:
        par_dict = json.loads(f"{{{par}}}")
        par_id = par_dict['id']
        if remove_deleted_pars:
            # If par can't be found (deleted), don't add it.
            try:
                doc_info.document.get_paragraph(par_id)
            except TimDbException:
                continue
        # Cherry pick attributes, because others are unnecessary for the search.
        par_md = par_dict['md'].replace("\r", " ").replace("\n", " ")
        par_attrs = par_dict['attrs']
        par_json_list.append({'id': par_id, 'attrs': par_attrs, 'md': par_md})
    if add_title:
        if not doc_info:
            doc_info = DocEntry.find_by_id(doc_id)
        doc_title = doc_info.title
        return json.dumps({'doc_id': doc_id, 'doc_title': doc_title, 'pars': par_json_list}, ensure_ascii=False) + '\n'
    else:
        return json.dumps({'doc_id': doc_id, 'pars': par_json_list}, ensure_ascii=False) + '\n'


def get_doc_par_id(line: str) -> Union[Tuple[int, str, str], None]:
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


class Paragraph(Document):
    body = Text(analyzer='snowball', fielddata=True)  # md
    plugin = Text()
    setting = Text()
    par_id = Keyword()
    doc_id = Integer()

    class Index:
        name = 'pars'

    def is_plugin_or_setting(self) -> bool:
        if self.plugin or self.setting:
            return True
        else:
            return False

    def save(self, **kwargs):
        return super(Paragraph, self).save(**kwargs)


@search_routes.route("elasticsearch/createIndex")
def elasticsearch_create_index():
    def generate():
        es.indices.delete(index='pars', ignore=[400, 404])
        Paragraph.init()
        with open(Path(app.config['FILES_PATH']) / 'pars' / RAW_CONTENT_FILE_NAME, "r+", encoding='utf-8') as file:
            for line in file:
                doc_id, par_id, par = get_doc_par_id(line)
                par_dict = json.loads(f"{{{par}}}")
                par_md = par_dict['md']
                yield {'_index': 'pars',
                       '_type': 'doc',
                       '_id': f'{doc_id}.{par_id}',
                       "_source": {
                           'doc_id': doc_id,
                           'par_id': par_id,
                           'body': par_md}
                       }

    bulk(es, generate())
    return ok_response()


@search_routes.route("elasticsearch")
def elasticsearch():
    query = request.args.get('q', '')
    s = Search(index="pars").query("match", body=query)
    s.aggs.bucket('per_paragraph', 'terms', field='body')

    response = s.execute()
    results = []
    for hit in response:
        results.append({'doc_id': hit.doc_id, 'par_id': hit.par_id, 'md': hit.body})
    return json_response(results)


@search_routes.route("createContentFile")
def create_search_file():
    """
    Groups all TIM-paragraphs under documents and combines them into a single file.
    Creates also a raw file without grouping.
    Note: may take several minutes, so timeout settings need to be lenient.
    :return: A message confirming success of file creation.
    """
    verify_admin()

    # Checks paragraph existence before adding at the cost of taking more time.
    remove_deleted_pars = get_option(request, 'removeDeletedPars', default=False, cast=bool)
    # Include document titles in the file.
    add_titles = get_option(request, 'addTitles', default=False, cast=bool)

    dir_path = Path(app.config['FILES_PATH']) / 'pars'
    raw_file_name = RAW_CONTENT_FILE_NAME
    file_name = PROCESSED_CONTENT_FILE_NAME
    raw_file = None

    try:
        subprocess.Popen(f'grep -R "" --include="current" . > {raw_file_name} 2>&1',
                         cwd=dir_path,
                         shell=True).communicate()
    except Exception as e:
        abort(400,
              f"Failed to create preliminary file {dir_path / raw_file_name}: {get_error_message(e)}")
    try:
        raw_file = open(dir_path / raw_file_name, "r", encoding='utf-8')
    except FileNotFoundError:
        abort(400, f"Failed to open preliminary file {dir_path / raw_file_name}")
    try:
        with raw_file, open(dir_path / file_name, "w+", encoding='utf-8') as file:
            current_doc, current_pars = -1, []
            for line in raw_file:
                try:
                    doc_id, par_id, par = get_doc_par_id(line)
                    if not doc_id:
                        continue
                    if not current_doc:
                        current_doc = doc_id
                    # If same doc as previous line or the first, just add par data to list.
                    if current_doc == doc_id:
                        current_pars.append(par)
                        continue
                    # Otherwise save the previous one and empty par data.
                    else:
                        new_line = add_doc_info_line(current_doc, current_pars, remove_deleted_pars, add_titles)
                        if new_line and len(new_line) >= MIN_CONTENT_FILE_LINE_LENGTH:
                            file.write(new_line)
                        current_doc = doc_id
                        current_pars.clear()
                        current_pars.append(par)
                except Exception as e:
                    log_error(f"'{get_error_message(e)}' while writing search file line '{line}''")
            # Write the last line separately, because loop leaves it unsaved.
            if current_doc and current_pars:
                new_line = add_doc_info_line(current_doc, current_pars, remove_deleted_pars, add_titles)
                if new_line and len(new_line) >= MIN_CONTENT_FILE_LINE_LENGTH:
                    file.write(new_line)
        return json_response({'status': f"Combined and processed paragraph file created to {dir_path / file_name}"})
    except Exception as e:
        abort(400, f"Failed to create search file {dir_path / file_name}: {get_error_message(e)}")


@search_routes.route("/titles")
def title_search():
    """
    Performs search on document titles.
    :return: Title search results.
    """
    (query, folder, regex, case_sensitive, search_whole_words, search_owned_docs) = get_common_search_params(request)

    results = []
    title_result_count = 0
    term_regex = None

    validate_query(query, search_whole_words)

    if search_owned_docs:
        custom_filter = DocEntry.id.in_(get_current_user_object().get_personal_group().accesses.
                                        filter_by(type=AccessType.owner.value)
                                        .with_entities(BlockAccess.block_id))
    else:
        custom_filter = None
    docs = get_documents(
        filter_user=get_current_user_object(),
        filter_folder=folder,
        custom_filter=custom_filter,
        search_recursively=True)

    if not docs:
        if not folder:
            folder = "root"
        abort(400, f"Folder '{folder}' not found or not accessible")

    if case_sensitive:
        flags = re.DOTALL
    else:
        flags = re.DOTALL | re.IGNORECASE
    if regex:
        term = query
    else:
        term = re.escape(query)
    if search_whole_words:
        # Picks the term if it's a whole word, only word or separated by comma etc.
        term = fr"(?:^|\W)({term})(?:$|\W)"
    try:
        term_regex = re.compile(term, flags)
    except sre_constants.error as e:
        abort(400, f"Invalid regex: {str(e)}")
    # TODO: Make faster.
    if term_regex:
        for d in docs:
            current_doc = d.path
            try:
                title_result = TitleResult()
                d_title = d.title
                matches = list(term_regex.finditer(d_title))
                if matches:
                    for m in matches:
                        result = WordResult(match_word=m.group(0),
                                            match_start=m.start(),
                                            match_end=m.end())
                        title_result.add_result(result)
                if title_result.has_results():
                    r = DocResult(d)
                    r.add_title_result(title_result)
                    results.append(r)
                    title_result_count += r.get_title_match_count()
            except Exception as e:
                log_search_error(get_error_message(e), query, current_doc, par="")

    return json_response(result_response(results, title_result_count))


def result_response(results, title_result_count: int = 0, word_result_count: int = 0, incomplete_search_reason=""):
    """
    Formats result data for JSON-response.
    :param results: List of result dictionaries.
    :param title_result_count: Number of title results.
    :param word_result_count: Number of paragraph word results.
    :param incomplete_search_reason: Whether search was cut short.
    :return: Dictionary containing search results.
    """
    return {
        'title_result_count': title_result_count,
        'word_result_count': word_result_count,
        'errors': [],
        'incomplete_search_reason': incomplete_search_reason,
        'results': results,
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
            abort(400, f'Search text must be at least {MIN_QUERY_LENGTH} character(s) long with whitespace stripped.')
    if len(query.strip()) < MIN_WHOLE_WORDS_QUERY_LENGTH and search_whole_words:
        abort(400, f'Whole word search text must be at least {MIN_WHOLE_WORDS_QUERY_LENGTH} character(s) '
                   f'long with whitespace stripped.')


def get_error_message(e: Exception):
    """
    Gives error message with error class.
    :param e: Exception.
    :return: String 'ErrorClass: reason'.
    """
    return f"{str(e.__class__.__name__)}: {str(e)}"


@search_routes.route("")
def search():
    """
    Perform document word search on a combined and grouped par file using grep.
    :return: Document paragraph search results with total result count.
    """
    # If the file containing all TIM content doesn't exists, give warning immediately.
    dir_path = Path(app.config['FILES_PATH']) / 'pars'
    search_file_name = PROCESSED_CONTENT_FILE_NAME
    if not Path(dir_path / search_file_name).exists():
        abort(404, f"Combined content file '{search_file_name}' not found, unable to perform content search!")

    (query, folder, regex, case_sensitive, search_whole_words, search_owned_docs) = get_common_search_params(request)
    max_results = get_option(request, 'maxResults', default=1000000, cast=int)
    ignore_plugins = get_option(request, 'ignorePlugins', default=False, cast=bool)

    validate_query(query, search_whole_words)

    term_regex = None
    owned_docs = []
    incomplete_search_reason = ""
    current_doc = ""
    current_par = ""
    output = []
    results = []
    word_result_count = 0

    cmd = ["grep"]

    if case_sensitive:
        flags = re.DOTALL
    else:
        cmd.append("-i")
        flags = re.DOTALL | re.IGNORECASE
    if regex:
        cmd.append("-E")
        term = query
    else:
        cmd.append("-F")
        term = re.escape(query)
    if search_whole_words:
        # Picks the term if it's a whole word, only word or separated by comma etc.
        cmd.append("-sw")
        term = fr"(?:^|\W)({term})(?:$|\W)"
    try:
        term_regex = re.compile(term, flags)
    except sre_constants.error as e:
        abort(400, f"Invalid regex: {str(e)}")

    cmd.append(query)
    cmd.append(search_file_name)

    # TODO: Error cases in subprocess may not show, slips through as empty search result instead.
    # TODO: Output has all paragraphs even though only one or more have matches.
    try:
        s = subprocess.Popen(cmd,
                             cwd=dir_path,
                             stdout=subprocess.PIPE)
        output_str = s.communicate()[0].decode('utf-8').strip()
        output = output_str.splitlines()
    except Exception as e:
        abort(400, get_error_message(e))
    if not output:
        return json_response(result_response([]))

    if search_owned_docs:
        owned_docs = get_documents_by_access_type(AccessType.owner)
        if not owned_docs:
            abort(400, f"No owned documents found")

    for line in output:
        try:
            if line and len(line) > 10:

                # The file is supposed to contain doc_id and pars in a list for each document.
                line_info = json.loads(line)

                doc_id = line_info['doc_id']
                doc_info = DocEntry.find_by_id(doc_id)

                # If doc isn't in the search path, continue to the next one.
                if not doc_info.path.startswith(folder):
                    continue

                # If not allowed to view, continue to the next one.
                if not has_view_access(doc_info):
                    continue

                # Skip if searching only owned and it's not owned.
                if search_owned_docs:
                    if not in_doc_list(doc_info, owned_docs):
                        continue

                pars = line_info['pars']
                doc_result = DocResult(doc_info)
                edit_access = has_edit_access(doc_info)

                for par in pars:
                    par_id = par['id']
                    md = par['md']

                    # Tries to get plugin and settings key values from dict;
                    # if par isn't either, gives KeyError and does nothing.
                    try:
                        plugin = par['attrs']['plugin']
                        # If ignore_plugins or no edit access, leave out plugin and setting results.
                        if ignore_plugins or not edit_access:
                            continue
                    except KeyError:
                        pass
                    try:
                        settings = par['attrs']['settings']
                        if ignore_plugins or not edit_access:
                            continue
                    except KeyError:
                        pass

                    par_result = ParResult(par_id)
                    matches = list(term_regex.finditer(md))

                    if matches:
                        for m in matches:
                            result = WordResult(match_word=m.group(0),
                                                match_start=m.start(),
                                                match_end=m.end())
                            par_result.add_result(result)
                        par_result.preview = preview_result(md, query, matches[0])

                    # Don't add empty par result (in error cases).
                    if par_result.has_results():
                        doc_result.add_par_result(par_result)

                # If no valid paragraph results, skip document.
                if doc_result.has_results():
                    word_result_count += doc_result.get_par_match_count()
                    results.append(doc_result)  # Save directly as dict to save time.

                # End search if the limit is reached.
                if word_result_count > max_results:
                    incomplete_search_reason = f"more than maximum of {max_results} results"
                    break

        except Exception as e:
            log_search_error(get_error_message(e), query, current_doc, par=current_par)

    return json_response(result_response(
        results,
        word_result_count=word_result_count,
        incomplete_search_reason=incomplete_search_reason))
