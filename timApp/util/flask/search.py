"""Routes for searching."""
import re
import sre_constants
import subprocess
from datetime import datetime
from typing import Match, List

from flask import Blueprint, json
from flask import abort
from flask import request
from sqlalchemy.orm import joinedload

from timApp.auth.accesshelper import verify_logged_in, has_view_access
from timApp.auth.accesstype import AccessType
from timApp.auth.auth_models import BlockAccess
from timApp.auth.sessioninfo import get_current_user_id
from timApp.auth.sessioninfo import get_current_user_object
from timApp.document.docentry import DocEntry, get_documents
from timApp.document.docinfo import DocInfo
from timApp.folder.folder import Folder
from timApp.item.block import Block
from timApp.item.tag import Tag
from timApp.util.flask.cache import cache
from timApp.util.flask.requesthelper import get_option
from timApp.util.flask.responsehelper import json_response
from timApp.util.logger import log_error

search_routes = Blueprint('search',
                          __name__,
                          url_prefix='/search')

WHITE_LIST = ["c#"]  # Ignore query length limitations
MIN_QUERY_LENGTH = 3  # For word and title search. Tags have no limitations.
MIN_EXACT_WORDS_QUERY_LENGTH = 1  # For whole word search.
PREVIEW_LENGTH = 40  # Before and after the search word separately.
PREVIEW_MAX_LENGTH = 160


# noinspection PyUnusedLocal
def make_cache_key(*args, **kwargs):
    path = request.path
    return (str(get_current_user_id()) + path + str(request.query_string)).encode('utf-8')


@search_routes.route('/getFolders')
def get_subfolders():
    """
    Returns subfolders of the starting folder.
    Options:
    folder = Starting folder.
    recursive = Search every subfolder's subfolder; otherwise only to three steps depth.
    :return: A list of subfolder paths.
    """
    verify_logged_in()
    recursive = get_option(request, 'recursive', default=False, cast=bool)
    folder_set = set()
    if recursive:
        get_folders_recursive(request.args.get('folder', ''), folder_set)
    else:
        get_folders_three_levels(request.args.get('folder', ''), folder_set)
    return json_response(list(folder_set))


def get_folders_three_levels(starting_path: str, folder_set) -> None:
    """
    Limited folder search with depth of three steps from the root:
    root/level_1/level_2/level_3
    :param starting_path: The search root folder path.
    :param folder_set: A string set where the results are saved.
    :return: None.
    """
    folders = Folder.get_all_in_path(starting_path)
    for folder_l1 in folders:
        folder_l1_path = folder_l1.path
        folder_set.add(folder_l1_path)
        for folder_l2 in Folder.get_all_in_path(folder_l1_path):
            folder_l2_path = folder_l2.path
            folder_set.add(folder_l2_path)
            for folder_l3 in Folder.get_all_in_path(folder_l2_path):
                folder_l3_path = folder_l3.path
                folder_set.add(folder_l3_path)


def get_folders_recursive(starting_path: str, folder_set) -> None:
    """
    Recursive function for get_subfolders. Note: very slow in large and deep directories.
    :param starting_path: The search root folder path.
    :param folder_set: A string set where the results are saved.
    :return: None.
    """
    folders = Folder.get_all_in_path(starting_path)
    if folders:
        for folder in folders:
            folder_set.add(folder.path)
            get_folders_recursive(folder.path, folder_set)


@search_routes.route('/tags')
def tag_search():
    """
    A route for document tag search.
    """
    # verify_logged_in()

    query = request.args.get('query', '')
    case_sensitive = get_option(request, 'caseSensitive', default=False, cast=bool)
    folder = request.args.get('folder', '')
    regex_option = get_option(request, 'regex', default=False, cast=bool)
    search_owned_docs = get_option(request, 'searchOwned', default=False, cast=bool)
    search_exact_words = get_option(request, 'searchExactWords', default=False, cast=bool)
    results = []

    # PostgreSQL doesn't support regex directly, so as workaround get all tags below the search folder
    # from the database and then apply regex search on them.
    any_tag = "%%"
    custom_filter = DocEntry.id.in_(Tag.query.filter(Tag.name.ilike(any_tag) &
                                                     ((Tag.expires > datetime.now()) | (Tag.expires == None))).
                                    with_entities(Tag.block_id))
    query_options = joinedload(DocEntry._block).joinedload(Block.tags)
    docs = get_documents(filter_user=get_current_user_object(), filter_folder=folder,
                         search_recursively=True, custom_filter=custom_filter,
                         query_options=query_options)
    if not docs:
        if not folder:
            folder = "root"
        abort(400, f"Folder '{folder}' not found or not accessible")

    if search_owned_docs:
        docs = list(set(docs) - (set(docs) - set(get_documents_by_access_type(AccessType.owner))))
    tag_result_count = 0
    error_list = []
    current_doc = "before search"
    current_tag = "before search"
    if case_sensitive:
        flags = re.DOTALL
    else:
        flags = re.DOTALL | re.IGNORECASE
    if regex_option:
        term = query
    else:
        term = re.escape(query)
    if search_exact_words:
        term = fr"\b{term}\b"
    try:
        regex = re.compile(term, flags)
        for d in docs:
            try:
                current_doc = d.path
                m_tags = []
                m_num = 0
                for tag in d.block.tags:
                    current_tag = tag.name
                    matches = list(regex.finditer(tag.name))
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
                error = f"{str(e.__class__.__name__)}: {str(e)}"
                error_list.append({
                    'error': error,
                    'doc_path': current_doc,
                    'tag_name': current_tag
                })
                log_search_error(error, query, current_doc, tag=current_tag)
    except sre_constants.error as e:
        abort(400, f"Invalid regex: {str(e)}")
    except Exception as e:
        abort(400, f"{str(e.__class__.__name__)}: {str(e)}")
    else:
        try:
            return json_response({'results': results,
                                  'incomplete_search_reason': "",
                                  'tagResultCount': tag_result_count,
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


def get_documents_by_access_type(access: AccessType):
    """
    Return all documents that user has certain access type to.
    :param access: Access type, for example: AccessType.owner or AccessType.view.
    :return: List of documents the user has the set type of access to.
    """
    block_query = get_current_user_object().get_personal_group().accesses.filter_by(type=access.value).with_entities(
        BlockAccess.block_id)
    docs = DocEntry.query.filter(DocEntry.id.in_(block_query)).all()
    return docs


def preview(md: str, query, m: Match[str], snippet_length=PREVIEW_LENGTH, max_length=PREVIEW_MAX_LENGTH):
    """
    Forms preview of the match paragraph.
    :param md: Paragraph markdown to preview.
    :param query: Search word.
    :param m: Match object.
    :param snippet_length: The lenght of preview before and after search word.
    :param max_length: The maximum allowed length of the preview.
    :return: Preview with set amount of characters around search word.
    """
    start_index = m.start() - snippet_length
    end_index = m.end() + snippet_length
    # If the match is longer than given treshold, limit its size.
    if end_index - start_index > max_length:
        end_index = m.start() + len(query) + snippet_length
    prefix = "..."
    postfix = "..."
    if start_index < 0:
        start_index = 0
        prefix = ""
    if end_index > len(md):
        end_index = len(md)
        postfix = "..."
    return prefix + md[start_index:end_index] + postfix


class WordResult:
    """
    One match word with location and match word.
    """

    def __init__(self, match_word: str, match_start: int, match_end: int):
        self.match_word = match_word
        self.match_start = match_start
        self.match_end = match_end

    def __repr__(self):
        return f"match_word: {self.match_word}, match_start: {self.match_start}, match_end: {self.match_end},"

    def to_dict(self):
        return {'match_word': self.match_word, 'match_start': self.match_start, 'match_end': self.match_end}


class ParResult:
    """
    Paragraph search results.
    """

    def __init__(self, par_id: str = "", preview: str = "", word_results=None):
        if word_results is None:
            word_results = []
        self.par_id = par_id
        self.preview = preview
        self.word_results = word_results

    def add_result(self, result: WordResult):
        self.word_results.append(result)

    def has_results(self) -> bool:
        return len(self.word_results) > 0

    def __repr__(self):
        temp_preview = self.preview.replace('\n', ' ').replace('\r', '')
        temp_preview = ""
        return f"par_id: {self.par_id}, preview: {temp_preview}, word_results: {repr(self.word_results)}"

    def to_dict(self):
        """
        Form a dictionary from the attributes and derived attributes.
        :return:
        """
        results_dicts = []
        for r in self.word_results:
            results_dicts.append(r.to_dict())
        return {'par_id': self.par_id,
                'preview': self.preview,
                'results': results_dicts, 'num_results': self.get_match_count()}

    def get_match_count(self):
        """
        :return: How many matches there are in this paragraph.
        """
        return len(self.word_results)


class TitleResult:
    """
    Title search result containing a list of match data.
    """

    def __init__(self, word_results=None):
        if word_results is None:
            word_results = []
        self.word_results = word_results

    def add_result(self, result: WordResult):
        """
        Add new result to the list.
        :param result:
        :return:
        """
        self.word_results.append(result)

    def has_results(self) -> bool:
        """
        :return: Whether the object contains any results.
        """
        return len(self.word_results) > 0

    def __repr__(self):
        return f"word_results: {repr(self.word_results)}"

    def to_dict(self):
        """
        Form a dictionary from the attributes and derived attributes.
        :return:
        """
        results_dicts = []
        for r in self.word_results:
            results_dicts.append(r.to_dict())
        return {'results': results_dicts, 'num_results': self.get_match_count()}

    def get_match_count(self):
        """
        How many match words the paragraph has.
        :return:
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

    def add_par_result(self, result: ParResult):
        """
        Add new paragraph search result to the list.
        :param result:
        :return: None.
        """
        self.par_results.append(result)

    def add_title_result(self, result: TitleResult):
        """
        Add new title search result to the list.
        :param result:
        :return: None.
        """
        self.title_results.append(result)

    def has_results(self) -> bool:
        """
        :return: Whether the document has any results in it.
        """
        return len(self.par_results) > 0 or len(self.title_results) > 0

    def __repr__(self):
        return f"doc_info: {repr(self.doc_info)}, par_results: {repr(self.par_results)}, " \
               f"title_results: {repr(self.title_results)}"

    def to_dict(self):
        """
        Form a dictionary suitable for JSON.
        :return:
        """
        par_result_dicts = []
        for r in self.par_results:
            par_result_dicts.append(r.to_dict())
        title_result_dicts = []
        for r in self.title_results:
            title_result_dicts.append(r.to_dict())
        return {
            'doc': self.doc_info, 'incomplete': self.incomplete,
            'title_results': title_result_dicts, 'num_title_results': self.get_title_match_count(),
            'par_results': par_result_dicts, 'num_par_results': self.get_par_match_count()}

    def latest_par_result(self):
        """
        Return the previously added par result.
        :return:
        """
        try:
            return self.par_results[len(self.par_results) - 1]
        except IndexError:
            return None

    def get_par_match_count(self):
        """
        Total document count for word matches.
        :return:
        """
        count = 0
        for p in self.par_results:
            count += p.get_match_count()
        return count

    def get_title_match_count(self):
        """
        Total document count for title matches.
        :return:
        """
        count = 0
        for p in self.title_results:
            count += p.get_match_count()
        return count


def in_doc_list(doc_info: DocInfo, docs: List[DocEntry], shorten_list: bool = True):
    """
    Check whether a document is in document entry list.
    :param doc_info: Document to search from the list.
    :param docs: List of documents.
    :param shorten_list: Remove found documents from the list.
    :return: Whether the document is in list.
    """
    for d in docs:
        if doc_info.id is d.id:
            if shorten_list:
                docs.remove(d)
            return True
    return False


@search_routes.route("combinePars")
def create_search_file():
    """
    Combines all TIM-paragraphs into one file.
    :return:
    """
    dir = '/tim_files/pars/'
    file = 'all.log'
    s = subprocess.Popen(f'grep -R "" --include="current" . > all.log 2>&1',
                         cwd=dir,
                         shell=True)
    s.communicate()
    return json_response(f"File created to {dir}{file}")


@search_routes.route("/titles")
def title_search():
    """
    Performs search on document titles.
    :return:
    """
    query = request.args.get('query', '')
    folder = request.args.get('folder', '')
    regex_option = get_option(request, 'regex', default=False, cast=bool)
    case_sensitive = get_option(request, 'caseSensitive', default=False, cast=bool)
    search_exact_words = get_option(request, 'searchExactWords', default=False, cast=bool)
    search_owned_docs = get_option(request, 'searchOwned', default=False, cast=bool)

    results_dicts = []
    word_result_count = 0
    title_result_count = 0
    term_regex = None

    validate_query(query, search_exact_words)

    docs = list(set(get_documents(
        filter_user=get_current_user_object(),
        filter_folder=folder,
        search_recursively=True)))

    if not docs:
        if not folder:
            folder = "root"
        abort(400, f"Folder '{folder}' not found or not accessible")
    if search_owned_docs:
        docs = list(set(docs) - (set(docs) - set(get_documents_by_access_type(AccessType.owner))))
        if not docs:
            abort(400, f"No owned documents found in '{folder}'")

    if case_sensitive:
        flags = re.DOTALL
    else:
        flags = re.DOTALL | re.IGNORECASE
    if regex_option:
        term = query
    else:
        term = re.escape(query)
    if search_exact_words:
        # Picks the term if it's a whole word, only word or separated by comma etc.
        term = fr"(?:^|\W)({term})(?:$|\W)"
    try:
        term_regex = re.compile(term, flags)
    except sre_constants.error as e:
        abort(400, f"Invalid regex: {str(e)}")
    if term_regex:
        for d in docs:
            current_doc = d.path
            try:
                title_result = TitleResult()
                d_title = d.document.docinfo.title
                matches = list(term_regex.finditer(d_title))
                if matches:
                    for m in matches:
                        result = WordResult(match_word=m.group(0),
                                            match_start=m.start(),
                                            match_end=m.end())
                        title_result.add_result(result)
                if title_result.has_results():
                    r = DocResult(d.document.docinfo)
                    r.add_title_result(title_result)
                    results_dicts.append(r.to_dict())
                    title_result_count += r.get_title_match_count()
            except Exception as e:
                log_search_error(f"{str(e.__class__.__name__)}: {str(e)}", query, current_doc, par="")

    return json_response({
        'titleResultCount': title_result_count,
        'wordResultCount': word_result_count,
        'errors': [],
        'incomplete_search_reason': "",
        'results': results_dicts,
    })


def validate_query(query, search_exact_words):
    """
    Abort if query is too short.
    :param query:
    :param search_exact_words:
    :return:
    """
    if len(query.strip()) < MIN_QUERY_LENGTH and not search_exact_words:
        if query.strip().lower() not in WHITE_LIST:
            abort(400, f'Search text must be at least {MIN_QUERY_LENGTH} character(s) long with whitespace stripped.')
    if len(query.strip()) < MIN_EXACT_WORDS_QUERY_LENGTH and search_exact_words:
        abort(400, f'Whole word search text must be at least {MIN_EXACT_WORDS_QUERY_LENGTH} character(s) '
                   f'long with whitespace stripped.')


@search_routes.route("")
@cache.cached(key_prefix=make_cache_key)
def search():
    """
    Perform document word search with a combined par file and grep.
    :return:
    """

    query = request.args.get('query', '')
    folder = request.args.get('folder', '')
    regex_option = get_option(request, 'regex', default=False, cast=bool)
    case_sensitive = get_option(request, 'caseSensitive', default=False, cast=bool)
    search_exact_words = get_option(request, 'searchExactWords', default=False, cast=bool)
    search_owned_docs = get_option(request, 'searchOwned', default=False, cast=bool)
    max_results = get_option(request, 'maxResults', default=100000, cast=int)

    validate_query(query, search_exact_words)

    dir = '/tim_files/pars/'
    grep_flags = ""
    term_regex = None
    owned_docs = []
    # results = []
    incomplete_search_reason = ""
    doc_result = None
    current_doc = ""
    current_par = ""
    output = []
    results_dicts = []
    title_result_count = 0
    word_result_count = 0

    if case_sensitive:
        flags = re.DOTALL
    else:
        grep_flags += "-i "
        flags = re.DOTALL | re.IGNORECASE
    if regex_option:
        term = query
    else:
        grep_flags += "-F "
        term = re.escape(query)
    if search_exact_words:
        # Picks the term if it's a whole word, only word or separated by comma etc.
        grep_flags += "-sw "
        term = fr"(?:^|\W)({term})(?:$|\W)"
    try:
        term_regex = re.compile(term, flags)
    except sre_constants.error as e:
        abort(400, f"Invalid regex: {str(e)}")

    if search_owned_docs:
        owned_docs = get_documents_by_access_type(AccessType.owner)
        if not owned_docs:
            abort(400, f"No owned documents found")

    try:
        cmd = f'grep {grep_flags}"{query}" all.log'
        s = subprocess.Popen(cmd,
                             cwd=dir,
                             stdout=subprocess.PIPE,
                             shell=True)
        output_str = s.communicate()[0].decode('utf-8').strip()
        output = output_str.splitlines()
    except Exception as e:
        abort(400, f"{str(e.__class__.__name__)}: {str(e)}")

    for line in output:
        try:
            if line and len(line) > 10:
                temp = line[2:].split("/", 2)
                doc_id = int(temp[0])
                par_id = temp[1]
                current_doc = doc_id
                current_par = par_id

                doc = DocEntry.find_by_id(doc_id)
                if not doc:
                    raise Exception(f"Unable to find document with id: {doc_id}")
                doc_info = DocEntry.find_by_id(doc_id).document.get_docinfo()
                current_doc = doc_info.path

                # If not allowed to view, continue to the next one.
                if not has_view_access(doc_info):
                    continue

                # If doc isn't in the search path, continue to the next one.
                if not doc_info.path.startswith(folder):
                    continue

                if search_owned_docs:
                    if not in_doc_list(doc_info, owned_docs):
                        continue

                # par = doc_info.document.get_paragraph(par_id)
                # md = par.get_markdown()
                par_result = ParResult(par_id)
                par_info = json.loads(temp[2].replace("current:", "", 1))
                md = par_info['md']
                matches = list(term_regex.finditer(md))
                if matches:
                    for m in matches:
                        result = WordResult(match_word=m.group(0),
                                            match_start=m.start(),
                                            match_end=m.end())
                        par_result.add_result(result)
                    par_result.preview = preview(md, query, matches[0])

                # Create new doc result if first.
                if not doc_result:
                    doc_result = DocResult(doc_info)

                # If not the same doc as previous result line, save non-empty previous result object and create new.
                elif doc_result.doc_info.id != doc_id:
                    if doc_result.has_results():
                        # results.append(doc_result)
                        word_result_count += doc_result.get_par_match_count()
                        results_dicts.append(doc_result.to_dict())  # Save directly as dict to save time.
                    doc_result = DocResult(doc_info)

                # Add the paragraph results to the most recent doc, unless empty.
                if par_result.has_results():
                    doc_result.add_par_result(par_result)
                if word_result_count > max_results:
                    incomplete_search_reason = f"more than maximum of {max_results} results"
                    break

        except Exception as e:
            log_search_error(f"{str(e.__class__.__name__)}: {str(e)}", query, current_doc, par=current_par)

    # TODO: Check the logic here.
    # Since the loop ends before saving the last one, add it.
    if doc_result and doc_result.has_results():
        # results.append(doc_result)
        results_dicts.append(doc_result.to_dict())

    # for r in results:
    #     word_result_count += r.get_par_match_count()
    #     results_dicts.append(r.to_dict())

    return json_response({
        'titleResultCount': title_result_count,
        'wordResultCount': word_result_count,
        'errors': [],
        'incomplete_search_reason': incomplete_search_reason,
        'results': results_dicts,
    })
