"""Routes for searching."""
import re
import sre_constants
import time
from datetime import datetime
from typing import Generator, Match

from flask import Blueprint
from flask import abort
from flask import request
from sqlalchemy.orm import joinedload

from timApp.admin.search_in_documents import SearchArgumentsBasic, SearchResult
from timApp.admin.util import enum_pars
from timApp.auth.accesshelper import verify_logged_in
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
def search_tags():
    """
    A route for document tag search.
    """
    verify_logged_in()

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


def log_search_error(error: str, query: str, path: str, tag: str = "", par: str = "") -> None:
    """
    Forms an error report and sends it to timLog.
    :param error: The error's message
    :param query: Search word.
    :param path: Document path.
    :param tag: Tag name.
    :param par: Par id.
    :return: None.
    """
    if not error:
        error = "Unknown error"
    common_part = f"'{error}' while searching '{query}' in document {path}"
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


def preview(par, query, m: Match[str], snippet_length=PREVIEW_LENGTH, max_length=PREVIEW_MAX_LENGTH):
    """
    Forms preview of the match paragraph.
    :param par: Paragraph to preview.
    :param query: Search word.
    :param m: Match object.
    :param snippet_length: The lenght of preview before and after search word.
    :param max_length: The maximum allowed length of the preview.
    :return: Preview with set amount of characters around search word.
    """
    text = par.get_markdown()
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
    if end_index > len(text):
        end_index = len(text)
        postfix = "..."
    return prefix + text[start_index:end_index] + postfix


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


@search_routes.route("")
@cache.cached(key_prefix=make_cache_key)
def search():
    """
    Route for document word and title searches.
    :return:
    """
    verify_logged_in()
    current_user = get_current_user_object()
    error_list = []  # List of errors during search.
    incomplete_search_reason = ""  # Tells why some results were left out.

    query = request.args.get('query', '')
    folder = request.args.get('folder', '')
    regex_option = get_option(request, 'regex', default=False, cast=bool)
    case_sensitive = get_option(request, 'caseSensitive', default=False, cast=bool)

    # Limit how many pars are searched from any document. The rest are skipped.
    max_doc_pars = get_option(request, 'maxDocPars', default=1000, cast=int)
    # Limit number of found search results after which the search will end.
    # If number of results is very high, just showing them will crash the search.
    max_results_total = get_option(request, 'maxTotalResults', default=10000, cast=int)
    # Time limit for the searching process.
    max_time = get_option(request, 'maxTime', default=15, cast=int)
    # Limit the number of results per document.
    max_results_doc = get_option(request, 'maxDocResults', default=100, cast=int)
    # Don't search paragraphs that are marked as plugin or setting.
    ignore_plugins_settings = get_option(request, 'ignorePluginsSettings', default=False, cast=bool)
    # Only search documents that have current user as owner.
    search_owned_docs = get_option(request, 'searchOwned', default=False, cast=bool)
    # Don't create more than # previews per document.
    max_previews = get_option(request, 'maxPreviews', default=10, cast=int)

    search_doc_names = get_option(request, 'searchDocNames', default=False, cast=bool)
    search_exact_words = get_option(request, 'searchExactWords', default=False, cast=bool)
    search_words = get_option(request, 'searchWords', default=True, cast=bool)

    if len(query.strip()) < MIN_QUERY_LENGTH and not search_exact_words:
        if query.strip().lower() not in WHITE_LIST:
            abort(400, f'Search text must be at least {MIN_QUERY_LENGTH} character(s) long with whitespace stripped.')
    if len(query.strip()) < MIN_EXACT_WORDS_QUERY_LENGTH and search_exact_words:
        abort(400, f'Whole word search text must be at least {MIN_EXACT_WORDS_QUERY_LENGTH} character(s) '
                   f'long with whitespace stripped.')

    docs = list(set(get_documents(filter_user=current_user, filter_folder=folder, search_recursively=True)))
    if not docs:
        abort(400, f"Folder '{folder}' not found or not accessible")
    if search_owned_docs:
        docs = list(set(docs) - (set(docs) - set(get_documents_by_access_type(AccessType.owner))))
        if not docs:
            abort(400, f"No owned documents found in '{folder}'")
    results = []
    args = SearchArgumentsBasic(
        term=query,
        regex=regex_option,
        onlyfirst=max_doc_pars,
        format="")

    if case_sensitive:
        flags = re.DOTALL
    else:
        flags = re.DOTALL | re.IGNORECASE
    if args.regex:
        term = args.term
    else:
        term = re.escape(args.term)
    if search_exact_words:
        # Picks the term if it's a whole word, only word or separated by comma etc.
        term = fr"(?:^|\W)({term})(?:$|\W)"

    starting_time = time.clock()
    current_doc = "before search"
    current_par = "before search"
    try:
        term_regex = re.compile(term, flags)
        for d in docs:
            d_words_count = 0
            current_doc = d.path
            doc_info = d.document.docinfo
            doc_result = DocResult(doc_info)
            try:
                # Cut search before timeout.
                if (time.clock() - starting_time) > max_time:
                    incomplete_search_reason = f"search timeout after {max_time} seconds"
                    break
                # If results are too large, MemoryError occurs.
                if len(results) > max_results_total:
                    incomplete_search_reason = f"maximum of {max_results_total} total results reached"
                    break

                if search_doc_names:
                    title_result = TitleResult()
                    d_title = doc_info.title
                    matches = list(term_regex.finditer(d_title))
                    if matches:
                        for m in matches:
                            result = WordResult(match_word=m.group(0),
                                                match_start=m.start(),
                                                match_end=m.end())
                            title_result.add_result(result)
                    if title_result.has_results():
                        doc_result.add_title_result(title_result)

                if search_words:
                    current_par = ""
                    for r in search_in_doc(d=doc_info, term_regex=term_regex, args=args, use_exported=False,
                                           ignore_plugins_settings=ignore_plugins_settings):
                        current_par = r.par.dict()['id']

                        # If results per document limit is reached, move on to the next doc.
                        if d_words_count >= max_results_doc:
                            incomplete_search_reason = f"one or more documents have more than the maximum of " \
                                                       f"{max_results_doc} results"
                            doc_result.incomplete = True
                            break

                        d_words_count += 1
                        word_result = WordResult(match_word=r.match.group(0),
                                                 match_start=r.match.start(),
                                                 match_end=r.match.end())

                        # Don't return results within plugins if user has no edit access to the document.
                        if (r.par.is_setting() or r.par.is_plugin()) and \
                                (not get_current_user_object().has_edit_access(d)):
                            break
                        else:
                            # If previous search result was in same par, add this result to its object.
                            previous_par_result = doc_result.latest_par_result()
                            if previous_par_result and previous_par_result.par_id is current_par:
                                previous_par_result.add_result(word_result)
                            # Otherwise create a new entry to store the result.
                            else:
                                if len(doc_result.par_results) > max_previews:
                                    par_preview = ""
                                else:
                                    par_preview = preview(r.par, query, r.match)
                                par_result = ParResult(par_id=current_par, preview=par_preview)
                                par_result.add_result(word_result)
                                doc_result.add_par_result(par_result)

            # If error happens inside loop, report and continue.
            except Exception as e:
                error = "Unknown error"
                try:
                    error = f"{str(e.__class__.__name__)}: {str(e)}"
                    doc_result.incomplete = True
                    incomplete_search_reason = error
                    # Try adding the result despite error.
                    if doc_result.has_results():
                        results.append(doc_result)
                except:
                    pass
                finally:
                    error_list.append({
                        'error': f"{error}",
                        'doc_path': current_doc,
                        'par_id': current_par
                    })
                    log_search_error(error, query, current_doc, par=current_par)
            else:
                if doc_result.has_results():
                    results.append(doc_result)
    except sre_constants.error as e:
        abort(400, f"Invalid regex: {str(e)}")
    except Exception as e:
        abort(400, f"{str(e.__class__.__name__)}: {str(e)}")
    else:
        try:
            clean_results = []
            # Remove results that would break JSON-formatting.
            title_result_count = 0
            word_result_count = 0
            for r in results:
                title_result_count += r.get_title_match_count()
                word_result_count += r.get_par_match_count()
                try:
                    clean_r = r.to_dict()
                    json_response(clean_r)
                except TypeError as e:
                    # Report the offending paragraph.
                    error = f"Formatting JSON-response for a result failed: {e}"
                    log_search_error(error, query, r.doc_info.path, par="")
                    incomplete_search_reason = f"error in formatting JSON-response in document '{r.doc_info.path}'"
                else:
                    clean_results.append(clean_r)
            return json_response({
                'titleResultCount': title_result_count,
                'wordResultCount': word_result_count,
                'errors': error_list,
                'incomplete_search_reason': incomplete_search_reason,
                'results': clean_results,
            })
        except MemoryError:
            abort(400, f"MemoryError: results too large")
        except Exception as e:
            abort(400, f"Error encountered while formatting JSON-response: {e}")


def search_in_doc(d: DocInfo, term_regex, args: SearchArgumentsBasic, use_exported: bool,
                  ignore_plugins_settings=False) -> Generator[SearchResult, None, None]:
    """
    Performs a search operation for the specified document, yielding SearchResults.
    Copied and edited from a search_in_documents function with following changes:
     - term_regex formed outside of the function
     - ignore plugins and settings option added

    :param args: The search arguments.
    :param d: The document to process.
    :param term_regex: Regex formatted before calling.
    :param use_exported: Whether to search in the exported form of paragraphs.
    :param ignore_plugins_settings: Skip plugin and setting pars.
    """
    results_found = 0
    pars_processed = 0
    pars_found = 0
    for d, p in enum_pars(d):
        pars_processed += 1
        if ignore_plugins_settings and (p.is_setting() or p.is_plugin()):
            continue
        md = p.get_exported_markdown(skip_tr=True) if use_exported else p.get_markdown()
        matches = set(term_regex.finditer(md))
        if matches:
            pars_found += 1
            for m in matches:
                results_found += 1
                yield SearchResult(
                    doc=d,
                    par=p,
                    num_results=results_found,
                    num_pars=pars_processed,
                    num_pars_found=pars_found,
                    match=m,
                )
        if args.onlyfirst and pars_processed >= args.onlyfirst:
            break
