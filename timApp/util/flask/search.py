"""Routes for searching."""
import re
import sre_constants
import time
from datetime import datetime
from typing import Generator

from flask import Blueprint
from flask import abort
from flask import request
from sqlalchemy.orm import joinedload

from timApp.admin.search_in_documents import SearchArgumentsBasic, SearchResult
from timApp.admin.util import enum_pars
from timApp.auth.accesshelper import verify_logged_in
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

search_routes = Blueprint('search',
                          __name__,
                          url_prefix='/search')

WHITE_LIST = ["c#"]
MIN_QUERY_LENGTH = 3
MIN_EXACT_WORDS_QUERY_LENGTH = 1


# noinspection PyUnusedLocal
def make_cache_key(*args, **kwargs):
    path = request.path
    return (str(get_current_user_id()) + path + str(request.query_string)).encode('utf-8')


@search_routes.route('/getFolders')
def get_subfolders():
    """
    Returns all subfolders and their subfolders.
    :return:
    """
    verify_logged_in()
    folder_set = set()
    # get_folders_recursive(request.args.get('folder', ''), folder_set)
    get_folders_three_levels(request.args.get('folder', ''), folder_set)
    return json_response(list(folder_set))


def get_folders_three_levels(starting_path: str, folder_set):
    """
    Limited folder search with depth of three steps from the root:
    root/level_1/level_2/level_3
    :param starting_path:
    :param folder_set:
    :return:
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


def get_folders_recursive(starting_path: str, folder_set):
    """
    Recursive function for get_subfolders.
    :param starting_path:
    :param folder_set:
    :return:
    """
    folders = Folder.get_all_in_path(starting_path)
    if folders:
        for folder in folders:
            folder_set.add(folder.path)
            get_folders_recursive(folder.path, folder_set)


@search_routes.route('/tags')
def search_tags():
    """
    Route for document tag search. Returns a list of documents with matching tag in a folder and its subfolders.
    """
    verify_logged_in()

    query = request.args.get('query', '')
    case_sensitive = get_option(request, 'caseSensitive', default=False, cast=bool)
    folder = request.args.get('folder', '')
    regex_option = get_option(request, 'regex', default=False, cast=bool)
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
                error_list.append({
                    'error': f"{str(e.__class__.__name__)}: {str(e)}",
                    'doc_path': current_doc,
                    'tag_name': current_tag
                })
    except sre_constants.error as e:
        abort(400, f"Invalid regex: {str(e)}")
    except Exception as e:
        abort(400, f"{str(e.__class__.__name__)}: {str(e)}")
    else:
        try:
            return json_response({'results': results,
                                  'complete': True,
                                  'tagResultCount': tag_result_count,
                                  'errors': error_list
                                  })
        except MemoryError:
            abort(400, "MemoryError: results too large")
        except Exception as e:
            abort(400, f"Error encountered while formatting JSON-response: {e}")


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

    query = request.args.get('query', '')
    folder = request.args.get('folder', '')
    regex_option = get_option(request, 'regex', default=False, cast=bool)
    case_sensitive = get_option(request, 'caseSensitive', default=False, cast=bool)

    # Limit how many pars are searched from any document. The rest are skipped.
    max_doc_pars = get_option(request, 'maxDocPars', default=1000, cast=int)
    # Limit number of found search results after which the search will end.
    max_results_total = get_option(request, 'maxTotalResults', default=100000, cast=int)
    # Time limit for the searching process.
    max_time = get_option(request, 'maxTime', default=10, cast=int)
    # Limit the number of results per document.
    max_results_doc = get_option(request, 'maxDocResults', default=100, cast=int)
    # Don't search paragraphs that are marked as plugin or setting.
    ignore_plugins_settings = get_option(request, 'ignorePluginsSettings', default=False, cast=bool)

    search_doc_names = get_option(request, 'searchDocNames', default=False, cast=bool)
    search_exact_words = get_option(request, 'searchExactWords', default=False, cast=bool)
    search_words = get_option(request, 'searchWords', default=True, cast=bool)

    if len(query.strip()) < MIN_QUERY_LENGTH and not search_exact_words:
        if query.strip().lower() not in WHITE_LIST:
            abort(400, f'Search text must be at least {MIN_QUERY_LENGTH} character(s) long with whitespace stripped.')
    if len(query.strip()) < MIN_EXACT_WORDS_QUERY_LENGTH and search_exact_words:
        abort(400, f'Whole word search text must be at least {MIN_EXACT_WORDS_QUERY_LENGTH} character(s) '
                   f'long with whitespace stripped.')

    # Won't search subfolders if search_recursively isn't true.
    docs = get_documents(filter_user=current_user, filter_folder=folder, search_recursively=True)
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
        # Picks the term if it's a whole word, the only word or separated by comma etc.
        term = fr"(?:^|\W)({term})(?:$|\W)"

    starting_time = time.clock()
    current_doc = "before search"
    current_par = "before search"
    complete = True  # Whether the search was complete or ended without searching everything.
    title_result_count = 0
    word_result_count = 0
    try:
        regex = re.compile(term, flags)
        for d in docs:
            try:
                current_doc = d.path
                doc_info = d.document.docinfo
                # print(time.clock() - starting_time)
                if (time.clock() - starting_time) > max_time:
                    complete = False
                    break
                # If results are too large, MemoryError occurs.
                if len(results) > max_results_total:
                    complete = False
                    break
                if search_doc_names:
                    d_title = doc_info.title
                    matches = list(regex.finditer(d_title))
                    if matches:
                        for m in matches:
                            title_result_count += 1
                            result = {'doc': doc_info,
                                      'par': None,
                                      'match_word': m.group(0),
                                      'match_start_index': m.start(),
                                      'match_end_index': m.end(),
                                      'in_title': True}
                            results.append(result)
                if search_words:
                    d_words_count = 0
                    for r in search_in_doc(d=doc_info, regex=regex, args=args, use_exported=False,
                                           ignore_plugins=ignore_plugins_settings):
                        current_par = r.par.dict()['id']
                        # Limit matches per document to get diverse document results faster.
                        if d_words_count >= max_results_doc:
                            complete = False
                            continue
                        d_words_count += 1
                        word_result_count += 1
                        result = {'doc': r.doc,
                                  'par': r.par,
                                  'match_word': r.match.group(0),
                                  'match_start_index': r.match.span()[0],
                                  'match_end_index': r.match.span()[1],
                                  'in_title': False}
                        # Don't return results within plugins if no edit access to the document.
                        if r.par.is_setting() or r.par.is_plugin():
                            if get_current_user_object().has_edit_access(d) and not ignore_plugins_settings:
                                results.append(result)
                        else:
                            results.append(result)
            # If error happens inside loop, report and continue.
            except Exception as e:
                try:
                    error_list.append({
                        'error': f"{str(e.__class__.__name__)}: {str(e)}",
                        'doc_path': current_doc,
                        'par_id': current_par
                    })
                except:
                    error_list.append({
                        'error': "Unknown error",
                        'doc_path': current_doc,
                        'par_id': current_par
                    })
    except sre_constants.error as e:
        abort(400, f"Invalid regex: {str(e)}")
    except Exception as e:
        abort(400, f"{str(e.__class__.__name__)}: {str(e)}")
    else:
        try:
            clean_results = []
            # Remove results that would break JSON-formatting.
            for r in results:
                try:
                    json_response(r)
                except TypeError as e:
                    # Report the offending paragraph.
                    try:
                        path = r['doc'].path
                    except:
                        path = ""
                    try:
                        id = r['par'].dict()['id']
                    except:
                        id = ""
                    error_list.append({
                        'error': f"Formatting JSON-response for a search result failed: {e}",
                        'doc_path': path,
                        'par_id': id
                    })
                else:
                    clean_results.append(r)
            return json_response({
                'results': clean_results,
                'complete': complete,
                'titleResultCount': title_result_count,
                'wordResultCount': word_result_count,
                'errors': error_list
            })
        except MemoryError:
            abort(400, f"MemoryError: results too large")
        except Exception as e:
            abort(400, f"Error encountered while formatting JSON-response: {e}")


def search_in_doc(d: DocInfo, regex, args: SearchArgumentsBasic, use_exported: bool,
                  ignore_plugins: bool = False) -> Generator[SearchResult, None, None]:
    """
    Performs a search operation for the specified document, yielding SearchResults.

    :param args: The search arguments.
    :param d: The document to process.
    :param regex: Regex formatted before calling.
    :param use_exported: Whether to search in the exported form of paragraphs.
    :param ignore_plugins: Whether to search paragraphs marked as plugin or setting.
    """
    results_found = 0
    pars_processed = 0
    pars_found = 0
    for d, p in enum_pars(d):
        if ignore_plugins and (p.is_setting() or p.is_plugin()):
            continue
        pars_processed += 1
        md = p.get_exported_markdown(skip_tr=True) if use_exported else p.get_markdown()
        matches = set(regex.finditer(md))
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
