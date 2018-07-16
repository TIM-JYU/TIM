"""Routes for searching."""
import re
import sre_constants
from datetime import datetime

from flask import Blueprint
from flask import abort
from flask import request
from sqlalchemy.orm import joinedload

from timApp.admin import search_in_documents
from timApp.admin.search_in_documents import SearchArgumentsBasic
from timApp.auth.accesshelper import verify_logged_in
from timApp.auth.sessioninfo import get_current_user_id
from timApp.auth.sessioninfo import get_current_user_object
from timApp.document.docentry import DocEntry, get_documents
from timApp.document.docinfo import DocInfo
from timApp.document.docparagraph import DocParagraph
from timApp.folder.folder import Folder
from timApp.item.block import Block
from timApp.item.tag import Tag
from timApp.util.flask.cache import cache
from timApp.util.flask.requesthelper import get_option
from timApp.util.flask.responsehelper import json_response

folder_set = set()

search_routes = Blueprint('search',
                          __name__,
                          url_prefix='/search')


# noinspection PyUnusedLocal
def make_cache_key(*args, **kwargs):
    path = request.path
    return (str(get_current_user_id()) + path + str(request.query_string)).encode('utf-8')


# TODO: Use the classes and functions from search_in_documents.py
class SearchResult:
    def __init__(self, document: DocInfo, par: DocParagraph):
        self.document = document
        self.par = par


@search_routes.route('/getFolders')
def get_subfolders():
    """
    Returns all subfolders and their subfolders.
    :return:
    """
    verify_logged_in()
    get_folders_recursive(request.args.get('folder', ''))
    global folder_set
    return json_response(list(folder_set))


def get_folders_recursive(starting_path: str):
    """
    Recursive function for get_subfolders.
    :param starting_path:
    :return:
    """
    folders = Folder.get_all_in_path(starting_path)
    global folder_set
    if folders:
        for folder in folders:
            folder_set.add(folder)
            get_folders_recursive(folder.path)


@search_routes.route('/tags')
def search_tags():
    """
    Route for document tag search. Returns a list of documents with matching tag in a folder and its subfolders.
    """
    query = request.args.get('query', '')
    case_sensitive = get_option(request, 'case_sensitive', default=False, cast=bool)
    folder = request.args.get('folder', '')
    regex_option = get_option(request, 'regex', default=False, cast=bool)
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
    if case_sensitive:
        regex = re.compile(query if regex_option else re.escape(query), re.DOTALL)
    else:
        regex = re.compile(query if regex_option else re.escape(query), re.DOTALL | re.IGNORECASE)
    for d in docs:
        m_tags = []
        m_num = 0
        for tag in d.block.tags:
            matches = list(regex.finditer(tag.name))
            if matches:
                m_tags.append(tag)
                m_num += len(matches)
        if m_num > 0:
            results.append({'doc': d, 'matching_tags': m_tags, 'num_results': m_num})

    return json_response(results)


@search_routes.route("")
@cache.cached(key_prefix=make_cache_key)
def search():
    """
    Route for document word and title searches.
    :return:
    """

    verify_logged_in()

    query = request.args.get('query', '')
    if len(query.strip()) < 2:
        abort(400, 'Search text must be at least 2 characters long with whitespace stripped.')

    folder = request.args.get('folder', '')
    regex_option = get_option(request, 'regex', default=False, cast=bool)
    case_sensitive = get_option(request, 'caseSensitive', default=False, cast=bool)
    onlyfirst = get_option(request, 'onlyfirst', default=9999, cast=int)
    ignore_plugins_settings = get_option(request, 'ignorePluginsSettings', default=False, cast=bool)
    search_doc_names = get_option(request, 'searchDocNames', default=False, cast=bool)
    search_words = get_option(request, 'searchWords', default=True, cast=bool)
    current_user = get_current_user_object()

    # Won't search subfolders if search_recursively isn't true.
    docs = get_documents(filter_user=current_user, filter_folder=folder, search_recursively=True)
    results = []
    args = SearchArgumentsBasic(
        term=query,
        regex=regex_option,
        onlyfirst=onlyfirst,
        case_sensitive=case_sensitive,
        format="")

    # TODO: regex doesn't work consistently (ki*a can't find some things kis*a does)?
    try:
        regex = re.compile(args.term if args.regex else re.escape(args.term), re.DOTALL | re.IGNORECASE)
        if args.case_sensitive:
            regex = re.compile(args.term if args.regex else re.escape(args.term), re.DOTALL)
        for d in docs:
            doc_info = d.document.docinfo
            if search_doc_names:
                d_title = doc_info.title
                matches = list(regex.finditer(d_title))
                if matches:
                    for m in matches:
                        result = {'doc': doc_info,
                                  'par': None,
                                  'match_word': m.group(0),
                                  'match_start_index': m.start(),
                                  'match_end_index': m.end(),
                                  'num_results': len(matches),
                                  'num_pars': 0,
                                  'num_pars_found': 0,
                                  'in_title': True}
                        results.append(result)
            if search_words:
                for r in search_in_documents.search(d=doc_info, args=args, use_exported=False):
                    result = {'doc': r.doc,
                              'par': r.par,
                              'match_word': r.match.group(0),
                              'match_start_index': r.match.span()[0],
                              'match_end_index': r.match.span()[1],
                              'num_results': r.num_results,
                              'num_pars': r.num_pars,
                              'num_pars_found': r.num_pars_found,
                              'in_title': False}
                    # Don't return results within plugins if no edit access to the document.
                    if r.par.is_setting() or r.par.is_plugin():
                        if get_current_user_object().has_edit_access(d) and not ignore_plugins_settings:
                            results.append(result)
                    else:
                        results.append(result)
    except sre_constants.error:
        abort(400, "Invalid regex")
    else:
        return json_response(results)
