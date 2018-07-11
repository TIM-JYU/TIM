"""Routes for searching."""
from typing import Set

from flask import Blueprint, render_template
from flask import abort
from flask import request

from timApp.admin import search_in_documents
from timApp.admin.search_in_documents import SearchArgumentsBasic
from timApp.auth.accesshelper import verify_logged_in
from timApp.util.flask.cache import cache
from timApp.document.post_process import post_process_pars
from timApp.document.docparagraph import DocParagraph
from timApp.document.docsettings import DocSettings
from timApp.document.document import Document
from timApp.plugin.pluginControl import get_all_reqs
from timApp.util.flask.requesthelper import get_option
from timApp.auth.sessioninfo import get_current_user_object, get_current_user_id, logged_in, get_current_user_group, \
    get_user_settings
from timApp.document.docinfo import DocInfo
from timApp.document.docentry import DocEntry, get_documents
from timApp.auth.auth_models import BlockAccess
from timApp.util.flask.responsehelper import json_response

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


@search_routes.route("")
@cache.cached(key_prefix=make_cache_key)
def search():
    # verify_logged_in()

    query = request.args.get('query', '')
    if len(query.strip()) < 3:
        abort(400, 'Search text must be at least 3 characters long with whitespace stripped.')

    folder = request.args.get('folder', '')
    regex = get_option(request, 'regex', default=False, cast=bool)
    case_sensitive = get_option(request, 'caseSensitive', default=False, cast=bool)
    onlyfirst = get_option(request, 'onlyfirst', default=999, cast=int)
    ignore_plugins_settings = get_option(request, 'ignorePluginsSettings', default=False, cast=bool)
    current_user = get_current_user_object()

    # TODO: Regex in these doesn't work.
    ignore_list = ['%templates/%', '%/preamble%']

    docs = get_documents(filter_user=current_user, filter_folder=folder, custom_filter=DocEntry.name.notin_(ignore_list))
    results = []

    args = SearchArgumentsBasic(
        term=query,
        regex=regex,
        onlyfirst=onlyfirst,
        case_sensitive=case_sensitive,
        format="")

    for d in docs:
        doc_info = d.document.docinfo
        for r in search_in_documents.search(d=doc_info, args=args, use_exported=False):
            result = {'doc': r.doc,
                      'par': r.par,
                      'match_word': r.match.group(),
                      'match_start_index': r.match.span()[0],
                      'match_end_index': r.match.span()[1],
                      'num_results': r.num_results,
                      'num_pars': r.num_pars,
                      'num_pars_found': r.num_pars_found}

            # Don't return results within plugins if no edit access to the document.
            if r.par.is_setting() or r.par.is_plugin():
                if get_current_user_object().has_edit_access(d) and not ignore_plugins_settings:
                    results.append(result)
            else:
                results.append(result)
    return json_response(results)
