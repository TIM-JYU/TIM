"""Routes for searching."""
from flask import Blueprint, render_template
from flask import abort
from flask import request
from typing import Set

from timApp.accesshelper import verify_logged_in
from timApp.cache import cache
from timApp.common import post_process_pars, get_user_settings
from timApp.dbaccess import get_timdb
from timApp.documentmodel.docparagraph import DocParagraph
from timApp.pluginControl import get_all_reqs
from timApp.requesthelper import get_option
from timApp.sessioninfo import get_current_user_object, get_current_user_id, logged_in, get_current_user_group
from timApp.timdb.docinfo import DocInfo
from timApp.timdb.models.docentry import DocEntry
from timApp.timdb.tim_models import BlockAccess
from timApp.timdb.userutils import get_viewable_blocks

search_routes = Blueprint('search',
                          __name__,
                          url_prefix='/search')


# noinspection PyUnusedLocal
def make_cache_key(*args, **kwargs):
    path = request.path
    return (str(get_current_user_id()) + path + str(request.query_string)).encode('utf-8')


class SearchResult:
    def __init__(self, document: DocInfo, par: DocParagraph):
        self.document = document
        self.par = par


@search_routes.route('/<query>')
@cache.cached(key_prefix=make_cache_key)
def search(query):
    verify_logged_in()
    if len(query.strip()) < 3:
        abort(400, 'Search text must be at least 3 characters long with whitespace stripped.')
    timdb = get_timdb()
    show_full_pars = get_option(request, 'show_pars', False)
    max_results = get_option(request, 'max', 100)
    viewable = get_viewable_blocks(get_current_user_id())
    docs = timdb.documents.get_documents(filter_ids=viewable)
    current_user = get_current_user_object()
    all_texts = []
    all_js = []
    all_css = []
    all_modules = []
    results: Set[SearchResult] = set()
    query_lower = query.lower()
    found_docs: Set[DocInfo] = set()
    for d in docs:
        doc = d.document
        pars = doc.get_paragraphs()
        found_pars = []
        for t in pars:
            if query_lower in t.get_markdown().lower():
                found_pars.append(t)
                if d in found_docs:
                    continue
                found_docs.add(d)
                results.add(SearchResult(d, t))
        if not found_pars:
            continue
        if len(results) >= max_results:
            break
        if not show_full_pars:
            continue
        DocParagraph.preload_htmls(pars, doc.get_settings())
        pars, js_paths, css_paths, modules = post_process_pars(doc,
                                                               found_pars,
                                                               current_user if logged_in() else None,
                                                               sanitize=False,
                                                               do_lazy=get_option(request, "lazy", True),
                                                               load_plugin_states=False)
        all_texts.extend(pars)
        for j in js_paths:
            all_js.append(j)
        for c in css_paths:
            all_css.append(c)
        for m in modules:
            all_modules.append(m)
        if len(all_texts) > 500:
            break
    if show_full_pars:
        for t in all_texts:
            if not t.get('attrs'):
                t['attrs'] = {}
            t['attrs']['rl'] = 'force'
            t['ref_doc_id'] = t['doc_id']
            t['ref_id'] = t['id']
        return render_template('view_html.html',
                               access=BlockAccess(),
                               route='search',
                               item=DocEntry.get_dummy('Search results'),
                               text=all_texts,
                               js=all_js,
                               cssFiles=all_css,
                               jsMods=all_modules,
                               group=get_current_user_group(),
                               reqs=get_all_reqs(),
                               settings=get_user_settings(),
                               version={'hash': None},
                               translations=None,
                               start_index=None,
                               in_lecture=False,
                               disable_read_markings=True,
                               no_browser=get_option(request, "noanswers", False),
                               doc_settings_dict={})
    results = list(results)
    results.sort(key=lambda r: r.document.path)
    return render_template('search.html',
                           query=query,
                           results=results,
                           too_many=len(results) >= max_results)
