"""Routes for searching."""
from flask import Blueprint, render_template

import tim
from documentmodel.docparagraph import DocParagraph
from options import get_option
from .common import *
from .cache import cache

search_routes = Blueprint('search',
                          __name__,
                          url_prefix='/search')


def make_cache_key(*args, **kwargs):
    path = request.path
    return (str(getCurrentUserId()) + path).encode('utf-8')


@search_routes.route('/<query>')
@cache.cached(key_prefix=make_cache_key)
def search(query):
    if len(query.strip()) < 3:
        abort(400, 'Search text must be at least 3 characters long with whitespace stripped.')
    timdb = getTimDb()
    docs = timdb.documents.get_documents()
    viewable = timdb.users.get_viewable_blocks(getCurrentUserId())
    allowed_docs = [doc for doc in docs if doc['id'] in viewable]
    current_user = get_current_user()
    all_texts = []
    all_js = []
    all_css = []
    all_modules = []
    for d in allowed_docs:
        doc = Document(d['id'])
        pars = doc.get_paragraphs()
        found_pars = []
        for t in pars:
            if query.lower() in t.get_markdown().lower():
                found_pars.append(t)
        if not found_pars:
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
    for t in all_texts:
        t['attrs']['rl'] = 'force'
        t['ref_doc_id'] = t['doc_id']
        t['ref_id'] = t['id']
    return render_template('view_html.html',
                           route='search',
                           doc={'id': -1, 'name': 'Search results', 'fullname': 'Search results',
                                'title': 'Search results'},
                           text=all_texts,
                           current_user=current_user,
                           js=all_js,
                           cssFiles=all_css,
                           jsMods=all_modules,
                           group=getCurrentUserGroup(),
                           rights={'editable': False,
                                   'can_mark_as_read': False,
                                   'can_comment': False,
                                   'browse_own_answers': False,
                                   'teacher': False,
                                   'see_answers': False,
                                   'manage': False
                                   },
                           reqs=pluginControl.get_all_reqs(),
                           settings=tim.get_user_settings(),
                           version={'hash': None},
                           translations=None,
                           start_index=None,
                           in_lecture=False,
                           disable_read_markings=True)
