from flask import Blueprint
from flask import abort
from flask import request

from timApp.accesshelper import verify_comment_right, verify_logged_in, has_ownership
from timApp.accesshelper import verify_view_access
from timApp.dbaccess import get_timdb
from timApp.documentmodel.document import Document
from timApp.requesthelper import get_referenced_pars_from_req, verify_json_params
from timApp.responsehelper import json_response
from timApp.routes.edit import par_response
from timApp.routes.notify import notify_doc_watchers
from timApp.sessioninfo import get_current_user_group
from timApp.timdb.models.docentry import DocEntry
from timApp.timdb.models.notification import NotificationType
from timApp.timdb.timdbexception import TimDbException

notes = Blueprint('notes',
                  __name__,
                  url_prefix='')

KNOWN_TAGS = ['difficult', 'unclear']


@notes.route("/note/<int:note_id>")
def get_note(note_id):
    timdb = get_timdb()
    note = timdb.notes.get_note(note_id)
    if not note:
        abort(404)
    if not (timdb.notes.has_edit_access(get_current_user_group(), note_id) or has_ownership(note['doc_id'])):
        abort(403)
    note.pop('usergroup_id')
    tags = note['tags']
    note['tags'] = {}
    for tag in KNOWN_TAGS:
        note['tags'][tag] = tag in tags
    return json_response({'text': note['content'], 'extraData': note})


@notes.route("/postNote", methods=['POST'])
def post_note():
    note_text, access, doc_id, par_id = verify_json_params('text', 'access', 'docId', 'par')
    sent_tags, = verify_json_params('tags', require=False, default={})
    tags = []
    for tag in KNOWN_TAGS:
        if sent_tags.get(tag):
            tags.append(tag)
    docentry = DocEntry.find_by_id(doc_id, try_translation=True)
    verify_comment_right(doc_id)
    doc = Document(doc_id)
    try:
        par = doc.get_paragraph(par_id)
    except TimDbException as e:
        return abort(404, str(e))
    timdb = get_timdb()
    group_id = get_current_user_group()

    if par.get_attr('r') != 'tr':
        par = get_referenced_pars_from_req(par)[0]

    timdb.notes.add_note(group_id, Document(par.get_doc_id()), par, note_text, access, tags)

    if access == "everyone":
        notify_doc_watchers(docentry,
                            note_text,
                            NotificationType.CommentAdded,
                            par)
    return par_response([doc.get_paragraph(par_id)],
                        doc)


@notes.route("/editNote", methods=['POST'])
def edit_note():
    verify_logged_in()
    jsondata = request.get_json()
    group_id = get_current_user_group()
    doc_id = int(jsondata['docId'])
    verify_view_access(doc_id)
    note_text = jsondata['text']
    access = jsondata['access']
    par_id = jsondata['par']
    docentry = DocEntry.find_by_id(doc_id, try_translation=True)
    par = docentry.document.get_paragraph(par_id)
    note_id = int(jsondata['id'])
    sent_tags = jsondata.get('tags', {})
    tags = []
    for tag in KNOWN_TAGS:
        if sent_tags.get(tag):
            tags.append(tag)
    timdb = get_timdb()
    if not (timdb.notes.has_edit_access(group_id, note_id) or has_ownership(doc_id)):
        abort(403, "Sorry, you don't have permission to edit this note.")
    timdb.notes.modify_note(note_id, note_text, access, tags)

    if access == "everyone":
        notify_doc_watchers(docentry,
                            note_text,
                            NotificationType.CommentModified,
                            par)
    doc = Document(doc_id)
    return par_response([doc.get_paragraph(par_id)],
                        doc)


@notes.route("/deleteNote", methods=['POST'])
def delete_note():
    group_id = get_current_user_group()
    doc_id, note_id, paragraph_id = verify_json_params('docId', 'id', 'par')
    timdb = get_timdb()
    if not (timdb.notes.has_edit_access(group_id, note_id) or has_ownership(doc_id)):
        abort(403, "Sorry, you don't have permission to remove this note.")
    timdb.notes.delete_note(note_id)
    doc = Document(doc_id)
    return par_response([doc.get_paragraph(paragraph_id)],
                        doc)
