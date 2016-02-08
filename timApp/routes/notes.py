from flask import Blueprint, abort, request

from documentmodel.document import Document
from routes.common import *
from routes.edit import par_response

notes = Blueprint('notes',
                  __name__,
                  url_prefix='')

KNOWN_TAGS = ['difficult', 'unclear']


@notes.route("/note/<int:note_id>")
def get_note(note_id):
    timdb = getTimDb()
    note = timdb.notes.get_note(note_id)
    if not (timdb.notes.hasEditAccess(getCurrentUserGroup(), note_id) or timdb.users.userIsOwner(getCurrentUserId(),
                                                                                                 note['doc_id'])):
        abort(403)
    note.pop('UserGroup_id')
    tags = note['tags']
    note['tags'] = {}
    for tag in KNOWN_TAGS:
        note['tags'][tag] = tag in tags
    return jsonResponse({'text': note['content'], 'extraData': note})


@notes.route("/postNote", methods=['POST'])
def post_note():
    jsondata = request.get_json()
    note_text = jsondata['text']
    access = jsondata['access']
    sent_tags = jsondata.get('tags', {})
    tags = []
    for tag in KNOWN_TAGS:
        if sent_tags.get(tag):
            tags.append(tag)
    doc_id = jsondata['docId']
    par_id = jsondata['par']
    verify_comment_right(doc_id)
    doc = Document(doc_id)
    par = doc.get_paragraph(par_id)
    if par is None:
        abort(400, 'Non-existent paragraph')
    timdb = getTimDb()
    group_id = getCurrentUserGroup()

    if par.get_attr('r') != 'tr':
        par = get_referenced_pars_from_req(par)[0]

    timdb.notes.addNote(group_id, Document(par.get_doc_id()), par, note_text, access, tags)
    notify_doc_owner(doc_id, '{} has posted a note on your document "{}":\n\n{}'.format(
        getCurrentUserName(), timdb.documents.get_first_document_name(doc_id), note_text))

    return par_response([doc.get_paragraph(par_id)],
                        doc)


@notes.route("/editNote", methods=['POST'])
def edit_note():
    verifyLoggedIn()
    jsondata = request.get_json()
    group_id = getCurrentUserGroup()
    doc_id = int(jsondata['docId'])
    verify_view_access(doc_id, getCurrentUserGroup())
    note_text = jsondata['text']
    access = jsondata['access']
    par_id = jsondata['par']
    note_id = int(jsondata['id'])
    sent_tags = jsondata.get('tags', {})
    tags = []
    for tag in KNOWN_TAGS:
        if sent_tags[tag]:
            tags.append(tag)
    timdb = getTimDb()
    if not (timdb.notes.hasEditAccess(group_id, note_id) or timdb.users.userIsOwner(getCurrentUserId(), doc_id)):
        abort(403, "Sorry, you don't have permission to edit this note.")
    timdb.notes.modifyNote(note_id, note_text, access, tags)
    notify_doc_owner(doc_id, '{} has edited a note on your document "{}":\n\n{}'.format(
        getCurrentUserName(), timdb.documents.get_first_document_name(doc_id), note_text))

    doc = Document(doc_id)
    return par_response([doc.get_paragraph(par_id)],
                        doc)


@notes.route("/deleteNote", methods=['POST'])
def delete_note():
    jsondata = request.get_json()
    group_id = getCurrentUserGroup()
    doc_id = int(jsondata['docId'])
    note_id = int(jsondata['id'])
    paragraph_id = jsondata['par']
    timdb = getTimDb()
    if not (timdb.notes.hasEditAccess(group_id, note_id) or timdb.users.userIsOwner(getCurrentUserId(), doc_id)):
        abort(403, "Sorry, you don't have permission to remove this note.")
    timdb.notes.deleteNote(note_id)
    doc = Document(doc_id)
    return par_response([doc.get_paragraph(paragraph_id)],
                        doc)
