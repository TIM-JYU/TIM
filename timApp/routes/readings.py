from flask import Blueprint, abort
from flask import current_app
from sqlalchemy.exc import IntegrityError

from documentmodel.document import Document
from routes.common import verify_read_marking_right, getTimDb, jsonResponse, getCurrentUserGroup, \
    get_referenced_pars_from_req, okJsonResponse, get_session_usergroup_ids
from timdb.readparagraphtype import ReadParagraphType
from timdb.timdbexception import TimDbException

readings = Blueprint('readings',
                     __name__,
                     url_prefix='')


@readings.route("/read/<int:doc_id>", methods=['GET'])
def get_read_paragraphs(doc_id):
    verify_read_marking_right(doc_id)
    timdb = getTimDb()
    doc = Document(doc_id)
    # TODO: Get the intersection of read markings of all session users
    readings = timdb.readings.get_readings(getCurrentUserGroup(), doc)
    return jsonResponse(readings)


@readings.route("/read/<int:doc_id>/<specifier>/<int:read_type>", methods=['PUT'])
def set_read_paragraph(doc_id, specifier, read_type):
    paragraph_type = ReadParagraphType(read_type)
    if current_app.config['DISABLE_AUTOMATIC_READINGS'] and paragraph_type in (ReadParagraphType.on_screen,
                                                                               ReadParagraphType.hover_par):
        return okJsonResponse()
    verify_read_marking_right(doc_id)
    timdb = getTimDb()

    doc = Document(doc_id)
    try:
        par = doc.get_paragraph(specifier)
    except TimDbException:
        return abort(404, 'Non-existent paragraph')

    for group_id in get_session_usergroup_ids():
        for p in get_referenced_pars_from_req(par):
            timdb.readings.mark_read(group_id, Document(p.get_doc_id()), p, paragraph_type, commit=False)
    try:
        timdb.commit()
    except IntegrityError:
        abort(400, 'Paragraph was already marked read')
    return okJsonResponse()


@readings.route("/read/<int:doc_id>", methods=['PUT'])
def mark_all_read(doc_id):
    verify_read_marking_right(doc_id)
    timdb = getTimDb()
    doc = Document(doc_id)
    for group_id in get_session_usergroup_ids():
        timdb.readings.mark_all_read(group_id, doc, commit=False)
    timdb.commit()
    return okJsonResponse()
