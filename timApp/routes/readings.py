from flask import Blueprint, abort

from documentmodel.document import Document
from routes.common import verify_read_marking_right, getTimDb, jsonResponse, getCurrentUserGroup, \
    get_referenced_pars_from_req, okJsonResponse, get_session_usergroup_ids

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


@readings.route("/read/<int:doc_id>/<specifier>", methods=['PUT'])
def set_read_paragraph(doc_id, specifier):
    verify_read_marking_right(doc_id)
    timdb = getTimDb()

    doc = Document(doc_id)
    par = doc.get_paragraph(specifier)
    if par is None:
        return abort(400, 'Non-existent paragraph')

    for group_id in get_session_usergroup_ids():
        for p in get_referenced_pars_from_req(par):
            timdb.readings.mark_read(group_id, Document(p.get_doc_id()), p, commit=False)
    timdb.commit()
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
