from flask import Blueprint, abort
from flask import current_app
from sqlalchemy.exc import IntegrityError

from timApp.auth.accesshelper import verify_read_marking_right, get_doc_or_abort
from timApp.util.flask.requesthelper import verify_json_params, get_referenced_pars_from_req
from timApp.util.flask.responsehelper import json_response, ok_response
from timApp.auth.sessioninfo import get_session_usergroup_ids, get_current_user_group
from timApp.timdb.exceptions import TimDbException
from timApp.readmark.readings import mark_read, get_readings, mark_all_read
from timApp.readmark.readparagraphtype import ReadParagraphType
from timApp.timdb.sqa import db
from timApp.readmark.readparagraph import ReadParagraph

readings = Blueprint('readings',
                     __name__,
                     url_prefix='')


@readings.route("/read/<int:doc_id>", methods=['GET'])
def get_read_paragraphs(doc_id):
    d = get_doc_or_abort(doc_id)
    verify_read_marking_right(d)
    doc = d.document
    # TODO: Get the intersection of read markings of all session users
    result = get_readings(get_current_user_group(), doc)
    return json_response(result)


@readings.route("/unread/<int:doc_id>/<par_id>", methods=['PUT'])
def unread_paragraph(doc_id, par_id):
    return set_read_paragraph(doc_id, par_id, unread=True)


@readings.route("/read/<int:doc_id>/<par_id>/<int:read_type>", methods=['PUT'])
def set_read_paragraph(doc_id, par_id, read_type=None, unread=False):
    paragraph_type = ReadParagraphType(read_type) if read_type is not None else ReadParagraphType.click_red
    if current_app.config['DISABLE_AUTOMATIC_READINGS'] and paragraph_type in (ReadParagraphType.on_screen,
                                                                               ReadParagraphType.hover_par):
        return ok_response()
    d = get_doc_or_abort(doc_id)
    verify_read_marking_right(d)
    doc = d.document
    par_ids, = verify_json_params('pars', require=False)
    if not par_ids:
        par_ids = [par_id]
    try:
        pars = [doc.get_paragraph(par_id) for par_id in par_ids]
    except TimDbException:
        return abort(404, 'Non-existent paragraph')

    for group_id in get_session_usergroup_ids():
        for par in pars:
            for p in get_referenced_pars_from_req(par):
                if unread:
                    rp = ReadParagraph.query.filter_by(usergroup_id=group_id,
                                                       doc_id=p.get_doc_id(),
                                                       par_id=p.get_id(),
                                                       type=paragraph_type)
                    rp.delete()
                else:
                    mark_read(group_id, p.doc, p, paragraph_type)
    try:
        db.session.commit()
    except IntegrityError:
        abort(400, 'Paragraph was already marked read')
    return ok_response()


@readings.route("/read/<int:doc_id>", methods=['PUT'])
def mark_document_read(doc_id):
    d = get_doc_or_abort(doc_id)
    verify_read_marking_right(d)
    doc = d.document
    for group_id in get_session_usergroup_ids():
        mark_all_read(group_id, doc)
    db.session.commit()
    return ok_response()
