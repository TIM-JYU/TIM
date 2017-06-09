from flask import Blueprint, abort
from flask import current_app
from sqlalchemy.exc import IntegrityError

from timApp.accesshelper import verify_read_marking_right
from timApp.dbaccess import get_timdb
from timApp.documentmodel.document import Document
from timApp.requesthelper import verify_json_params, get_referenced_pars_from_req
from timApp.responsehelper import json_response, ok_response
from timApp.sessioninfo import get_session_usergroup_ids, get_current_user_group
from timApp.timdb.readparagraphtype import ReadParagraphType
from timApp.timdb.tim_models import ReadParagraph
from timApp.timdb.timdbexception import TimDbException

readings = Blueprint('readings',
                     __name__,
                     url_prefix='')


@readings.route("/read/<int:doc_id>", methods=['GET'])
def get_read_paragraphs(doc_id):
    verify_read_marking_right(doc_id)
    timdb = get_timdb()
    doc = Document(doc_id)
    # TODO: Get the intersection of read markings of all session users
    result = timdb.readings.get_readings(get_current_user_group(), doc)
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
    verify_read_marking_right(doc_id)
    timdb = get_timdb()

    doc = Document(doc_id)
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
                    timdb.readings.mark_read(group_id, Document(p.get_doc_id()), p, paragraph_type, commit=False)
    try:
        timdb.commit()
    except IntegrityError:
        abort(400, 'Paragraph was already marked read')
    return ok_response()


@readings.route("/read/<int:doc_id>", methods=['PUT'])
def mark_all_read(doc_id):
    verify_read_marking_right(doc_id)
    timdb = get_timdb()
    doc = Document(doc_id)
    for group_id in get_session_usergroup_ids():
        timdb.readings.mark_all_read(group_id, doc, commit=False)
    timdb.commit()
    return ok_response()
