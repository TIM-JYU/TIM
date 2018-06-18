from flask import request, abort, Blueprint

from timApp.auth.accesshelper import get_doc_or_abort, verify_manage_access
from timApp.slide.slidestatus import SlideStatus
from timApp.timdb.sqa import db
from timApp.util.flask.responsehelper import json_response, ok_response

slide_bp = Blueprint('slide',
                     __name__,
                     url_prefix='')


@slide_bp.route("/getslidestatus")
def getslidestatus():
    if 'doc_id' not in request.args:
        abort(404, "Missing doc id")
    doc_id = int(request.args['doc_id'])
    status = SlideStatus.query.filter_by(doc_id=doc_id).first()
    if status:
        status = status.status
    return json_response(status)


@slide_bp.route("/setslidestatus")
def setslidestatus():
    if 'doc_id' not in request.args or 'status' not in request.args:
        abort(404, "Missing doc id or status")
    doc_id = int(request.args['doc_id'])
    d = get_doc_or_abort(doc_id)
    verify_manage_access(d)
    status = request.args['status']
    status = SlideStatus(doc_id=doc_id, status=status)
    db.session.merge(status)
    db.session.commit()
    return ok_response()
