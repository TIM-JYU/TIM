from flask import Response, abort, request, Blueprint

from timApp.auth.accesshelper import get_doc_or_abort, verify_edit_access
from timApp.document.documentversion import DocumentVersion
from timApp.tim_app import app
from timApp.timdb.exceptions import TimDbException
from timApp.util.flask.responsehelper import json_response

doc_bp = Blueprint('document',
                   __name__,
                   url_prefix='')


@doc_bp.route('/download/<int:doc_id>')
def download_document(doc_id):
    d = get_doc_or_abort(doc_id)
    verify_edit_access(d)
    return Response(d.document.export_markdown(), mimetype="text/plain")


@doc_bp.route('/download/<int:doc_id>/<int:major>/<int:minor>')
def download_document_version(doc_id, major, minor):
    d = get_doc_or_abort(doc_id)
    verify_edit_access(d)
    doc = DocumentVersion(doc_id, (major, minor))
    if not doc.exists():
        abort(404, "This document version does not exist.")
    return Response(doc.export_markdown(), mimetype="text/plain")


@doc_bp.route('/diff/<int:doc_id>/<int:major1>/<int:minor1>/<int:major2>/<int:minor2>')
def diff_document(doc_id, major1, minor1, major2, minor2):
    d = get_doc_or_abort(doc_id)
    verify_edit_access(d)
    doc1 = DocumentVersion(doc_id, (major1, minor1))
    doc2 = DocumentVersion(doc_id, (major2, minor2))
    if not doc1.exists():
        abort(404, f"The document version {(major1, minor1)} does not exist.")
    if not doc2.exists():
        abort(404, f"The document version {(major2, minor2)} does not exist.")
    return Response(DocumentVersion.get_diff(doc1, doc2), mimetype="text/html")


@doc_bp.route("/getBlock/<int:doc_id>/<par_id>")
def get_block(doc_id, par_id):
    d = get_doc_or_abort(doc_id)
    verify_edit_access(d)
    area_start = request.args.get('area_start')
    area_end = request.args.get('area_end')
    if area_start and area_end:
        try:
            section = d.document.export_section(area_start, area_end)
        except TimDbException as e:
            return abort(404, str(e))
        return json_response({"text": section})
    else:
        try:
            par = d.document.get_paragraph(par_id)
        except TimDbException as e:
            return abort(404, str(e))
        return json_response({"text": par.get_exported_markdown()})
