from dataclasses import dataclass
from typing import Optional

from flask import Response, request, Blueprint
from webargs.flaskparser import use_args

from timApp.auth.accesshelper import get_doc_or_abort, verify_edit_access, can_see_par_source, verify_copy_access, \
    AccessDenied
from timApp.auth.sessioninfo import get_current_user_object
from timApp.document.docparagraph import DocParagraph
from timApp.document.document import Document
from timApp.document.documentversion import DocumentVersion
from tim_common.marshmallow_dataclass import class_schema
from timApp.timdb.exceptions import TimDbException
from timApp.util.flask.requesthelper import get_option, NotExist
from timApp.util.flask.responsehelper import json_response

doc_bp = Blueprint('document',
                   __name__,
                   url_prefix='')


@doc_bp.get('/download/<int:doc_id>')
def download_document(doc_id):
    d = get_doc_or_abort(doc_id)
    verify_copy_access(d)
    return return_doc_content(d.document)


def return_doc_content(d: Document):
    use_raw = get_option(request, 'format', 'md') == 'json'
    if use_raw:
        return json_response(d.export_raw_data())
    else:
        return Response(d.export_markdown(), mimetype="text/plain")


@doc_bp.get('/download/<int:doc_id>/<int:major>/<int:minor>')
def download_document_version(doc_id, major, minor):
    d = get_doc_or_abort(doc_id)
    verify_edit_access(d)
    doc = DocumentVersion(doc_id, (major, minor))
    if not doc.exists():
        raise NotExist("This document version does not exist.")
    return return_doc_content(doc)


@doc_bp.get('/diff/<int:doc_id>/<int:major1>/<int:minor1>/<int:major2>/<int:minor2>')
def diff_document(doc_id, major1, minor1, major2, minor2):
    d = get_doc_or_abort(doc_id)
    verify_edit_access(d)
    doc1 = DocumentVersion(doc_id, (major1, minor1))
    doc2 = DocumentVersion(doc_id, (major2, minor2))
    if not doc1.exists():
        raise NotExist(f"The document version {(major1, minor1)} does not exist.")
    if not doc2.exists():
        raise NotExist(f"The document version {(major2, minor2)} does not exist.")
    return Response(DocumentVersion.get_diff(doc1, doc2), mimetype="text/html")


@dataclass
class GetBlockModel:
    doc_id: int
    par_id: str
    area_start: Optional[str] = None
    area_end: Optional[str] = None
    par_hash: Optional[str] = None
    use_exported: bool = True


GetBlockModelSchema = class_schema(GetBlockModel)


@doc_bp.get("/getBlock/<int:doc_id>/<par_id>")
def get_block(doc_id, par_id):
    return get_block_2(GetBlockModel(
        area_end=request.args.get('area_end'),
        area_start=request.args.get('area_start'),
        doc_id=doc_id,
        par_id=par_id,
    ))


@doc_bp.get("/getBlock")
@use_args(GetBlockModelSchema())
def get_block_schema(args: GetBlockModel):
    return get_block_2(args)


def get_block_2(args: GetBlockModel):
    d = get_doc_or_abort(args.doc_id)
    area_start = args.area_start
    area_end = args.area_end
    if area_start and area_end:
        verify_copy_access(d)
        try:
            section = d.document.export_section(area_start, area_end)
        except TimDbException as e:
            raise NotExist('Area not found. It may have been deleted.')
        return json_response({"text": section})
    else:
        try:
            p = d.document.get_paragraph(args.par_id)
            if not can_see_par_source(get_current_user_object(), p):
                raise AccessDenied()
            if args.par_hash:
                par = DocParagraph.get(d.document, args.par_id, args.par_hash or p.get_hash())
            else:
                par = p
        except TimDbException as e:
            raise NotExist('Paragraph not found. It may have been deleted.')
        if args.use_exported:
            return json_response({"text": par.get_exported_markdown()})
        else:
            return json_response({"text": par.get_markdown()})
