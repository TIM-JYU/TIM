import re

from flask import request, Blueprint
from sqlalchemy.exc import IntegrityError

from timApp.auth.accesshelper import (
    get_doc_or_abort,
    verify_view_access,
    verify_manage_access,
    has_manage_access,
    AccessDenied,
)
from timApp.auth.sessioninfo import get_current_user_object
from timApp.document.docentry import create_document_and_block, DocEntry
from timApp.document.documents import add_reference_pars
from timApp.document.translation.translation import Translation
from timApp.item.block import copy_default_rights, BlockType
from timApp.timdb.exceptions import ItemAlreadyExistsException
from timApp.timdb.sqa import db
from timApp.util.flask.requesthelper import verify_json_params, NotExist
from timApp.util.flask.responsehelper import json_response, ok_response
from timApp.util import logger
from timApp.document.translation.translator import DeepLTranslator


def valid_language_id(lang_id):
    return re.match(r"^\w+$", lang_id) is not None


tr_bp = Blueprint("translation", __name__, url_prefix="")


@tr_bp.post("/translate/<int:tr_doc_id>/<language>")
def create_translation_route(tr_doc_id, language):
    title = request.get_json().get("doc_title", None)

    doc = get_doc_or_abort(tr_doc_id)

    verify_view_access(doc)
    if not valid_language_id(language):
        raise NotExist("Invalid language identifier")
    if doc.has_translation(language):
        raise ItemAlreadyExistsException("Translation for this language already exists")
    verify_manage_access(doc.src_doc)

    src_doc = doc.src_doc.document
    cite_doc = create_document_and_block(get_current_user_object().get_personal_group())

    tr = Translation(doc_id=cite_doc.doc_id, src_docid=src_doc.doc_id, lang_id=language)
    tr.title = title

    # This call gets references to original document and attaches them to the new translated document
    add_reference_pars(cite_doc, src_doc, "tr")

    # Get the API-key from environment
    try:
        # TODO Get the API-key from user profile or the post-request
        from os import environ

        api_key = environ["DEEPL_API_KEY"]
        translator = DeepLTranslator(api_key)
        usage = translator.usage()
        logger.log_info(
            "Current DeepL API usage: "
            + str(usage.character_count)
            + "/"
            + str(usage.character_limit)
        )
    except KeyError:
        api_key = None
        logger.log_info(
            "The DEEPL_API_KEY environment variable is not set and automatic translation will not be made."
        )
    if api_key:
        # Be careful about closing the underlying file when iterating document
        with tr.document.get_source_document().__iter__() as doc_iter:
            # Translate each paragraph sequentially
            for orig_par, tr_par in zip(doc_iter, tr.document):
                new_text = translator.translate(orig_par.md, "FI", "EN-GB")
                logger.log_info(new_text)
                tr.document.modify_paragraph(tr_par.id, new_text)

    if isinstance(doc, DocEntry):
        de = doc
    elif isinstance(doc, Translation):
        de = doc.docentry
    else:
        assert False, "doc has unexpected type"
    de.trs.append(tr)
    copy_default_rights(tr, BlockType.Document)
    db.session.commit()
    return json_response(tr)


@tr_bp.post("/translation/<int:doc_id>")
def update_translation(doc_id):
    (lang_id, doc_title) = verify_json_params("new_langid", "new_title", require=True)
    if not valid_language_id(lang_id):
        raise AccessDenied("Invalid language identifier")
    doc = get_doc_or_abort(doc_id)
    if not has_manage_access(doc) and not has_manage_access(doc):
        raise AccessDenied(
            "You need manage access of either this or the translated document"
        )
    doc.lang_id = lang_id
    doc.title = doc_title
    try:
        db.session.commit()
    except IntegrityError:
        raise ItemAlreadyExistsException("This language already exists.")
    return ok_response()


@tr_bp.get("/translations/<int:doc_id>")
def get_translations(doc_id):
    d = get_doc_or_abort(doc_id)
    verify_manage_access(d)

    return json_response(d.translations)


@tr_bp.get("/translations/source-languages")
def get_source_languages():
    """
    A very rough version of getting the languages.
    """

    sl = ["Finnish-FI", "English-EN"]
    return json_response(sl)


@tr_bp.get("/translations/target-languages")
def get_target_languages():
    """
    A very rough version of getting the languages.
    """

    sl = ["Finnish-FI", "English-EN", "German-GE"]
    return json_response(sl)
