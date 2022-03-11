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
from timApp.util.flask.requesthelper import verify_json_params, NotExist, RouteException
from timApp.util.flask.responsehelper import json_response, ok_response
from timApp.util import logger
from timApp.document.translation.translator import DeepLTranslator


def valid_language_id(lang_id):
    return re.match(r"^\w+$", lang_id) is not None


tr_bp = Blueprint("translation", __name__, url_prefix="")


@tr_bp.post("/translate/<int:tr_doc_id>/<language>")
def create_translation_route(tr_doc_id, language):
    req_data = request.get_json()
    title = req_data.get("doc_title", None)

    doc = get_doc_or_abort(tr_doc_id)

    verify_view_access(doc)
    if not valid_language_id(language):
        raise NotExist("Invalid language identifier")
    if doc.has_translation(language):
        raise ItemAlreadyExistsException("Translation for this language already exists")
    verify_manage_access(doc.src_doc)

    # NOTE Failing to create the translation still increases document id number and sometimes the manage page gets stuck (because of it?)
    src_doc = doc.src_doc.document
    cite_doc = create_document_and_block(get_current_user_object().get_personal_group())

    tr = Translation(doc_id=cite_doc.doc_id, src_docid=src_doc.doc_id, lang_id=language)
    tr.title = title

    add_reference_pars(cite_doc, src_doc, "tr")

    # Select the specified translator and translate if valid
    if translator_code := req_data.get("autotranslate", None):
        if translator_code.lower() == "deepl":
            deepl_translate(tr, src_doc.docinfo.lang_id, language, True)

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


@tr_bp.post("/translate/<int:tr_doc_id>/<language>/translate_block")
def create_block_translation_route(tr_doc_id, language):
    req_data = request.get_json()

    doc = get_doc_or_abort(tr_doc_id)

    verify_view_access(doc)
    verify_manage_access(doc.src_doc)

    # NOTE Failing to create the translation still increases document id number and sometimes the manage page gets stuck (because of it?)
    src_doc = doc.src_doc.document

    tr = Translation(doc_id=tr_doc_id, src_docid=src_doc.doc_id, lang_id=language)

    # add_reference_pars(cite_doc, src_doc, "tr")

    # Select the specified translator and translate if valid
    if translator_code := req_data.get("autotranslate", None):
        if translator_code.lower() == "deepl":
            deepl_translate(tr, src_doc.docinfo.lang_id, language, False)

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


@tr_bp.get("/translations/document-languages")
def get_document_languages():
    """
    A very rough version of getting the languages.
    """

    sl = ["Finnish-FI", "English-EN", "French-FR", "German-GE"]
    return json_response(sl)


@tr_bp.get("/translations/target-languages")
def get_target_languages():
    """
    A very rough version of getting the languages.
    """

    sl = ["Finnish-FI", "English-EN", "German-GE"]
    return json_response(sl)


def deepl_translate(
    tr: Translation, source_lang: str, target_lang: str, full_document: bool
) -> None:
    """
    Perform the machine translation using DeepL API
    :param tr: The version of the document to translate
    :param source_lang: The language to translate from
    :param target_lang: The language to translate into
    :param full_document: Whether the entire document will be translated or not
    """
    # Get the API-key from environment variable
    try:
        # TODO Get the API-key from user profile or the post-request
        from os import environ

        api_key = environ["DEEPL_API_KEY"]
        translator = DeepLTranslator(api_key)
    except KeyError:
        raise NotExist("The DEEPL_API_KEY is not set into your configuration")

    # TODO Languages should use common values / standard codes at this point (also applies to any doc.lang_id)
    if not translator.supports(source_lang, target_lang):
        raise RouteException(
            description=f"The language pair from {source_lang} to {target_lang} is not supported"
        )

    usage = translator.usage()
    logger.log_info(
        "Current DeepL API usage: "
        + str(usage.character_count)
        + "/"
        + str(usage.character_limit)
    )
    if full_document:
        # Be careful about closing the underlying file when iterating document
        with tr.document.get_source_document().__iter__() as doc_iter:
            # Translate each paragraph sequentially
            for orig_par, tr_par in zip(doc_iter, tr.document):
                new_text = translator.translate(orig_par.md, source_lang, target_lang)
                logger.log_info(new_text)
                tr.document.modify_paragraph(tr_par.id, new_text)
    """ else:
        test = tr.document.get_source_document().get_paragraph()
        test2 = [tr.document.get_paragraph(tr.__getattribute__("rt")).md]
        new_text = translator.translate(
            [tr.document.get_paragraph(tr.__getattribute__("rt")).md],
            source_lang,
            target_lang,
        )
        logger.log_info(new_text)
        tr.document.modify_paragraph(tr.doc_id, new_text)"""
