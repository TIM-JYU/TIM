import re

from flask import request, Blueprint
from sqlalchemy.exc import IntegrityError
import langcodes

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
from timApp.util.flask.responsehelper import json_response, ok_response, Response
from timApp.util import logger
from timApp.document.translation.translator import (
    TranslationService,
    DeeplTranslationService,
)
from timApp.document.translation.language import Language
from timApp.user.usergroup import UserGroup


def valid_language_id(lang_id: str) -> bool:
    """Check that the id is recognized by the langcodes library."""
    # TODO Handle non langcodes-supported languages
    try:
        tag = langcodes.standardize_tag(lang_id)
        return Language.find_by_str(tag) is not None
    except (langcodes.LanguageTagError, LookupError):
        return False


def translate(
    translator: TranslationService,
    text: str,
    source_lang: Language,
    target_lang: Language,
) -> list[str]:
    # TODO This helper-function would be better if there was some way to hide the translate-methods on ITranslators. Maybe save for the eventual(?) TranslatorSelector?
    """
    Use the specified ITranslator to perform machine translation on text
    :param translator: The translator to use
    :param text: The text to translate
    :param source_lang: The language that text is in
    :param target_lang: The language that text will be translated into
    :return: The text in the target language
    """
    translated_text = translator.translate([text], source_lang, target_lang)
    # TODO Maybe log the length of text or other shorter info?
    logger.log_info("\n".join(translated_text))

    usage = translator.usage()
    logger.log_info(
        "Current DeepL API usage: "
        + str(usage.character_count)
        + "/"
        + str(usage.character_limit)
    )

    return translated_text


def init_deepl_translator(
    source_lang: Language, target_lang: Language
) -> TranslationService:
    """
    Initialize the deepl translator using the API-key from user's configuration
    :param source_lang: Language that is requested to translate from
    :param target_lang: Language that is requested to translate into
    :return: DeepLTranslator instance, that is ready for translate-calls
    """
    # Get the API-key from database
    translator = DeeplTranslationService.query.filter(
        UserGroup.id == get_current_user_object().get_personal_group().id
    ).first()

    # TODO Languages should use common values / standard codes at this point (also applies to any doc.lang_id)
    if not translator.supports(source_lang, target_lang):
        raise RouteException(
            description=f"The language pair from '{source_lang}' to '{target_lang}' is not supported with DeepL"
        )

    return translator


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

    translator_language = req_data.get("translatorlang", None)
    # Check if translator language is the same as document language
    if language == translator_language:
        tr_lang = language
    else:
        tr_lang = translator_language

    # Use the translator with a different source language if specified
    src_lang = req_data.get("origlang", src_doc.docinfo.lang_id)

    # Select the specified translator
    translator = None
    if translator_code := req_data.get("autotranslate", None):
        # Get the actual Language objects TODO This is dumb here
        # TODO Add the language to database if not already found
        src_lang = Language.find_by_str(src_lang)
        tr_lang = Language.find_by_str(tr_lang)
        # TODO From database, check that the translation language-pair is supported, or just let it through and shift this responsibility to user and the interface, because the API-call should handle unsupported languages anyway?
        if translator_code.lower() == "deepl":
            translator = init_deepl_translator(src_lang, tr_lang)
    # Translate each paragraph sequentially if a translator was created
    if translator:
        # Get the actual Language objects TODO This is dumb here
        # TODO Add the language to database if not already found
        src_lang = Language.find_by_str(src_lang)
        tr_lang = Language.find_by_str(tr_lang)
        for orig_par, tr_par in zip(
            tr.document.get_source_document().get_paragraphs(), tr.document
        ):
            translated_text = translate(translator, orig_par.md, src_lang, tr_lang)
            tr.document.modify_paragraph(tr_par.id, translated_text)

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
def text_translation_route(tr_doc_id: int, language: str) -> Response:
    req_data = request.get_json()

    doc = get_doc_or_abort(tr_doc_id)

    verify_view_access(doc)
    verify_manage_access(doc.src_doc)

    src_doc = doc.src_doc.document

    # Select the specified translator and translate if valid
    if req_data and (translator_code := req_data.get("autotranslate", None)):
        src_text = req_data.get("originaltext", None)
        if translator_code.lower() == "deepl":
            src_lang = Language.find_by_str(src_doc.docinfo.lang_id)
            target_lang = Language.find_by_str(language)
            translator = init_deepl_translator(src_lang, target_lang)
            block_text = translate(translator, src_text, src_lang, target_lang)
    else:
        raise RouteException(
            description=f"Please select a translator from the 'Translator data' tab"
        )

    return json_response(block_text)


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
def get_translations(doc_id: int) -> Response:
    d = get_doc_or_abort(doc_id)
    verify_manage_access(d)

    return json_response(d.translations)


@tr_bp.get("/translations/source-languages")
def get_source_languages() -> Response:
    """
    A very rough version of getting the languages.
    """

    sl = ["Finnish-FI", "English-EN"]
    return json_response(sl)


@tr_bp.get("/translations/document-languages")
def get_document_languages() -> Response:
    """
    A very rough version of getting the languages.
    """

    sl = ["Finnish-FI", "English-EN", "French-FR", "German-GE"]
    return json_response(sl)


@tr_bp.get("/translations/target-languages")
def get_target_languages() -> Response:
    """
    A very rough version of getting the languages.
    """

    sl = ["Finnish-FI", "English-EN", "German-GE"]
    return json_response(sl)


@tr_bp.get("/translations/translators")
def get_translators() -> Response:
    """
    A very rough version of getting the translators.
    """

    sl = ["Manual", "DeepL"]
    return json_response(sl)
