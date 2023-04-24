"""
Contains routes for making operations on translation documents. Mainly
translations on whole documents, paragraphs and raw text.

Also contains routes for getting available languages, names of machine
translators and queries related to API-keys of these machine translators.
"""

__authors__ = [
    "Mika Lehtinen",
    "Denis Zhidkikh",
    "Noora Jokela",
    "Riku Lehkonen",
    "Vili Moisala",
    "Juho Tarkkanen",
    "Sami Viitanen",
]
__license__ = "MIT"
__date__ = "25.4.2022"

import langcodes
import requests
from flask import request, Blueprint
from sqlalchemy.exc import IntegrityError

from timApp.auth.accesshelper import (
    get_doc_or_abort,
    verify_manage_access,
    has_manage_access,
    AccessDenied,
    verify_logged_in,
    verify_copy_access,
    verify_edit_access,
)
from timApp.auth.sessioninfo import get_current_user_object
from timApp.item.copy_rights import copy_rights
from timApp.document.document import Document
from timApp.document.docentry import create_document_and_block, DocEntry
from timApp.document.documents import add_reference_pars
from timApp.document.translation.translation import Translation
from timApp.timdb.exceptions import ItemAlreadyExistsException
from timApp.timdb.sqa import db
from timApp.util.flask.requesthelper import verify_json_params, NotExist, RouteException
from timApp.util.flask.responsehelper import json_response, ok_response, Response
from timApp.document.translation.translator import (
    TranslationService,
    RegisteredTranslationService,
    TranslationServiceKey,
    TranslationTarget,
    TranslateProcessor,
)
from timApp.document.translation.language import Language
from timApp.item.routes import get_document_relevance, set_relevance


def is_valid_language_id(lang_id: str) -> bool:
    """
    Check that the ID is recognized by the langcodes library and found in
    database.

    :param lang_id: Language id (or "tag") to check for validity.
    :return: True, if the standardized ID is found in database.
    """
    try:
        tag = langcodes.standardize_tag(lang_id)
        lang = Language.query_by_code(tag)
        return lang is not None
    except langcodes.LanguageTagError:
        return False


def translate_full_document(
    tr: Translation, src_doc: Document, target_language: Language, translator_code: str
) -> None:
    """
    Translate matching paragraphs of document based on an original source
    document.

    :param tr: The metadata of the translation target.
    :param src_doc: The original source document with translatable text.
    :param target_language: The language to translate the document into.
    :param translator_code: Identifier of the translator to use (machine or "Manual"
     if empty).
    :return: None. The translation is applied to document based on the
     tr-parameter.
    """

    processor = TranslateProcessor(
        translator_code,
        src_doc.docinfo.lang_id,
        target_language,
        get_current_user_object().get_personal_group(),
    )

    # Translate the paragraphs of the document if a translator was successfully
    # created.

    # Ignore the settings-paragraphs entirely to protect them from
    # mangling.
    source_paragraphs = list(
        filter(
            lambda x: not x.is_setting(),
            tr.document.get_source_document().get_paragraphs(),
        ),
    )

    tr_paragraphs = filter(lambda x: not x.is_setting(), tr.document.get_paragraphs())

    # Call the partially applied function that contains languages
    # selected earlier, to translate texts
    translated_texts = processor.translate(
        # Wrap the paragraphs to TranslationTarget objects that
        # translator accepts.
        # TODO This TranslationTarget -pattern is kinda useless, because
        #  explicit typechecking is performed on translate_paragraphs
        #  anyway...
        list(map(TranslationTarget, source_paragraphs))
    )

    # The order of paragraphs in both docs must match, so that correct
    # ones are modified.
    for tr_paragraph, text in zip(tr_paragraphs, translated_texts):
        # Note that the paragraph's text is stripped, as extra newlines at
        # start or end seem to break plugins.
        tr.document.modify_paragraph(tr_paragraph.id, text.strip())

    # Raise exception here rather than before the modification as not to
    # waste the (potentially usable) translation.
    # TODO Make TranslationService.translate to handle or accumulate(?) the
    #  exceptions so that for example the quota running out
    #  mid-translation, the partial results can be recovered.
    if len(translated_texts) != len(source_paragraphs):
        raise RouteException(
            description="Machine translation produced different amount of paragraphs"
        )


def get_languages(source_languages: bool) -> Response:
    """
    Get list of supported languages by machine translator.

    :param source_languages: Flag for getting source-language (True) list instead
     of target-language (False).
    :return: List of the supported languages by type (source or target).
    """
    req_data = request.get_json()
    translator = req_data.get("translator", "")

    # Do not make unneeded database queries if manual translation.
    if translator.lower() == "manual" or translator.lower() == "":
        langs = []
    else:
        # Get the translation service by the provided service name
        # TODO Maybe change to use an id instead?
        tr = (
            TranslationService.query.with_polymorphic("*")
            .filter(TranslationService.service_name == translator)
            .one()
        )

        if isinstance(tr, RegisteredTranslationService):
            tr.register(get_current_user_object().get_personal_group())

        langs = tr.get_languages(source_languages)

    return json_response(langs)


tr_bp = Blueprint("translation", __name__, url_prefix="")


@tr_bp.post("/translate/<int:tr_doc_id>/<language>/<translator>")
def create_translation_route(
    tr_doc_id: int, language: str, translator: str
) -> Response:
    """
    Create and add a translation version of a whole document. Make machine
    translation on it if so requested and authorized to.

    :param tr_doc_id: ID of a document that the translation can be made based
     on. ID of document, that is or is linked to the original source document.
    :param language: Language that will be set to the translation document and
     used in potential machine translation.
    :param translator: Identifying name of the translator to use (machine or
     manual).
    :return: The created translation document's information as JSON.
    """
    # TODO Move doc_title -parameter to the URL as well
    title = request.get_json().get("doc_title", None)

    doc = get_doc_or_abort(tr_doc_id)

    # The user making the translation should be able to fully read the
    # contents of the document the translation is started from, which
    # copy-access allows.
    verify_copy_access(doc)
    if not is_valid_language_id(language):
        raise NotExist("Invalid language identifier")
    if doc.has_translation(language):
        raise ItemAlreadyExistsException("Translation for this language already exists")
    # Manage access to the _actual_ source document is needed because creating
    # a new translation adds information to the source document (i.e. inserts
    # the new translation into the list of translations).
    verify_manage_access(doc.src_doc)

    src_doc = doc.src_doc.document
    cite_doc = create_document_and_block(get_current_user_object().get_personal_group())

    tr = Translation(doc_id=cite_doc.doc_id, src_docid=src_doc.doc_id, lang_id=language)
    tr.title = title

    add_reference_pars(
        cite_doc,
        src_doc,
        "tr",
        # TODO If auto-translation fails, maybe prevent this information from
        #  being included.
        translator=translator if translator != "Manual" else None,
    )

    if isinstance(doc, DocEntry):
        de = doc
    elif isinstance(doc, Translation):
        de = doc.docentry
    else:
        raise Exception("doc has unexpected type")
    de.trs.append(tr)
    # Inherit parent document's rights
    copy_rights(doc, tr, get_current_user_object(), copy_expired=False)
    db.session.commit()

    # Run automatic translation if requested
    if translator != "Manual":
        translate_full_document(tr, src_doc, language, translator)

    # Copy source document search relevance value to translation
    set_relevance(tr.id, get_document_relevance(doc))

    return json_response(tr)


@tr_bp.post("/translate/paragraph/<int:tr_doc_id>/<tr_par_id>/<language>/<transl>")
def paragraph_translation_route(
    tr_doc_id: int, tr_par_id: str, language: str, transl: str
) -> Response:
    """
    Replace the content of paragraph with requested translation.

    :param tr_doc_id: ID of the document that the paragraph is in.
    :param tr_par_id: ID of the paragraph in the Translation NOTE: NOT the
     original paragraph!
    :param language: Language to translate into.
    :param transl: Identifying code of the translator to use.
    :return: OK-response if translation and modification was successful.
    """
    translator_code = transl

    tr = get_doc_or_abort(tr_doc_id)
    src_doc = tr.src_doc

    if not isinstance(tr, Translation):
        raise Exception("Document is not Translation-type")
    # Need to be able to edit (this paragraph of) the translation-document.
    verify_edit_access(tr)
    # Need to be able to fully read the contents of (the matching paragraph
    # of) the document being translated from.
    verify_copy_access(src_doc)

    if not is_valid_language_id(language):
        raise NotExist("Invalid language identifier")

    if translator_code:
        # Paragraph in Translation is always assumed to be a reference and
        # raise an exception otherwise.
        tr_par = tr.document.get_paragraph(tr_par_id)
        if not tr_par.is_reference():
            raise RouteException(
                description="Cannot translate because paragraph missing reference to original"
            )

        src_par = src_doc.document.get_paragraph(tr_par.get_attr("rp"))
        processor = TranslateProcessor(
            translator_code,
            src_doc.lang_id,
            language,
            get_current_user_object().get_personal_group(),
        )

        translated_text = processor.translate([TranslationTarget(src_par)])[0]
        tr.document.modify_paragraph(tr_par_id, translated_text)

    return ok_response()


@tr_bp.post("/translate/<int:tr_doc_id>/<language>/translate_block/<transl>")
def text_translation_route(tr_doc_id: int, language: str, transl: str) -> Response:
    """
    Translate raw text between the source document's language and the one
    requested.

    :param tr_doc_id: ID of the document that the text is from.
    :param language: Language to translate the text into.
    :param transl: Identifying code of the translator to use.
    :return: The translated text.
    """
    req_data = request.get_json()

    doc = get_doc_or_abort(tr_doc_id)

    # Some of these checks might be overkill in some cases. They are to
    # prevent use of text-translation without a fitting Translation-document,
    # because the use-case is translating text highlighted using the
    # paragraph-editor on a translation-doc.
    if not isinstance(doc, Translation):
        raise Exception("Document is not Translation-type")
    verify_edit_access(doc)
    verify_copy_access(doc.src_doc)

    src_doc = doc.src_doc.document

    # Select the specified translator and translate if valid.
    if req_data and (translator_code := transl):
        src_text = req_data.get("originaltext", None)
        processor = TranslateProcessor(
            translator_code,
            src_doc.docinfo.lang_id,
            language,
            get_current_user_object().get_personal_group(),
        )
        # Save the leading and trailing whitespace as some machine translators
        # (especially DeepL) like to erase them (when they are used with
        # certain parameters).
        leading_wspace = src_text[: len(src_text) - len(src_text.lstrip())]
        # The trailing whitespace is reversed twice here, as it is also
        # collected reversed.
        trailing_wspace = src_text[::-1][
            : len(src_text) - len(src_text[::-1].lstrip())
        ][::-1]
        src_text = src_text.strip()
        block_text = processor.translate([TranslationTarget(src_text)])[0]
        # Remove extra newlines from start and end, as the parser likes to add
        # these.
        block_text = block_text.strip("\n")
        # Insert the whitespaces back.
        block_text = leading_wspace + block_text + trailing_wspace
    else:
        raise RouteException(
            description=f"Please select a translator from the 'Translator data' tab."
        )

    # TODO change to string_response?
    return json_response(block_text)


@tr_bp.post("/translation/<int:doc_id>")
def update_translation(doc_id):
    (lang_id, doc_title) = verify_json_params("new_langid", "new_title", require=True)
    if not is_valid_language_id(lang_id):
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


# TODO change into GET?
@tr_bp.post("/translations/sourceLanguages")
def get_source_languages() -> Response:
    """
    Query the database for the possible source languages.

    :return: JSON response containing the languages.
    """
    return get_languages(source_languages=True)


@tr_bp.get("/translations/allLanguages")
def get_all_languages() -> Response:
    """
    Query the database for all the available languages to be used for documents.

    :return: JSON response containing all the available languages.
    """

    langs = sorted(Language.query.all())
    return json_response(langs)


# TODO Change into GET?
@tr_bp.post("/translations/targetLanguages")
def get_target_languages() -> Response:
    """
    Query the database for the possible target languages.

    :return: JSON response containing the languages.
    """
    return get_languages(source_languages=False)


@tr_bp.get("/translations/translators")
def get_translators() -> Response:
    """
    Query the database for the possible machine translators.

    :return: JSON response containing the translators.
    """

    translationservice_names = TranslationService.query.with_entities(
        TranslationService.service_name
    ).all()
    # The SQLAlchemy query returns a list of tuples even when values of a
    # single column were requested, so they must be unpacked.
    # TODO Add "Manual" to the TranslationService-table instead of hardcoding
    #  here (and elsewhere)?
    sl = ["Manual"] + [x[0] for x in translationservice_names]
    return json_response(sl)


@tr_bp.put("/translations/apiKeys")
def add_api_key() -> Response:
    """
    Add API key to the database for current user.

    :return: OK response if adding the key was successful.
    """

    req_data = request.get_json()
    translator = req_data.get("translator", "")
    key = req_data.get("apikey", "")

    tr = TranslationService.query.filter(
        translator == TranslationService.service_name
    ).first()

    verify_logged_in()
    user = get_current_user_object()
    duplicate = TranslationServiceKey.query.filter(
        tr.id == TranslationServiceKey.service_id,
        user.get_personal_group().id == TranslationServiceKey.group_id,
    ).first()
    if duplicate:
        raise RouteException("There is already a key for this translator for this user")

    # Add the new API key.
    new_key = TranslationServiceKey(
        api_key=key,
        group_id=user.get_personal_group().id,
        service_id=tr.id,
    )
    db.session.add(new_key)
    db.session.commit()
    return ok_response()


@tr_bp.delete("/translations/apiKeys")
def remove_api_key() -> Response:
    """
    Remove the current user's API key from the database.

    :return: OK-response if removing the key was successful.
    """

    verify_logged_in()
    user = get_current_user_object()

    req_data = request.get_json()
    translator = req_data.get("translator", "")
    key = req_data.get("apikey", "")

    TranslationServiceKey.query.filter(
        key == TranslationServiceKey.api_key,
        TranslationServiceKey.group_id == user.get_personal_group().id,
        translator == TranslationService.service_name,
    ).delete(synchronize_session=False)

    db.session.commit()

    return ok_response()


# TODO Could this be GET? And would it create any security problems with for
#  example the API-key being shown in logs?
@tr_bp.post("/translations/apiKeys/quota")
def get_quota():
    """
    Gets the quota info for the user's API key.

    :return: The used and available quota for the user's API key as JSON.
    """
    verify_logged_in()

    req_data = request.get_json()
    translator = req_data.get("translator", "")
    key = req_data.get("apikey", "")

    # Get the translation service by the provided service name.
    # TODO Maybe change to use id instead?
    tr = TranslationService.query.filter(
        translator == TranslationService.service_name,
    ).first()
    tr.register(get_current_user_object().get_personal_group())

    return json_response(tr.usage())


@tr_bp.post("/translations/apiKeys/validate")
def get_valid_status() -> Response:
    """
    Check the validity of a given api-key with the chosen translator engine.

    :return: OK-response if the key is valid, or an Exception.
    """

    verify_logged_in()

    req_data = request.get_json()
    translator = req_data.get("translator", "")
    key = req_data.get("apikey", "")

    # Get the translation service by the provided service name.
    tr = TranslationService.query.filter(
        translator == TranslationService.service_name,
    ).first()

    # Each new translator engine should add their preferred method for
    # validating api keys here.
    # TODO might be prudent to do this in the specific translator class in the
    #  future.
    if tr.service_name.startswith("DeepL"):
        resp = requests.post(
            tr.service_url + "/usage",
            headers={"Authorization": f"DeepL-Auth-Key {key}"},
        )

    if resp.ok:
        return ok_response()
    else:
        raise RouteException(
            description="Inserted API key is not valid for the chosen translator engine."
        )


@tr_bp.get("/translations/apiKeys")
def get_keys() -> Response:
    """
    Gets the user's API keys.

    :return: The user's API keys as JSON.
    """
    verify_logged_in()

    user = get_current_user_object()
    keys = TranslationServiceKey.query.filter(
        TranslationServiceKey.group_id == user.get_personal_group().id
    ).all()

    return json_response(keys)


@tr_bp.get("/translations/myTranslators")
def get_my_translators() -> Response:
    """
    Gets the names of the translators the user has the API keys for.

    :return: The JSON-list of the names of the translators the user has the
     API keys for.
    """
    verify_logged_in()

    user = get_current_user_object()
    keys = TranslationServiceKey.query.filter(
        TranslationServiceKey.group_id == user.get_personal_group().id
    ).all()

    result = []
    for x in keys:
        # TODO Could implement to_json for TranslationServices also.
        result.append(x.service.service_name)

    return json_response(result)
