import re
import langcodes
import requests

from flask import request, Blueprint
from sqlalchemy.exc import IntegrityError

import timApp.util.flask.responsehelper
from timApp.auth.accesshelper import (
    get_doc_or_abort,
    verify_view_access,
    verify_manage_access,
    has_manage_access,
    AccessDenied,
    verify_logged_in,
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
from timApp.document.translation.translator import (
    TranslationService,
    TranslationServiceKey,
    TranslationTarget,
    TranslateMethodFactory,
    get_lang_lists,
)
from timApp.document.translation.language import Language


def valid_language_id(lang_id: str) -> bool:
    """
    Check that the id is recognized by the langcodes library and found in
    database.

    :param lang_id: Language id (or "tag") to check
    :return: True, if id is found in database
    """
    # TODO This could return the queried language if found (to reduce db-queries)
    try:
        tag = langcodes.standardize_tag(lang_id)
        lang = Language.query_by_code(tag)
        return lang is not None
    except langcodes.LanguageTagError:
        return False


tr_bp = Blueprint("translation", __name__, url_prefix="")


@tr_bp.post("/translate/<int:tr_doc_id>/<string:language>/<string:transl>")
def create_translation_route(tr_doc_id: int, language: str, transl: str) -> Response:
    req_data = request.get_json()
    # TODO Move doc_title -parameter to the URL as well
    title = req_data.get("doc_title", None)

    doc = get_doc_or_abort(tr_doc_id)

    verify_view_access(doc)
    if not valid_language_id(language):
        raise NotExist("Invalid language identifier")
    if doc.has_translation(language):
        raise ItemAlreadyExistsException("Translation for this language already exists")
    verify_manage_access(doc.src_doc)

    # NOTE Failing to create the translation still increases document id number and sometimes the manage page gets stuck
    # (because of it?)
    src_doc = doc.src_doc.document
    cite_doc = create_document_and_block(get_current_user_object().get_personal_group())

    tr = Translation(doc_id=cite_doc.doc_id, src_docid=src_doc.doc_id, lang_id=language)
    tr.title = title

    translator = transl

    add_reference_pars(
        cite_doc,
        src_doc,
        "tr",
        translator=translator if translator != "Manual" else None,
    )

    # Select the specified translator
    translator_func = None
    if translator_code := translator:
        # Use the translator with a different source language if specified
        # and get the actual Language objects from database TODO Is database-query dumb here?

        # Manual translation can still be done without source language
        # TODO This could probably be handled nicer...
        if translator_code != "Manual":
            translator_func = TranslateMethodFactory.create(
                translator_code,
                src_doc.docinfo.lang_id,
                language,
                get_current_user_object().get_personal_group(),
            )

    # Translate the paragraphs of the document if a translator was created
    if translator_func:
        # Ignore the settings-paragraphs entirely to protect them from mangling
        source_paragraphs = list(
            filter(
                lambda x: not x.is_setting(),
                tr.document.get_source_document().get_paragraphs(),
            ),
        )

        tr_paragraphs = filter(
            lambda x: not x.is_setting(), tr.document.get_paragraphs()
        )

        # Call the partially applied function, that contains languages selected earlier, to translate texts
        translated_texts = translator_func(
            # Wrap the paragraphs to TranslationTarget objects, that translator accepts.
            # TODO Remove this TranslationTarget -pattern as useless, because explicit typechecking is performed on
            #  translate_paragraphs anyway...
            list(map(TranslationTarget, source_paragraphs))
        )

        assert len(translated_texts) == len(
            source_paragraphs
        ), "Translation produced different amount of paragraphs"

        # The order of paragraphs in both docs must match, so that correct ones are modified.
        for tr_paragraph, text in zip(tr_paragraphs, translated_texts):
            # Note that the paragraph's text is stripped, as extra newlines at start or end seemed to break plugins
            tr.document.modify_paragraph(tr_paragraph.id, text.strip())

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


@tr_bp.post(
    "/translate/paragraph/<int:tr_doc_id>/<string:tr_par_id>/<string:language>/<string:transl>"
)
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
    :return: Response to the request.
    """
    translator_code = transl

    # TODO Why should get_doc_or_abort be used?
    tr = Translation.query.get(get_doc_or_abort(tr_doc_id).id)
    src_doc = tr.src_doc

    if not isinstance(tr, Translation):
        assert False, "doc has unexpected type"
    # TODO Do these checks follow TIM conventions?
    verify_view_access(tr)
    verify_manage_access(src_doc)

    if not valid_language_id(language):
        raise NotExist("Invalid language identifier")

    if translator_code:
        # Paragraph in Translation is always assumed to be a reference and raise an exception otherwise
        tr_par = tr.document.get_paragraph(tr_par_id)
        if not tr_par.is_reference():
            raise RouteException(
                description="Cannot translate because paragraph missing reference to original"
            )

        src_par = src_doc.document.get_paragraph(tr_par.get_attr("rp"))
        translator_func = TranslateMethodFactory.create(
            translator_code,
            src_doc.lang_id,
            language,
            get_current_user_object().get_personal_group(),
        )

        translated_text = translator_func([TranslationTarget(src_par)])[0]
        tr.document.modify_paragraph(tr_par_id, translated_text)
        # TODO Maybe this is needed for modifying paragraphs???
        db.session.commit()

    # TODO (maybe duplicate) Could this cause unhandled exception if translator_code is not recognised and tries to
    #  return json_response(block_text) when block_text is not defined
    # TODO What should this even return? Maybe an ok_response?
    #  Or string_response?
    return json_response(tr_doc_id)


@tr_bp.post("/translate/<int:tr_doc_id>/<language>/translate_block/<string:transl>")
def text_translation_route(tr_doc_id: int, language: str, transl: str) -> Response:
    req_data = request.get_json()

    doc = get_doc_or_abort(tr_doc_id)

    verify_view_access(doc)
    verify_manage_access(doc.src_doc)

    src_doc = doc.src_doc.document

    # Select the specified translator and translate if valid
    if req_data and (translator_code := transl):
        src_text = req_data.get("originaltext", None)
        translator_func = TranslateMethodFactory.create(
            translator_code,
            src_doc.docinfo.lang_id,
            language,
            get_current_user_object().get_personal_group(),
        )
        block_text = translator_func([TranslationTarget(src_text)])[0]
    else:
        raise RouteException(
            description=f"Please select a translator from the 'Translator data' tab."
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


@tr_bp.post("/translations/source-languages")
def get_source_languages() -> Response:
    """Query the database for the possible source languages."""

    req_data = request.get_json()
    translator = req_data.get("translator", "")

    if translator.lower() == "manual" or translator.lower() == "":
        return json_response("")
    else:
        # Get the translation service by the provided service name TODO Maybe change to use id instead?
        tr = TranslationService.query.filter(
            translator == TranslationService.service_name,
        ).first()
        # TODO This crashes(?) if the translation service does not implement register-method (ie. does not inherit
        #  from RegisteredTranslationService)
        tr.register(get_current_user_object().get_personal_group())

    if translator.lower() == "deepl free" or translator.lower() == "deepl pro":
        langs = get_lang_lists(translator, True)
        sl = list(map(lambda x: {"name": x.autonym, "code": x.lang_code}, langs))
        return json_response(sl)
    else:
        langs = Language.query.all()
        sl = list(map(lambda x: {"name": x.autonym, "code": x.lang_code}, langs))
        return json_response(sl)


@tr_bp.get("/translations/document-languages")
def get_document_languages() -> Response:
    """Query the database for the languages of existing documents.
    TODO Select from documents
    """

    langs = Language.query.all()
    sl = list(map(lambda x: {"name": x.autonym, "code": x.lang_code}, langs))
    return json_response(sl)


@tr_bp.post("/translations/target-languages")
def get_target_languages() -> Response:
    """Query the database for the possible target languages."""

    req_data = request.get_json()
    translator = req_data.get("translator", "")

    if translator.lower() == "manual" or translator.lower() == "":
        return json_response("")
    else:
        # Get the translation service by the provided service name TODO Maybe change to use id instead?
        tr = TranslationService.query.filter(
            translator == TranslationService.service_name,
        ).first()
        tr.register(get_current_user_object().get_personal_group())

    if translator.lower() == "deepl free" or translator.lower() == "deepl pro":
        langs = get_lang_lists(translator, False)
        # TODO Implement to_json for Language and use it here
        sl = list(map(lambda x: {"name": x.autonym, "code": x.lang_code}, langs))
        return json_response(sl)
    else:
        langs = Language.query.all()
        sl = list(map(lambda x: {"name": x.autonym, "code": x.lang_code}, langs))
        return json_response(sl)


@tr_bp.get("/translations/translators")
def get_translators() -> Response:
    """Query the database for the possible machine translators."""

    translationservice_names = TranslationService.query.with_entities(
        TranslationService.service_name
    ).all()
    # The SQLAlchemy query returns a list of tuples even when values of a
    # single column were requested, so they must be unpacked.
    # TODO Add "Manual" to the TranslationService-table instead of hardcoding
    #  here (and elsewhere)?
    sl = ["Manual"] + [x[0] for x in translationservice_names]
    return json_response(sl)


@tr_bp.post("apikeys/add")
def add_api_key() -> Response:
    """The function for adding API keys."""

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

    # Add the new API key
    new_key = TranslationServiceKey(
        api_key=key,
        group_id=user.get_personal_group().id,
        service_id=tr.id,
    )
    db.session.add(new_key)
    db.session.commit()
    return ok_response()


@tr_bp.post("apikeys/remove")
def remove_api_key() -> Response:
    """The function for removing API keys."""

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


@tr_bp.post("/apikeys/quota")
def get_quota():
    verify_logged_in()

    req_data = request.get_json()
    translator = req_data.get("translator", "")
    key = req_data.get("apikey", "")

    # Get the translation service by the provided service name TODO Maybe change to use id instead?
    tr = TranslationService.query.filter(
        translator == TranslationService.service_name,
    ).first()
    tr.register(get_current_user_object().get_personal_group())

    return json_response(tr.usage())


@tr_bp.post("/apikeys/validate")
def get_valid_status() -> Response:
    """
    Check the validity of a given api-key with the chosen translator engine.

    :return: Response from the server, or an Exception
    """

    verify_logged_in()

    req_data = request.get_json()
    translator = req_data.get("translator", "")
    key = req_data.get("apikey", "")

    # Get the translation service by the provided service name
    tr = TranslationService.query.filter(
        translator == TranslationService.service_name,
    ).first()

    # Each new translator engine should add their preferred method for validating api keys here
    # TODO might be prudent to do this in the specific translator class in the future
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


@tr_bp.get("/apikeys/get")
def get_keys() -> Response:
    verify_logged_in()

    user = get_current_user_object()
    keys = TranslationServiceKey.query.filter(
        TranslationServiceKey.group_id == user.get_personal_group().id
    ).all()

    result = []
    for x in keys:
        result.append(
            {
                "translator": x.service.service_name,
                "APIkey": x.api_key,
            }
        )

    return json_response(result)


@tr_bp.get("/apikeys/translators")
def get_my_translators() -> Response:
    verify_logged_in()

    user = get_current_user_object()
    keys = TranslationServiceKey.query.filter(
        TranslationServiceKey.group_id == user.get_personal_group().id
    ).all()

    result = []
    for x in keys:
        result.append(x.service.service_name)

    return json_response(result)
