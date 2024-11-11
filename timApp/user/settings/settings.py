"""Routes for settings view."""
from dataclasses import field
from typing import Any, Sequence

from flask import render_template, flash, Response
from flask import request
from flask_babel import refresh
from jinja2 import TemplateNotFound
from sqlalchemy import select

from timApp.admin.user_cli import do_soft_delete
from timApp.answer.answer_models import AnswerUpload
from timApp.answer.routes import hide_points, hide_points_modifier
from timApp.auth.accesshelper import verify_logged_in, verify_admin, verify_view_access
from timApp.auth.sessioninfo import get_current_user_object, clear_session
from timApp.document.docentry import DocEntry
from timApp.folder.folder import Folder
from timApp.item.block import Block, BlockType
from timApp.notification.notify import get_current_user_notifications
from timApp.timdb.sqa import db, run_sql
from timApp.user.consentchange import ConsentChange
from timApp.user.preferences import Preferences
from timApp.user.settings.style_utils import is_style_doc
from timApp.user.user import User, Consent, get_owned_objects_query
from timApp.util.flask.requesthelper import get_option, RouteException, NotExist
from timApp.util.flask.responsehelper import json_response, ok_response
from timApp.util.flask.typedblueprint import TypedBlueprint

settings_page = TypedBlueprint("settings_page", __name__, url_prefix="/settings")


@settings_page.before_request
def verify_login() -> None:
    verify_logged_in()


@settings_page.get("")
def show() -> str:
    try:
        limit = 50
        return render_template(
            "settings.jinja2",
            notification_limit=limit,
            notifications=get_current_user_notifications(limit=limit),
            contacts=get_current_user_object().contacts,
        )
    except TemplateNotFound:
        raise NotExist()


@settings_page.get("/get")
def get_settings() -> Response:
    return json_response(get_current_user_object().get_prefs())


def verify_new_styles(curr_styles: list[int], new_styles: list[int]) -> None:
    new_style_doc_ids = set(new_styles) - set(curr_styles)
    if not new_style_doc_ids:
        return

    new_style_docs: Sequence[DocEntry] = (
        run_sql(select(DocEntry).filter(DocEntry.id.in_(new_style_doc_ids)))
        .scalars()
        .all()
    )

    if len(new_style_docs) != len(new_style_doc_ids):
        raise NotExist("Some style docs could not be found")

    for doc in new_style_docs:
        if not is_style_doc(doc):
            raise RouteException(f"Document {doc.path} is not a style document")
        verify_view_access(doc)


@settings_page.post("/save")
def save_settings() -> Response:
    user = get_current_user_object()

    # Don't overwrite bookmarks. If the user has multiple tabs open, the latest bookmarks might get overwritten.
    attrs_to_preserve = {"bookmarks"}

    j = request.get_json(silent=True)
    if not j or not isinstance(j, dict):
        return json_response(user.get_prefs().to_json(with_style=True))

    try:
        curr_prefs = user.get_prefs()
        for attr in attrs_to_preserve:
            val = getattr(curr_prefs, attr)
            j[attr] = val
        new_prefs = Preferences.from_json(j)
        verify_new_styles(curr_prefs.style_doc_ids, new_prefs.style_doc_ids)
        user.set_prefs(new_prefs)
    except TypeError as e:
        raise RouteException(f"Invalid settings: {e}")
    db.session.commit()
    r = json_response(user.get_prefs().to_json(with_style=True))
    if new_prefs.language:
        r.set_cookie("lang", new_prefs.language)
    return r


@settings_page.put("/save/lang")
def save_language_route(lang: str) -> Response:
    u = get_current_user_object()
    prefs = u.get_prefs()
    prefs.language = lang
    u.set_prefs(prefs)
    db.session.commit()
    r = ok_response()
    r.set_cookie("lang", lang)

    refresh()

    return r


@settings_page.post("/save/parmenupos/<int:pos>")
def save_parmenu_position_route(pos: int) -> Response:
    u = get_current_user_object()
    prefs = u.get_prefs()
    prefs.parmenu_position = pos
    u.set_prefs(prefs)
    db.session.commit()
    r = ok_response()
    return r


@settings_page.get("/get/<name>")
def get_setting(name: str) -> Response:
    prefs = get_current_user_object().get_prefs()
    return json_response({name: getattr(prefs, name, None)})


def get_user_info(u: User, include_doc_content: bool = False) -> dict[str, Any]:
    """Returns all data associated with a user."""
    block_query = get_owned_objects_query(u)
    docs = (
        run_sql(select(DocEntry).filter(DocEntry.id.in_(block_query))).scalars().all()
    )
    folders = run_sql(select(Folder).filter(Folder.id.in_(block_query))).scalars().all()
    images = (
        run_sql(
            select(Block).filter(
                Block.id.in_(block_query) & (Block.type_id == BlockType.Image.value)
            )
        )
        .scalars()
        .all()
    )
    files = (
        run_sql(
            select(Block).filter(
                Block.id.in_(block_query) & (Block.type_id == BlockType.File.value)
            )
        )
        .scalars()
        .all()
    )
    answers = u.answers.all()
    answer_uploads = (
        run_sql(
            select(AnswerUpload).filter(
                AnswerUpload.answer_id.in_([a.id for a in answers])
            )
        )
        .scalars()
        .all()
    )
    answers_no_points = list(map(hide_points, answers))
    answers_no_points = list(map(hide_points_modifier, answers_no_points))
    for d in docs:
        d.serialize_content = include_doc_content
    annotations = u.annotations.all()
    for ann in annotations:
        for c in ann.comments:
            if c.commenter.id != u.id:
                c.commenter.is_anonymized = True

    return {
        "annotations": annotations,
        "answers": answers_no_points,
        "answer_uploads": answer_uploads,
        "groups": u.groups,
        "lectureanswers": u.lectureanswers.all(),
        "notes": u.get_personal_group().notes.all(),
        "owned_documents": docs,
        "owned_folders": folders,
        "owned_lectures": u.owned_lectures.all(),
        "readparagraphs": u.get_personal_group().readparagraphs.all(),
        "uploaded_images": images,
        "uploaded_files": files,
        "user": {
            **u.to_json(contacts=True),
            "given_name": u.given_name,
            "last_name": u.last_name,
            "prefs": u.prefs,
            "origin": u.origin,
            "consent": u.consent,
            "created": u.created,
            "modified": u.modified,
        },
        "velps": u.velps.all(),
    }


def get_info_for(u: User) -> Response:
    include_doc_content = get_option(request, "content", False)
    return json_response(get_user_info(u, include_doc_content))


@settings_page.get("/info")
def get_info_current() -> Response:
    return get_info_for(get_current_user_object())


@settings_page.get("/info/<username>")
def get_info_any(username: str) -> Response:
    verify_admin()
    u = User.get_by_name(username)
    if not u:
        raise NotExist("User not found")
    return get_info_for(u)


@settings_page.post("/updateConsent")
def update_consent(consent: Consent = field(metadata={"by_value": True})) -> Response:
    u = get_current_user_object()
    if u.consent != consent:
        u.consent = consent
        u.consents.append(ConsentChange(consent=consent))
        db.session.commit()
    return ok_response()


@settings_page.post("/account/delete")
def delete_account() -> Response:
    verify_logged_in()
    u = get_current_user_object()
    if not u.is_email_user:
        raise RouteException(
            "Only users registered via email can delete their account manually."
        )
    do_soft_delete(u)
    db.session.commit()
    clear_session()
    flash("Your account has been deleted.")
    return ok_response()
