"""Routes for settings view."""
from dataclasses import field
from typing import Any

from flask import render_template, session, flash, Response
from flask import request
from jinja2 import TemplateNotFound

from timApp.admin.user_cli import do_soft_delete
from timApp.answer.answer_models import AnswerUpload
from timApp.answer.routes import hide_points
from timApp.auth.accesshelper import verify_logged_in, verify_admin
from timApp.auth.sessioninfo import get_current_user_object
from timApp.document.docentry import DocEntry
from timApp.folder.folder import Folder
from timApp.item.block import Block, BlockType
from timApp.notification.notify import get_current_user_notifications
from timApp.timdb.sqa import db
from timApp.user.consentchange import ConsentChange
from timApp.user.preferences import Preferences
from timApp.user.user import User, Consent, get_owned_objects_query
from timApp.util.flask.requesthelper import get_option, RouteException, NotExist
from timApp.util.flask.responsehelper import json_response, ok_response, text_response
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


@settings_page.get("/getscss")
def get_scss() -> Response:
    from timApp.printing.print import print_doc_scss

    pref = get_current_user_object().get_prefs()
    res = print_doc_scss(DocEntry.query.filter_by(id=pref.theme_doc_ids[0]).first())
    res_str = open(res, "r").read()
    return text_response(res_str)


@settings_page.post("/save")
def save_settings() -> Response:
    user = get_current_user_object()

    # Don't overwrite bookmarks. If the user has multiple tabs open, the latest bookmarks might get overwritten.
    attrs_to_preserve = {"bookmarks"}

    j = request.get_json()
    if not j or not isinstance(j, dict):
        return json_response(user.get_prefs())

    try:
        curr_prefs = user.get_prefs()
        for attr in attrs_to_preserve:
            val = getattr(curr_prefs, attr)
            j[attr] = val
        user.set_prefs(Preferences.from_json(j))
    except TypeError as e:
        print(e)
        raise RouteException(f"Invalid settings: {j}")
    db.session.commit()
    show()  # Regenerate CSS
    return json_response(user.get_prefs())


@settings_page.put("/save/lang")
def save_language_route(lang: str) -> Response:
    u = get_current_user_object()
    prefs = u.get_prefs()
    prefs.language = lang
    u.set_prefs(prefs)
    db.session.commit()
    r = ok_response()
    r.set_cookie("lang", lang)
    return r


@settings_page.get("/get/<name>")
def get_setting(name: str) -> Response:
    prefs = get_current_user_object().get_prefs()
    return json_response({name: getattr(prefs, name, None)})


def get_user_info(u: User, include_doc_content: bool = False) -> dict[str, Any]:
    """Returns all data associated with a user."""
    block_query = get_owned_objects_query(u)
    docs = DocEntry.query.filter(DocEntry.id.in_(block_query)).all()
    folders = Folder.query.filter(Folder.id.in_(block_query)).all()
    images = Block.query.filter(
        Block.id.in_(block_query) & (Block.type_id == BlockType.Image.value)
    ).all()
    files = Block.query.filter(
        Block.id.in_(block_query) & (Block.type_id == BlockType.File.value)
    ).all()
    answers = u.answers.all()
    answer_uploads = AnswerUpload.query.filter(
        AnswerUpload.answer_id.in_([a.id for a in answers])
    ).all()
    answers_no_points = list(map(hide_points, answers))
    for d in docs:
        d.serialize_content = include_doc_content

    return {
        "annotations": u.annotations.all(),
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
    session.clear()
    flash("Your account has been deleted.")
    return ok_response()
