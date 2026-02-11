from flask import request, Response, current_app

from timApp.auth.sessioninfo import (
    logged_in,
    get_current_user_object,
    get_document_lang_override,
)


def get_locale(force_refresh: bool = False) -> str:
    header_lang: str = request.accept_languages.best_match(
        KNOWN_LANGUAGES, default="en-US"
    )  # type: ignore
    lng = request.cookies.get("lang")
    default_language: str | None = current_app.config["DEFAULT_UI_LANGUAGE"]
    if default_language not in KNOWN_LANGUAGES:
        default_language = None
    if not lng or force_refresh:
        if not logged_in():
            return default_language or header_lang
        u = get_current_user_object()
        lng = u.get_prefs().language
    if lng in KNOWN_LANGUAGES:
        header_lang = lng
    elif default_language is not None:
        header_lang = default_language
    override = get_document_lang_override()
    if override and override in KNOWN_LANGUAGES:
        header_lang = override
    return header_lang


def update_locale_lang(resp: Response) -> Response:
    resp.set_cookie(
        "lang",
        get_locale(force_refresh=True),
        samesite=current_app.config["SESSION_COOKIE_SAMESITE"],
        secure=current_app.config["SESSION_COOKIE_SECURE"],
    )
    return resp


KNOWN_LANGUAGES = [
    "fi",
    "sv",
    "en-US",
    "es",
    "it",
]
