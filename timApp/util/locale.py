from flask import request, Response

from timApp.auth.sessioninfo import logged_in, get_current_user_object


def get_locale() -> str:
    header_lang: str = request.accept_languages.best_match(
        KNOWN_LANGUAGES, default="en-US"
    )  # type: ignore
    lng = request.cookies.get("lang")
    if not lng:
        if not logged_in():
            return header_lang
        u = get_current_user_object()
        lng = u.get_prefs().language
    if lng in KNOWN_LANGUAGES:
        return lng
    return header_lang


def update_locale_lang(resp: Response) -> Response:
    resp.set_cookie("lang", get_locale())
    return resp


KNOWN_LANGUAGES = [
    "fi",
    "sv",
    "en-US",
]
