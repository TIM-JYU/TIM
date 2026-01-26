import re
from dataclasses import dataclass
from urllib.parse import urlparse, parse_qsl, urlencode, urlsplit, urlunsplit

from flask import Response, redirect, current_app, request

from timApp.auth.accesshelper import verify_view_access
from timApp.auth.sessioninfo import user_context_with_logged_in_or_anon
from timApp.document.docentry import DocEntry
from timApp.document.viewcontext import default_view_ctx
from timApp.markdown.markdownconverter import expand_macros_info
from timApp.util.flask.requesthelper import NotExist, RouteException
from timApp.util.flask.typedblueprint import TypedBlueprint

redirect_route = TypedBlueprint("redirect", __name__, url_prefix="")

TIMRedirectExceptionMessages: dict[str, dict[str, str]] = {
    "too_many_target_urls": {
        "description": "Redirect error: Too many target URLs",
        "start": "The alias file",
        "end": "should include only one line!",
    },
    "malformed_target_url": {
        "description": "Redirect error: Malformed target URL",
        "start": "The alias file",
        "end": "should start with 'http' or '/'!",
    },
}


@dataclass
class TIMRedirectException(RouteException):
    message: dict[str, str]
    alias_doc: dict[str, str]
    code: int = 400


@redirect_route.get("/r/<alias>")
def redirect_by_alias_file(alias: str) -> Response:
    alias = alias.lower().strip()
    match = re.match(r"^[a-zåäö0-9_.-]*", alias)
    alias = match.group(0) if match else ""
    hostname = current_app.config["TIM_HOST"]
    doc_entry = DocEntry.find_by_path(f"redirect/{alias}")
    if not doc_entry:
        raise NotExist(f"Alias '{alias}' does not exist!")

    verify_view_access(doc_entry)

    res = doc_entry.document.export_markdown(
        export_ids=False, export_settings=False
    ).strip("\n")

    # Fetch user context and unwrap macros
    user_ctx = user_context_with_logged_in_or_anon()
    settings = doc_entry.document.get_settings()
    macro_info = settings.get_macroinfo(default_view_ctx, user_ctx)
    res = expand_macros_info(res, macro_info, ignore_errors=True).strip("\n").strip(" ")
    alias_doc_name = f"{hostname}/view/redirect/{alias}"

    if res.find("\n") >= 0:
        raise TIMRedirectException(
            message=TIMRedirectExceptionMessages["too_many_target_urls"],
            alias_doc={"name": alias_doc_name, "content": res},
        )

    match = re.match("^(http)|(/).*$", res)
    if not match:
        raise TIMRedirectException(
            message=TIMRedirectExceptionMessages["malformed_target_url"],
            alias_doc={"name": alias_doc_name, "content": res},
        )

    # Possible regression with typing in Flask, or Mypy
    # see: https://github.com/pallets/flask/issues/4612 and https://github.com/pallets/flask/issues/4600
    res_parts = urlsplit(res)
    res_parsed = urlparse(res)
    res_qs = dict(parse_qsl(res_parsed.query, keep_blank_values=True))
    alias_qs = dict(request.args.lists())
    merged_qs = {**res_qs, **alias_qs}
    new_query = urlencode(merged_qs, doseq=True)
    # noinspection PyProtectedMember
    # new_url = urlunparse(res_parsed._replace(query=new_query))
    new_url = urlunsplit((
            res_parts.scheme,
            res_parts.netloc,
            res_parts.path,
            new_query,
            res_parts.fragment,
        )
    )
    return redirect(new_url)  # type: ignore
