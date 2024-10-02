from io import BytesIO
from os.path import basename
from urllib.parse import urlparse

import requests
from flask import Response, send_file
from requests.exceptions import MissingSchema, InvalidURL

from timApp.auth.accesshelper import verify_logged_in
from timApp.tim_app import app
from timApp.util.flask.requesthelper import RouteException
from timApp.util.flask.responsehelper import add_csp_header, json_response
from timApp.util.flask.typedblueprint import TypedBlueprint

proxy = TypedBlueprint("proxy", __name__, url_prefix="")


@proxy.get("/getproxy")
def getproxy(
    url: str,
    auth_token: str | None = None,
    raw: bool = False,
    mimetype: str | None = None,
    file: bool = False,
) -> Response:
    parsed = urlparse(url)
    if not parsed.scheme:
        raise RouteException("Unknown URL scheme")
    if parsed.scheme not in ("http", "https"):
        raise RouteException(f"URL scheme not allowed: {parsed.scheme}")
    if parsed.netloc not in app.config["PROXY_WHITELIST"]:
        raise RouteException(f"URL domain not whitelisted: {parsed.netloc}")
    if parsed.netloc not in app.config["PROXY_WHITELIST_NO_LOGIN"]:
        verify_logged_in()
    headers = {}
    if auth_token:
        headers["Authorization"] = f"Token {auth_token}"
    try:
        r = requests.get(url, headers=headers)
    except (MissingSchema, InvalidURL):
        raise RouteException("Invalid URL")
    except requests.exceptions.ConnectionError as e:
        req_url = e.request.url if e.request else url
        raise RouteException(f"Connection error to {req_url}: {e}")
    if raw:
        mimetype = mimetype or r.headers.get("Content-Type", None)
        resp = Response(
            r.content,
            status=r.status_code,
            mimetype=mimetype,
        )
        add_csp_header(resp, "sandbox allow-scripts")
        resp.headers["Access-Control-Allow-Origin"] = "*"
        return resp
    if file and r.status_code == 200:
        filename = basename(parsed.path) or "download"
        mimetype = r.headers.get("Content-Type", "application/octet-stream")
        return send_file(
            BytesIO(r.content),
            as_attachment=True,
            download_name=filename,
            mimetype=mimetype,
        )

    return json_response({"data": r.text, "status_code": r.status_code})
