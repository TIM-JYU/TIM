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

redirect_route = TypedBlueprint("redirect", __name__, url_prefix="")


@redirect_route.get("/r/<alias>")
def redirect_by_alias_file(
    alias: str
) -> Response:
    parsed = alias

    return json_response({"data": parsed, "status_code": 200})
