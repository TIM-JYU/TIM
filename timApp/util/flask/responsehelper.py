import csv
import http.client
import json
from _csv import QUOTE_MINIMAL
from io import StringIO
from typing import Any
from urllib.parse import urlparse, urljoin

from flask import (
    request,
    redirect,
    url_for,
    Response,
    stream_with_context,
    render_template,
    flash,
)

from timApp.document.viewcontext import ViewContext
from timApp.tim_app import app
from timApp.timdb.sqa import db
from tim_common.timjsonencoder import TimJsonEncoder


def is_safe_url(url):
    host_url = urlparse(request.host_url)
    test_url = urlparse(urljoin(request.host_url, url))
    return test_url.scheme in ["http", "https"] and host_url.netloc == test_url.netloc


def safe_redirect(url: str, **values) -> Response:
    if is_safe_url(url):
        return redirect(url, **values)
    return redirect(url_for("indexPage"))


def json_response(
    jsondata: Any,
    status_code: int = 200,
    headers: dict[str, str] | None = None,
    date_conversion: bool = False,
) -> Response:
    if not date_conversion:
        if headers is None:
            headers = {}
        headers["No-Date-Conversion"] = "true"
    response = Response(
        to_json_str(jsondata), mimetype="application/json", headers=headers
    )
    response.status_code = status_code
    return response


def json_response_and_commit(jsondata, status_code=200):
    db.session.commit()
    return json_response(jsondata, status_code)


def text_response(data: str, status_code: int = 200) -> Response:
    response = Response(data, mimetype="text/plain")
    response.status_code = status_code
    return response


def to_json_str(jsondata) -> str:
    return json.dumps(jsondata, separators=(",", ":"), cls=TimJsonEncoder)


def to_dict(jsondata: Any) -> dict:
    return json.loads(to_json_str(jsondata))


def no_cache_json_response(data: Any, date_conversion: bool = False) -> Response:
    """Returns a JSON response that prevents any caching of the result."""
    response = json_response(data, date_conversion=date_conversion)
    return add_no_cache_headers(response)


def add_no_cache_headers(response: Response):
    response.headers["Cache-Control"] = "no-store, must-revalidate"
    return response


def add_csp_header(response: Response, value: str = "sandbox"):
    response.headers["Content-Security-Policy"] = value
    return response


def ok_response() -> Response:
    return json_response({"status": "ok"})


def empty_response():
    return json_response({"empty": True})


def pad_csv_data(data: list[list[Any]]) -> list[list[str]]:
    """
    Pad each column with spaces to the maximum length of the column.
    :param data:
    :return:
    """
    max_lengths = []

    for column in zip(*data):
        max_lengths.append(max(len(str(cell)) for cell in column))
    new_data = []
    for row in data:
        new_row = []
        for i, cell in enumerate(row):
            cell = str(cell)
            new_row.append(cell.ljust(max_lengths[i]))
        new_data.append(new_row)
    return new_data


def csv_string(
    data: list[list[Any]],
    dialect: str,
    delimiter: str = ",",
    quoting: int = QUOTE_MINIMAL,
    pad_spaces: bool = False,
) -> str:
    line = StringIO()
    if pad_spaces:
        data = pad_csv_data(data)
    try:
        writer = csv.writer(line, dialect=dialect, delimiter=delimiter, quoting=quoting)
    except csv.Error:
        writer = csv.writer(line)
    for csv_line in data:
        writer.writerow(csv_line)
    return line.getvalue()  # .strip('\r\n')  # let the last lf be there


def iter_csv(
    data: list[list[Any]],
    dialect: str,
    delimiter: str = ",",
    quoting: int = QUOTE_MINIMAL,
    pad_spaces: bool = False,
):
    line = StringIO()
    if pad_spaces:
        data = pad_csv_data(data)
    try:
        writer = csv.writer(line, dialect=dialect, delimiter=delimiter, quoting=quoting)
    except csv.Error:
        writer = csv.writer(line)
    for csv_line in data:
        writer.writerow(csv_line)
        line.seek(0)
        yield line.read()
        line.truncate(0)
        line.seek(0)


def csv_response(
    data,
    dialect="excel",
    delimiter=",",
    quoting: int = QUOTE_MINIMAL,
    pad_spaces: bool = False,
):
    return Response(
        stream_with_context(iter_csv(data, dialect, delimiter, quoting, pad_spaces)),
        mimetype="text/plain",
    )


def error_generic(
    error: str | None, code: int, template="error.jinja2", status: str | None = None
):
    help_email = app.config["HELP_EMAIL"]
    if "text/html" in request.headers.get("Accept", ""):
        return (
            render_template(
                template,
                message=error
                or f"An error occurred ({code}). If this persists, please contact support at {help_email}",
                code=code,
                status=http.client.responses.get(code, None) or status,
            ),
            code,
        )
    else:
        return json_response({"error": error}, code)


def html_error(
    error: str,
    code: int,
    title=None,
    description="",
    auto_refresh=True,
    template="generic_html_error_standalone.jinja2",
):
    return (
        render_template(
            template,
            error=error,
            code=code,
            title=title,
            description=description,
            auto_refresh=auto_refresh,
            status=http.client.responses[code],
        ),
        code,
    )


def get_grid_modules():
    return [
        "ui.grid",
        "ui.grid.cellNav",
        "ui.grid.selection",
        "ui.grid.exporter",
        "ui.grid.autoResize",
        "ui.grid.saveState",
    ]


def flash_if_visible(message: str, view_ctx: ViewContext) -> None:
    if not view_ctx.preview and not view_ctx.partial:
        flash(message)
