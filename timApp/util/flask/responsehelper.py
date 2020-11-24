import csv
import http.client
import json
from io import StringIO
from typing import Any, Optional, Dict
from urllib.parse import urlparse, urljoin

from flask import request, redirect, url_for, Response, stream_with_context, render_template

from timApp.document.timjsonencoder import TimJsonEncoder
from timApp.timdb.sqa import db


def is_safe_url(url):
    host_url = urlparse(request.host_url)
    test_url = urlparse(urljoin(request.host_url, url))
    return test_url.scheme in ['http', 'https'] and \
        host_url.netloc == test_url.netloc


def safe_redirect(url: str, **values) -> Response:
    if is_safe_url(url):
        return redirect(url, **values)
    return redirect(url_for('indexPage'))


def json_response(
        jsondata: Any,
        status_code: int = 200,
        headers: Optional[Dict[str, str]] = None,
        date_conversion: bool = False,
) -> Response:
    if not date_conversion:
        if headers is None:
            headers = {}
        headers['No-Date-Conversion'] = 'true'
    response = Response(to_json_str(jsondata), mimetype='application/json', headers=headers)
    response.status_code = status_code
    return response


def json_response_and_commit(jsondata, status_code=200):
    db.session.commit()
    return json_response(jsondata, status_code)


def text_response(data: str, status_code: int=200) -> Response:
    response = Response(data, mimetype='text/plain')
    response.status_code = status_code
    return response


def to_json_str(jsondata) -> str:
    return json.dumps(jsondata,
                      separators=(',', ':'),
                      cls=TimJsonEncoder)


def to_dict(jsondata):
    return json.loads(to_json_str(jsondata))


def no_cache_json_response(data: Any, date_conversion: bool = False) -> Response:
    """Returns a JSON response that prevents any caching of the result.
    """
    response = json_response(data, date_conversion=date_conversion)
    return add_no_cache_headers(response)


def add_no_cache_headers(response: Response):
    response.headers['Cache-Control'] = 'no-store, must-revalidate'
    return response


def ok_response() -> Response:
    return json_response({'status': 'ok'})


def empty_response():
    return json_response({'empty': True})


def csv_string(data, dialect: str, delimiter: str = ","):
    line = StringIO()
    try:
        writer = csv.writer(line, dialect=dialect, delimiter=delimiter)
    except csv.Error:
        writer = csv.writer(line)
    for csv_line in data:
        writer.writerow(csv_line)
    return line.getvalue()    # .strip('\r\n')  # let the last lf be there


def iter_csv(data, dialect: str, delimiter: str = ","):
    line = StringIO()
    try:
        writer = csv.writer(line, dialect=dialect, delimiter=delimiter)
    except csv.Error:
        writer = csv.writer(line)
    for csv_line in data:
        writer.writerow(csv_line)
        line.seek(0)
        yield line.read()
        line.truncate(0)
        line.seek(0)


def csv_response(data, dialect='excel', delimiter=','):
    return Response(stream_with_context(iter_csv(data, dialect, delimiter)), mimetype='text/plain')


def error_generic(error: str, code: int, template='error.html'):
    if 'text/html' in request.headers.get("Accept", ""):
        return render_template(template,
                               message=error,
                               code=code,
                               status=http.client.responses[code]), code
    else:
        return json_response({'error': error}, code)


def get_grid_modules():
    return [
        "ui.grid",
        "ui.grid.cellNav",
        "ui.grid.selection",
        "ui.grid.exporter",
        "ui.grid.autoResize",
        "ui.grid.saveState",
    ]
