import json
from typing import Any, Dict
from xml.sax.saxutils import quoteattr

from flask import Blueprint, Response
from flask import request

from timApp.tim_app import csrf
from timApp.util.flask.responsehelper import json_response

tape_plugin = Blueprint('tape_plugin',
                            __name__,
                            url_prefix='/tape/')


@tape_plugin.get("reqs")
def tape_reqs() -> Response:
    reqs = {
        "type": "embedded",
        "js": ["tape"],
        "multihtml": True,
        "multimd": False,
        "default_automd": False,
    }
    return json_response(reqs)


@tape_plugin.post("multihtml")
@csrf.exempt
def tape_multihtml() -> Response:
    """
    Route for getting the HTML of all tape plugins in a document.
    :return:
    """
    jsondata = request.get_json()
    multi = []
    for jso in jsondata:
        multi.append(tape_get_html(jso))
    return json_response(multi)


def tape_get_html(jso: Dict[str, Any]) -> str:
    """
    Returns the HTML of a single tape paragraph.
    :param jso:
    :param review:
    :return:
    """
    values = jso['markup']
    attrs = json.dumps(values)
    runner = 'tim-tape'
    s = f'<{runner} data={quoteattr(attrs)}></{runner}>'
    return s

