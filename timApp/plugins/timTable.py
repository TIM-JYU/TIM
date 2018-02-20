import json
import re
from typing import Dict, Any, NamedTuple, Optional
from xml.sax.saxutils import quoteattr

import yaml
from flask import Blueprint
from flask import Response
from flask import abort
from flask import request

from timApp.containerLink import convert_md
from timApp.plugin import Plugin, PluginException
from timApp.plugin import get_num_value
from timApp.plugin import get_value
from timApp.requesthelper import verify_json_params
from timApp.responsehelper import json_response
from timApp.sessioninfo import get_current_user_object
from timApp.timdb.docinfo import DocInfo
from timApp.timdb.models.askedjson import normalize_question_json

timTable_plugin = Blueprint('timTable_plugin',
                       __name__,
                       url_prefix='/timTable/')  # TODO: Better URL prefix.

@timTable_plugin.route("reqs/")
@timTable_plugin.route("reqs")
def timTable_reqs():
    reqs = {
        "type": "Hello World",
        "js": [
            # "tim/controllers/qstController",
               ],
        "angularModule": [],
        "multihtml": True,
        "multimd": True
    }

    return json_response(reqs)


@timTable_plugin.route("multihtml/", methods=["POST"])
def timTable_multihtml():
    jsondata = request.get_json()
    multi = []
    #lol = jsondata['markup']
    for jso in jsondata:
        lol = jso['markup'].get('lol')
        multi.append('<p>hei '+lol+'</p>')
    return json_response(multi)