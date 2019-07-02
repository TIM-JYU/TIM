"""
Routes for timMenu plugin.
"""

from flask import Blueprint, request
from timApp.util.flask.responsehelper import json_response

timMenu_plugin = Blueprint('timMenu_plugin',
                       __name__,
                       url_prefix='')

@timMenu_plugin.route("reqs/")
@timMenu_plugin.route("reqs")
def reqs():
    reqs = {
        "type": "embedded",
        "js": [
            # "tim/controllers/timMenuController",
        ],
        "multihtml": True,
        "multimd": True,
        "default_automd": True,
    }
    return json_response(reqs)

@timMenu_plugin.route("multihtml/", methods=["POST"])
def timMenu_multihtml():
    jsondata = request.get_json()
    multi = []
    for jso in jsondata:
        multi.append(timMenu_multihtml(jso, is_review(request)))
    return json_response(multi)

def is_review(request):
    result = request.full_path.find("review=") >= 0
    return result

# class to enable direct calls from TIM container
class TimMenu:
    def __init__(self):
        pass

    @staticmethod
    def multihtml_direct_call(jsondata):
        return timMenu_multihtml(jsondata)