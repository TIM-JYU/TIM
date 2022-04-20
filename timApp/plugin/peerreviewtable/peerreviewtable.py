from dataclasses import dataclass

from flask import Response
from collections import defaultdict
from timApp.user.user import User
from timApp.auth.accesshelper import get_doc_or_abort
from timApp.auth.sessioninfo import get_current_user_object
from timApp.peerreview.peerreview_utils import get_all_reviews
from timApp.util.flask.responsehelper import json_response
from timApp.util.flask.typedblueprint import TypedBlueprint
from tim_common.markupmodels import GenericMarkupModel
from tim_common.marshmallow_dataclass import class_schema
from tim_common.pluginserver_flask import GenericHtmlModel, PluginReqs, register_html_routes

my_plugin = TypedBlueprint("my_plugin", __name__, url_prefix="/myplugin")

@dataclass
class MyPluginMarkUp(GenericMarkupModel):
    pass

#1h 14min
# Tallennetaan vastauksen tallennuksen yhteydessä
@dataclass
class MyPluginStateModel:
    pass


#1h 16min
# Mitä selain lähettää palvelimelle käyttäjä tallentaa vastauksen
@dataclass()
class MyPluginInputModel:
    pass

@dataclass()
class MyPluginHmlModel(GenericHtmlModel[MyPluginInputModel,MyPluginMarkUp, MyPluginStateModel]):

    def get_component_html_name(self) -> str:
        return 'tim-peerreview-table'

    def get_static_html(self) -> str:
        return """
        <div id="peerreview-container> </div>
        """

def reqs_handle() -> PluginReqs:
    return {
        "js": ["peerReviewTable"],
        "multihtml": True
    }


@my_plugin.get("/<int:doc_id>/test")
def test(doc_id: int) -> Response:
    d = get_doc_or_abort(doc_id)
    reviewers = get_all_reviews(d)
    result = list(map(lambda r: r.to_json, reviewers))
    for r in result:
        r['reviewer'] = User.get_by_id(r.get("reviewer_id")).pretty_full_name()
        r['reviewable'] = []
        r['reviewable'].append({
            "name": User.get_by_id(r.get("reviewable_id")).pretty_full_name(),
            "id": r["reviewable_id"]
        })
        r.pop("reviewable_id", None)
        r.pop("reviewer_id", None)
        r.pop("answer_id", None)
        r.pop("block_id", None)
        r.pop("end_time", None)
        r.pop("start_time", None)
        r.pop("task_name", None)

    listt = []
    print(result)
    for r in result:
        if any([r['reviewer'] in d.keys() for d in listt]):
            listt.append(r)
        else:
            el = list(filter(lambda e: e['reviewer'] == e['reviewer'], listt))
            print(el)

    return json_response(listt)

#/myplugin/reviews
@my_plugin.get("/reviews")
def get_reviews() -> Response:
    user = get_current_user_object()
    return json_response(
        {
            "result": [
                {
                    "name": "Mikki Hiiri",
                    "reviewers": ["Aku Ankka", "Minni Hiiri"],
                },
                {
                    "name": "Aku Ankka",
                    "reviewers": ["Hessu Hopo", "Pelle Peloton"],
                },
                {
                    "name": "Pelle Peloton",
                    "reviewers": ["Tupu Ankka", "Hupu Ankka"],
                }
            ]
        }
    )


register_html_routes(
    my_plugin,
    class_schema(MyPluginHmlModel),
    reqs_handle
)
