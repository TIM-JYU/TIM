from dataclasses import dataclass

from flask import Response

from timApp.auth.sessioninfo import get_current_user_object
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
class MyPluginHmlModel(GenericHtmlModel[MyPluginInputModel, MyPluginMarkUp, MyPluginStateModel]):

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
