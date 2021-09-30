"""
TIM plugin: a field to start other fileds
"""
from flask import Response

from .cbfield import cbfield_route
from .dropdown import dropdown_route
from .goaltable import goaltable_route
from .multisave import multisave_route
from .numericfield import numericfield_route
from tim_common.pluginserver_flask import jsonify, create_app, launch_if_main
from .rbfield import rbfield_route
from .textfield import textfield_route

app = create_app(__name__)

app.register_blueprint(rbfield_route)
app.register_blueprint(cbfield_route)
app.register_blueprint(textfield_route)
app.register_blueprint(numericfield_route)
app.register_blueprint(multisave_route)
app.register_blueprint(dropdown_route)
app.register_blueprint(goaltable_route)


@app.get("/reqs")
def reqs() -> Response:
    return jsonify(
        {
            "js": [],
            "multihtml": False,
            "css": [],
            "editor_tabs": [],
        },
    )


launch_if_main(__name__, app)
