"""
TIM plugin: a field to start other fileds
"""
from pluginserver_flask import GenericHtmlSchema, create_app, jsonify
from rbfield import rbfield_route
from cbfield import cbfield_route
from textfield import textfield_route
from numericfield import numericfield_route
from multisave import multisave_route
from dropdown import dropdown_route
from goaltable import goaltable_route


app = create_app(__name__, GenericHtmlSchema())

app.register_blueprint(rbfield_route)
app.register_blueprint(cbfield_route)
app.register_blueprint(textfield_route)
app.register_blueprint(numericfield_route)
app.register_blueprint(numericfield_route)
app.register_blueprint(multisave_route)
app.register_blueprint(dropdown_route)
app.register_blueprint(goaltable_route)


@app.route('/reqs/')
@app.route('/reqs')
def reqs():
    return jsonify({
        "js": [],
        "multihtml": False,
        "css": [],
        'editor_tabs': [],
    },
    )


if __name__ == '__main__':
    app.run(
        host='0.0.0.0',
        port=5000,
        debug=False,  # for live reloading, this can be turned on
    )
