from flask import Flask, Blueprint, request, Response

from timApp.notification.send_email import sent_mails_in_testing
from timApp.util.flask.responsehelper import ok_response, json_response


def register_testing_routes(app: Flask) -> None:
    bp = Blueprint("testing", __name__, url_prefix="/testing")

    @bp.post("/config")
    def config() -> Response:
        json = request.get_json()
        if isinstance(json, dict):
            for k, v in json.items():
                app.config[k] = v
        return ok_response()

    @bp.get("/sentEmails")
    def get_sent_emails() -> Response:
        return json_response(sent_mails_in_testing)

    app.register_blueprint(bp)
