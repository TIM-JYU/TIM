from flask import Response, Blueprint

from timApp.util.flask.requesthelper import use_model
from timApp.util.flask.responsehelper import ok_response
from timApp.util.flask.typedblueprint import TypedBlueprint
from dataclasses import dataclass, field
from timApp.notification.send_email import send_email
from timApp.tim_app import app

from marshmallow import Schema, fields

content_report = Blueprint("content_report", __name__, url_prefix="")


@dataclass
class ContentReport:
    reason: str
    name: str
    email: str
    reportedUrl: str


@content_report.post("/report")
@use_model(ContentReport)
def report_content(report: ContentReport) -> Response:
    report_message = f"""
    User Submitted Content Report for a TIM Page
    
    User has reported harmful or inappropriate content in the page: {report.reportedUrl}.

    Description: {report.reason}
    
    {'User name: ' + report.name if report.name else 'Name not supplied'}.

    {'User wishes a followup email to address: ' + report.email if report.email else 'Email not supplied'}

    """

    mail_subject = f"Content Report for {report.reportedUrl}"
    user_email = report.email if report.email else ""
    reply_addresses = f'{app.config["HELP_EMAIL"]}, ' + user_email

    send_email(
        rcpt=app.config["HELP_EMAIL"],
        subject=mail_subject,
        mail_from=app.config["MAIL_FROM"],
        reply_to=reply_addresses,
        msg=report_message,
    )
    return ok_response()
