from marshmallow import Schema, fields, pre_load
from flask import Response, Blueprint, request
from marshmallow import validate, fields, post_load

from timApp.util.flask.requesthelper import use_model
from timApp.util.flask.responsehelper import ok_response
from dataclasses import dataclass, field
from timApp.notification.send_email import send_email
from timApp.tim_app import app
import datetime as dt

content_report = Blueprint("content_report", __name__, url_prefix="")


# @dataclass
# class ContentReport:
#     reason: str
#     name: str
#     # email: str = field(metadata={"validate": validate.Email()})
#     email: str = field(
#         metadata={
#             "marshmallow_field": marshmallow.fields.Email(allow_none=True),
#         }
#     )
#     reportedUrl: str = field(metadata={"validate": validate.URL()})
#     # reportedUrl: str = field(
#     #     metadata={
#     #         "marshmallow_field": marshmallow.fields.URL(schemes=("http", "https")),
#     #     }
#     # )
#     createdAt: dt.datetime = field(
#         default_factory=lambda: dt.datetime.now(tz=dt.timezone.utc)
#     )


@dataclass
class ContentReport:
    name: str
    reason: str
    email: str
    reportedUrl: str
    createdAt: dt.datetime = field(
        default_factory=lambda: dt.datetime.now(tz=dt.timezone.utc)
    )


class ReportSchema(Schema):
    name: str = fields.Str(allow_none=True)
    reason: str = fields.Str(required=True)
    email: str = fields.Email(allow_none=True, missing=None)
    reportedUrl = fields.Url(allow_none=True)
    createdAt: dt.datetime = fields.DateTime(
        default=lambda: dt.datetime.now(tz=dt.timezone.utc)
    )

    @pre_load
    def empty_email_to_none(self, data, **kwargs):
        if "email" in data and data["email"] == "":
            data["email"] = None
        return data

    @post_load
    def make_content_report(self, data, **kwargs):
        return ContentReport(**data)


content_report_schema = ReportSchema()


@content_report.post("/report")
def report_content() -> Response:
    report = content_report_schema.load(request.json)

    report_message = f"""
    User Submitted Content Report for a TIM Page

    User has reported harmful or inappropriate content in the page: {report.reportedUrl}.

    Description: {report.reason}

    {'User name: ' + report.name if report.name else 'Name not supplied'}.

    {'User wishes a follow up email to address: ' + report.email if report.email else 'Email not supplied'}

    Report created at: {report.createdAt}
    """

    user_mail = "" if report.email is None else report.email
    reply_addresses = ",".join([app.config["CONTENT_REPORT_EMAIL"], user_mail])

    send_email(
        rcpt=app.config["CONTENT_REPORT_EMAIL"],
        subject="New Content Report",
        mail_from=app.config["MAIL_FROM"],
        reply_to=reply_addresses,
        msg=report_message,
    )
    return ok_response()
