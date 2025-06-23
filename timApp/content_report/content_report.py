from flask import Response

from timApp.util.flask.responsehelper import ok_response
from timApp.notification.send_email import send_email
from timApp.tim_app import app

from timApp.util.flask.typedblueprint import TypedBlueprint
from timApp.util.utils import get_current_time

content_report = TypedBlueprint("content_report", __name__, url_prefix="")


@content_report.post("/report")
def report_content(
    reason: str,
    reportedUrl: str | None = None,
    email: str | None = None,
) -> Response:
    now = get_current_time()

    report_message = f"""
    User Submitted Content Report for a TIM Page

    User has reported harmful or inappropriate content in the page: {reportedUrl}.

    Description: {reason}

    {'User wishes a follow up email to address: ' + email if email else 'Email not supplied'}

    Report created at: {now}
    """

    user_mail = "" if email is None else email
    reply_addresses = ",".join([app.config["CONTENT_REPORT_EMAIL"], user_mail])

    send_email(
        rcpt=app.config["CONTENT_REPORT_EMAIL"],
        subject="New Content Report",
        mail_from=app.config["MAIL_FROM"],
        reply_to=reply_addresses,
        msg=report_message,
    )
    return ok_response()
