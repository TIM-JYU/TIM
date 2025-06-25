from flask import Response, request

from timApp.util.flask.responsehelper import json_response
from timApp.notification.send_email import send_email
from timApp.tim_app import app

from timApp.util.flask.typedblueprint import TypedBlueprint
from timApp.util.utils import get_current_time, is_valid_email
from tim_common.html_sanitize import sanitize_html

content_report = TypedBlueprint("content_report", __name__, url_prefix="")


@content_report.post("/report")
def report_content(
    reason: str,
    reportedUrl: str | None = None,
    email: str | None = None,
) -> Response:
    now = get_current_time()

    if not (email is None or email == "") and not is_valid_email(email):
        return json_response({"status": "error", "description": "invalid_email"})

    # If given, the url address should start with the host name
    if not (
        reportedUrl is None or reportedUrl == ""
    ) and not reportedUrl.strip().startswith(request.host_url):
        return json_response(
            {
                "status": "error",
                "description": "invalid_url",
                "host": request.host_url,
                "reportedUrl": reportedUrl,
            }
        )

    sanitized_reason = sanitize_html(reason)

    report_message = f"""
    User Submitted Content Report for a TIM Page

    User has reported harmful or inappropriate content in the page: {reportedUrl}.

    Description: {sanitized_reason}

    {'User wishes a follow up email to address: ' + email if email else 'Email not supplied'}

    Report created at: {now}
    """

    user_reply = "" if email is None else email
    reply_addresses = ",".join([app.config["CONTENT_REPORT_EMAIL"], user_reply])

    send_email(
        rcpt=app.config["CONTENT_REPORT_EMAIL"],
        subject="New Content Report",
        mail_from=app.config["MAIL_FROM"],
        reply_to=reply_addresses,
        msg=report_message,
    )

    return json_response({"status": "ok"})
