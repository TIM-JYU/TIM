"""Defines routes for handling a global notification message that is visible to all users until it is removed or
TIM is restarted.
"""
import os

from flask import Blueprint, Response
from flask import current_app
from flask import url_for

from timApp.auth.accesshelper import verify_admin_no_ret, is_allowed_ip
from timApp.auth.sessioninfo import logged_in
from timApp.markdown.markdownconverter import md_to_html
from timApp.util.flask.responsehelper import safe_redirect

global_notification = Blueprint(
    "global_notification", __name__, url_prefix="/globalNotification"
)

global_notification.before_request(verify_admin_no_ret)


@global_notification.app_context_processor
def inject_global_notifications() -> dict:
    """ "Injects global notification message (if the file exists) to all templates."""
    notifications = []
    try:
        with open(current_app.config["GLOBAL_NOTIFICATION_FILE"]) as f:
            notifications.append(("global-message", f.read()))
    except FileNotFoundError:
        pass
    if not logged_in() and not is_allowed_ip():
        ip_block_msg = current_app.config["IP_BLOCK_MESSAGE"]
        if ip_block_msg:
            notifications.append(("ip-block-message", ip_block_msg))
    return dict(global_notifications=notifications)


@global_notification.get("/set/<path:message>")
def set_global_notification(message: str) -> Response:
    with open(
        current_app.config["GLOBAL_NOTIFICATION_FILE"], "wt", encoding="utf8"
    ) as f:
        f.write(md_to_html(message))
    return safe_redirect(url_for("start_page"))


@global_notification.get("/remove")
def remove_global_notification() -> Response:
    try:
        os.remove(current_app.config["GLOBAL_NOTIFICATION_FILE"])
    except FileNotFoundError:
        pass
    return safe_redirect(url_for("start_page"))
