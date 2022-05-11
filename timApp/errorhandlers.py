import re
import shutil
import sys
import traceback
from dataclasses import dataclass
from functools import wraps
from typing import Callable, Any

from flask import request, render_template, session, flash, Flask, redirect
from markupsafe import Markup
from marshmallow import ValidationError
from sass import CompileError

from timApp.answer.answers import TooLargeAnswerException
from timApp.auth.accesshelper import AccessDenied, ItemLockedException
from timApp.auth.login import logout
from timApp.auth.session.util import SessionExpired
from timApp.auth.sessioninfo import get_current_user_object, clear_session
from timApp.document.docentry import DocEntry
from timApp.document.docsettings import get_minimal_visibility_settings
from timApp.folder.folder import Folder
from timApp.markdown.dumboclient import DumboHTMLException
from timApp.notification.send_email import send_email
from timApp.plugin.pluginexception import PluginException
from timApp.sisu.sisu import IncorrectSettings, SisuError
from timApp.timdb.exceptions import ItemAlreadyExistsException
from timApp.timdb.sqa import db
from timApp.user.settings.style_utils import get_default_scss_gen_dir
from timApp.user.userutils import NoSuchUserException, DeletedUserException
from timApp.util.flask.requesthelper import (
    JSONException,
    get_request_message,
    RouteException,
    NotExist,
)
from timApp.util.flask.responsehelper import error_generic, html_error
from timApp.util.logger import log_error
from timApp.util.utils import get_current_time, get_exception_code

ERROR_CODES_FOLDER = "error-codes"


@dataclass
class SuppressedError(Exception):
    msg: str


def suppress_wuff(
    ex_type: type[Exception], details_url: str, message_regex: str | None = None
) -> Callable:
    """
    Decorator to prevent sending email errors ("wuffs") on the specified error.

    The decorator is meant to suppress well-known errors that are marked for fixing and are not critical for TIM
    to function.
    When decorating a function, it is suggested to include a comment that links to a clear description of the problem.

    :param ex_type: Exception type to suppress
    :param details_url: URL to a page with details about the issue (e.g. issue URL)
    :param message_regex: RegEx to check messages exception messages against.
                            If specified, exception is also matched against the message.
    :return: Wrapped function
    """

    def decorator(f: Callable) -> Callable:
        @wraps(f)
        def wrapped(*args: Any, **kwargs: Any) -> Any:
            try:
                return f(*args, **kwargs)
            except Exception as e:
                if isinstance(e, ex_type):
                    ex_msg = str(e)
                    if message_regex and not re.search(message_regex, ex_msg):
                        raise
                    # Wrap the error to detect suppression in main error handler
                    raise SuppressedError(
                        f"The error was suppressed. Original message: {e}\nMore info: {details_url}"
                    ) from e
                raise

        return wrapped

    return decorator


def register_errorhandlers(app: Flask):
    @app.errorhandler(CompileError)
    def handle_sass_compile_error(error):
        # Generally, compile error is caused by bad style SCSS (e.g. after an update)
        # Right now, fix this by forcing the cache to regenerate and display a message with the error info
        shutil.rmtree(get_default_scss_gen_dir(), ignore_errors=True)
        return html_error(
            str(error),
            400,
            description="Failed to load page style. The website might have been restarted recently.",
        )

    @app.errorhandler(AccessDenied)
    def handle_access_denied(error: AccessDenied):
        msg = (
            str(error)
            if error.args
            else "Sorry, you don't have permission to access this resource."
        )
        return error_generic(msg, 403)

    @app.errorhandler(SessionExpired)
    def handle_session_expired(error: SessionExpired):
        return error_generic(
            "Your session has expired. Please log in again.",
            490,
            status="Session expired",
        )

    @app.errorhandler(NotExist)
    def handle_access_denied(error):
        return error_generic(error.description, 404)

    @app.errorhandler(PluginException)
    def handle_plugin_exception(error):
        return error_generic(str(error), 400)

    @app.errorhandler(ValidationError)
    def handle_validation_error(error):
        return error_generic(str(error), 400)

    @app.errorhandler(IncorrectSettings)
    def handle_validation_error(error):
        return error_generic(str(error), 400)

    @app.errorhandler(SisuError)
    def handle_validation_error(error):
        return error_generic(str(error), 400)

    @app.errorhandler(RouteException)
    def handle_json_exception(error: RouteException):
        return error_generic(error.description, error.code)

    @app.errorhandler(JSONException)
    def handle_json_exception(error: JSONException):
        return error_generic(error.description, error.code)

    @app.errorhandler(ItemAlreadyExistsException)
    def handle_already_exists(error: ItemAlreadyExistsException):
        return error_generic(str(error), 403)

    @app.errorhandler(DumboHTMLException)
    def handle_dumbo_html_exception(error: DumboHTMLException):
        return error_generic(error.description, 400, template="dumbo_html_error.jinja2")

    @app.errorhandler(ItemLockedException)
    def handle_item_locked(error: ItemLockedException):
        item = DocEntry.find_by_id(error.access.block_id)
        is_folder = False
        if not item:
            is_folder = True
            item = Folder.get_by_id(error.access.block_id)
        if not item:
            raise NotExist()
        view_settings = get_minimal_visibility_settings(
            item.document if not is_folder else None
        )
        return (
            render_template(
                "duration_unlock.jinja2",
                item=item,
                item_type="folder" if is_folder else "document",
                access=error.access,
                msg=error.msg,
                next_doc=error.next_doc,
                view_settings=view_settings,
            ),
            403,
        )

    @app.errorhandler(NoSuchUserException)
    def handle_user_not_found(error):
        if error.user_id == session["user_id"]:
            flash(
                f"Your user id ({error.user_id}) was not found in the database. Clearing session automatically."
            )
            return logout()
        return error_generic(error.description, 500)

    @app.errorhandler(DeletedUserException)
    def handle_user_deleted(error):
        clear_session()
        return redirect("/")

    @app.errorhandler(TooLargeAnswerException)
    def handle_too_large_answer(error: TooLargeAnswerException):
        return error_generic(str(error), 400)

    ##############
    # HTTP codes #
    ##############

    @app.errorhandler(400)
    def handle_400(error):
        return error_generic(error.description, 400)

    @app.errorhandler(403)
    def handle_403(error):
        return error_generic(error.description, 403)

    @app.errorhandler(404)
    def handle_404(error):
        return error_generic(error.description, 404)

    @app.errorhandler(413)
    def handle_413(error):
        error.description = (
            "Your file is too large to be uploaded. "
            + f'Maximum size is {app.config["MAX_CONTENT_LENGTH"] / 1024 / 1024} MB.'
        )
        return error_generic(error.description, 413)

    @app.errorhandler(422)
    def handle_422(error):
        msgs = error.data.get("messages")
        if msgs:
            error.description = str(msgs)
        return error_generic(error.description, 422)

    @app.errorhandler(500)
    def handle_500(error):
        # NOTE: Rollback must be the first operation here. Otherwise any db access might fail.
        db.session.rollback()
        req_msg = get_request_message(500, include_body=True)
        log_error(req_msg)
        help_email = app.config["HELP_EMAIL"]
        host = app.config["TIM_HOST"]
        error.description = Markup(
            "Something went wrong with the server, sorry. "
            "TIM developers have been notified about this. "
            "If the problem persists, please send email to "
            f'<a href="mailto:{help_email}">{help_email}</a>.'
        )
        _, ex, tb_obj = sys.exc_info()
        if isinstance(ex, SuppressedError):
            return error_generic(error.description, 500)
        tb_str = traceback.format_exc()
        error_code = get_exception_code(ex, tb_obj)
        message = f"""
Exception happened on {get_current_time()} at {request.url}

Exception database: {host}/view/{ERROR_CODES_FOLDER}/{error_code.lower()}

{req_msg}

{tb_str}
    """.strip()
        u = get_current_user_object()
        send_email(
            rcpt=app.config["ERROR_EMAIL"],
            subject=f"{host}: Error at {request.path} ({u.name})",
            mail_from=app.config["WUFF_EMAIL"],
            reply_to=f'{app.config["ERROR_EMAIL"]},{u.email}',
            msg=message,
        )
        return error_generic(error.description, 500)

    @app.errorhandler(503)
    def handle_503(error):
        return error_generic(error.description, 503)
