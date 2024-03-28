"""
Routes for managing and using login codes
"""

import random
from datetime import datetime
from time import sleep

from flask import Response
from flask import session
from sqlalchemy import select, Row
from sqlalchemy.orm import joinedload

from timApp.auth.accesshelper import AccessDenied, verify_ip_ok
from timApp.auth.login import save_came_from, set_user_to_session, login_user_data
from timApp.auth.logincodes.model import UserLoginCode
from timApp.auth.sessioninfo import clear_session
from timApp.tim_app import app
from timApp.timdb.exceptions import TimDbException
from timApp.timdb.sqa import db, run_sql
from timApp.user.usergroup import UserGroup
from timApp.util.flask.responsehelper import json_response
from timApp.util.flask.typedblueprint import TypedBlueprint
from timApp.util.locale import update_locale_lang
from timApp.util.logger import log_warning
from timApp.util.utils import get_current_time

login_codes = TypedBlueprint("login_codes", __name__, url_prefix="/loginCode")
"""
Blueprint for login code routes.
"""


@app.before_request
def verify_login_code() -> None:
    """
    Verify if the login code is still valid.
    """
    if not app.config["LOGIN_CODES_ENABLED"]:
        return
    login_code_session = session.get("login_code_session")
    if not login_code_session:
        return
    res: Row[tuple[datetime, bool]] | None = run_sql(
        select(UserLoginCode.active_to, UserLoginCode.valid).filter(
            UserLoginCode.session_code == login_code_session
        )
    ).one_or_none()

    if res is None:
        clear_session()
        return

    inactive_after, valid = res
    if not valid or get_current_time() > inactive_after:
        clear_session()


@login_codes.post("/login")
def login(login_code: str) -> Response:
    """
    Login with a login code.

    :param login_code: Login code to log in with.
    :return: Response with the login data.
    """
    if not app.config["LOGIN_CODES_ENABLED"]:
        raise AccessDenied("Login codes are not enabled")
    save_came_from()
    res = json_response(logincode_login(login_code))
    res = update_locale_lang(res)
    return json_response(res)


def logincode_login(login_code: str) -> dict:
    """
    Login with a login code.

    :param login_code: Login code to log in with.
    :return: Login result.
    """
    now = get_current_time()
    user_logincode: UserLoginCode | None = db.session.get(
        UserLoginCode, login_code, options=[joinedload(UserLoginCode.user)]
    )

    error_msg = ""
    if user_logincode is None:
        log_warning(f"Invalid login code: {login_code}")
        error_msg = "InvalidLoginCode"
    elif not user_logincode.active_from or now < user_logincode.active_from:
        log_warning(f"Login code not yet active: {login_code}")
        error_msg = "LoginCodeNotYetActive"
    elif now > user_logincode.active_to:
        log_warning(f"Login code expired: {login_code}")
        error_msg = "LoginCodeExpired"
    elif not user_logincode.valid:
        log_warning(f"Login code not valid: {login_code}")
        error_msg = "LoginCodeNotValid"

    if user_logincode and not error_msg:
        user = user_logincode.user
        try:
            user.get_personal_group()
        except TimDbException:
            ug = UserGroup(name=user.name)
            user.groups.append(ug)
            db.session.commit()
        verify_ip_ok(user)

        set_user_to_session(user)
        session["login_code_session"] = user_logincode.session_code

        db.session.commit()
        return login_user_data()
    else:
        # Sleep a random time to slow down brute force attacks
        sleep(random.random() * 3 + 1)

    raise AccessDenied(error_msg)
