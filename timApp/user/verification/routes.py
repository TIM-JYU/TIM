from typing import Optional

from flask import render_template
from sqlalchemy.orm.exc import NoResultFound, MultipleResultsFound  # type: ignore

from timApp.auth.accesshelper import verify_logged_in
from timApp.auth.sessioninfo import get_current_user_id
from timApp.user.verification.verification import Verification, VerificationType
from timApp.util.flask.requesthelper import RouteException
from timApp.util.flask.typedblueprint import TypedBlueprint

verify = TypedBlueprint('verify', __name__, url_prefix='/verify')


@verify.get("/<verify_type>/<verify_token>")
def show_verify_page(verify_type: str, verify_token: str):
    verify_logged_in()
    verify_type_parsed = VerificationType.parse(verify_type)
    error = None
    if not verify_type_parsed:
        error = "Invalid verification type"

    verification_obj: Optional[Verification] \
        = Verification.query.filter_by(token=verify_token, type=verify_type_parsed).first()

    if not verification_obj:
        error = "No verification found for the given data"

    if verification_obj and verification_obj.user_id != get_current_user_id():
        error = "You are not authorized to verify this action"
        verification_obj = None

    return render_template("user_action_verification.jinja2",
                           verify_error=error,
                           verify_type=verify_type,
                           verify_info=verification_obj)


@verify.post("/<verify_type>/<verify_token>")
def do_verify(verify_type: str, verify_token: str):
    raise RouteException("TODO")
