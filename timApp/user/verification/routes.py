from typing import Optional, Tuple

from flask import render_template, Response
from sqlalchemy.orm.exc import NoResultFound, MultipleResultsFound  # type: ignore

from timApp.auth.accesshelper import verify_logged_in
from timApp.auth.sessioninfo import get_current_user_id
from timApp.timdb.sqa import db
from timApp.user.verification.verification import Verification, VerificationType
from timApp.util.flask.requesthelper import RouteException
from timApp.util.flask.responsehelper import ok_response
from timApp.util.flask.typedblueprint import TypedBlueprint
from timApp.util.utils import get_current_time

verify = TypedBlueprint('verify', __name__, url_prefix='/verify')


def get_verification_data(verify_type: str, verify_token: str) -> Tuple[Optional[Verification], Optional[str]]:
    verify_logged_in()
    verify_type_parsed = VerificationType.parse(verify_type)
    error = None
    if not verify_type_parsed:
        error = "Invalid verification type"

    verification: Optional[Verification] \
        = Verification.query.filter_by(token=verify_token, type=verify_type_parsed).first()

    if not verification:
        error = "No verification found for the given data"

    if verification and verification.user_id != get_current_user_id():
        error = "You are not authorized to verify this action"
        verification = None

    if verification and verification.reacted_at:
        error = "This verification request is no longer valid"
        verification = None

    return verification, error


@verify.get("/<verify_type>/<verify_token>")
def show_verify_page(verify_type: str, verify_token: str) -> str:
    verification, error = get_verification_data(verify_type, verify_token)
    return render_template("user_action_verification.jinja2",
                           verify_error=error,
                           verify_type=verify_type,
                           verify_info=verification)


@verify.post("/<verify_type>/<verify_token>")
def do_verify(verify_type: str, verify_token: str, verify: bool) -> Response:
    verification, error = get_verification_data(verify_type, verify_token)
    if error:
        raise RouteException(error)

    if verify:
        verification.approve()
    else:
        verification.deny()

    verification.reacted_at = get_current_time()
    db.session.commit()
    return ok_response()
