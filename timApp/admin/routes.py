import os

from flask import flash, url_for, Response
from sqlalchemy import select

from timApp.auth.accesshelper import verify_admin
from timApp.auth.sessioninfo import get_restored_context_user
from timApp.timdb.sqa import db, run_sql
from timApp.user.user import User
from timApp.user.usergroup import UserGroup
from timApp.util.flask.responsehelper import safe_redirect, json_response
from timApp.util.flask.typedblueprint import TypedBlueprint

admin_bp = TypedBlueprint("admin", __name__, url_prefix="")


@admin_bp.route("/exception", methods=["GET", "POST", "PUT", "DELETE"])
def throw_ex(db_error: bool = False) -> Response:
    verify_admin()
    if db_error:
        db.session.add(UserGroup(name="test"))
        db.session.add(UserGroup(name="test"))
        db.session.flush()
        raise Exception("Flush should have raised an exception!")
    raise Exception(
        "This route throws an exception intentionally for testing purposes."
    )


gunicorn_pid_path = "/var/run/gunicorn/gunicorn.pid"


@admin_bp.get("/restart")
def restart_server() -> Response:
    """Restarts the server by sending HUP signal to Gunicorn."""
    verify_admin()
    if os.path.exists(gunicorn_pid_path):
        os.system(f"kill -HUP $(cat {gunicorn_pid_path})")
        flash("Restart signal was sent to Gunicorn.")
    else:
        flash(
            "Gunicorn PID file was not found. TIM was probably not started with Gunicorn."
        )
    return safe_redirect(url_for("start_page"))


@admin_bp.get("/users/search/<term>")
def search_users(term: str, exact_match: bool = False, full: bool = False) -> Response:
    curr_user = get_restored_context_user()
    verify_admin(user=curr_user)
    q = select(User).order_by(User.id)
    if exact_match:
        q = q.filter((User.name == term) | (User.email == term))
    else:
        q = q.filter(
            User.name.ilike(f"%{term}%")
            | User.real_name.ilike(f"%{term}%")
            | User.email.ilike(f"%{term}%")
        )
    result: list[User] = run_sql(q).scalars().all()
    return json_response([u.to_json(contacts=True, full=full) for u in result])
