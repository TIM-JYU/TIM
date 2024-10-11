"""
Contains initialization of Celery distributed task queue and task functions.
Note: Add new tasks here. For scheduling add parameters to defaultconfig as well.
"""
import contextlib
import json
import logging
from concurrent.futures import Future
from copy import copy
from datetime import timedelta
from logging import Logger
from typing import Any

from celery import Celery
from celery.signals import after_setup_logger
from celery.utils.log import get_task_logger
from flask import session
from marshmallow import EXCLUDE, ValidationError
from sqlalchemy import delete

from timApp.answer.routes import post_answer_impl, AnswerRouteResult
from timApp.auth.sessioninfo import clear_session
from timApp.document.usercontext import UserContext
from timApp.document.viewcontext import default_view_ctx
from timApp.notification.notify import process_pending_notifications
from timApp.plugin.containerLink import call_plugin_generic
from timApp.plugin.exportdata import WithOutData, WithOutDataSchema
from timApp.plugin.plugin import Plugin
from timApp.plugin.pluginexception import PluginException
from timApp.tim_app import app
from timApp.timdb.sqa import db, run_sql
from timApp.user.user import User
from timApp.user.verification.verification import Verification
from timApp.util.flask.search import create_search_files
from timApp.util.utils import get_current_time, collect_errors_from_hosts
from tim_common.vendor.requests_futures import FuturesSession

logger: Logger = get_task_logger(__name__)


def make_celery(appl):
    """
    Initializes Celery.

    :param appl: Flask app.
    :return: Celery.
    """
    cel = Celery(
        appl.import_name,
        backend=appl.config["CELERY_RESULT_BACKEND"],
        broker=appl.config["CELERY_BROKER_URL"],
    )
    cel.conf.update(appl.config)
    TaskBase = cel.Task

    class ContextTask(TaskBase):
        abstract = True

        def __call__(self, *args, **kwargs):
            with appl.app_context():
                return TaskBase.__call__(self, *args, **kwargs)

    cel.Task = ContextTask
    return cel


@after_setup_logger.connect
def on_after_setup_logger(**kwargs):
    global logger
    logger.setLevel(logging.INFO)
    logging.getLogger("celery").setLevel(logging.INFO)


celery = make_celery(app)


@celery.task(ignore_result=True)
def update_search_files():
    """
    Calls function to create title and content search files. Meant to be scheduled.
    """
    create_search_files()


@celery.task(ignore_result=True)
def process_notifications():
    """
    Processes pending notifications.
    """
    process_pending_notifications()


@celery.task(ignore_result=True)
def run_user_function(user_id: int, task_id: str, plugin_input: dict[str, Any]):
    do_run_user_function(user_id, task_id, plugin_input)


def do_run_user_function(user_id: int, task_id: str, plugin_input: dict[str, Any]):
    # We emulate a request context so that we can set a temporary user session
    # Some plugins depend on the document context, which in turn may depend on permissions (e.g. extraPreambles)
    with app.test_request_context():
        session["user_id"] = user_id
        next_runner = task_id
        encountered_runners = set()
        u = User.get_by_id(user_id)
        step = 0
        while next_runner:
            step += 1
            encountered_runners.add(next_runner)
            logger.info(f"Plugin run: {u.name}, {next_runner}, step {step}")
            result = post_answer_impl(
                next_runner, copy(plugin_input), {}, {}, u, (), [], None
            )

            try:
                wod: WithOutData = WithOutDataSchema.load(
                    result.result.get("web"), unknown=EXCLUDE
                )
            except ValidationError:
                pass
            else:
                handle_exportdata(result, u, wod)

            if output := result.result.get("web", {}).get("output"):
                logger.info(f"Plugin run: {u.name}, result: {output}")

            # The user-provided parameters go only to the first plugin. Others will get no parameters.
            plugin_input = {}

            next_runner = result.plugin.values.get("nextRunner")
            if isinstance(next_runner, str):
                next_runner = f"{result.plugin.task_id.doc_id}.{next_runner}"
                if next_runner in encountered_runners:
                    logger.warning(f"Cycle in runners: {encountered_runners}")
                    break
            elif next_runner is not None:
                logger.warning(f"Invalid type for nextRunner: {next_runner}")
                break


def handle_exportdata(result: AnswerRouteResult, u: User, wod: WithOutData) -> None:
    d = None
    for p in wod.outdata.exportdata:
        if not p.save:
            continue
        plug, d = Plugin.from_task_id(
            f"{result.plugin.task_id.doc_id}.{p.plugin}",
            user_ctx=UserContext.from_one_user(u),
            view_ctx=default_view_ctx,
            cached_doc=d,
        )

        # csPlugin always returns status 200 for (almost?) any request, so we must handle it separately.
        if plug.type != "csPlugin":
            try:
                res = call_plugin_generic(
                    plug.type,
                    "post",
                    "convertExportData",
                    json.dumps(p.data),
                    headers={"Content-type": "application/json"},
                )
            except PluginException as e:
                logger.error(f"convertExportData call failed: {e}")
                continue
            if res.status_code != 200:
                logger.error(f"convertExportData returned status {res.status_code}")
                continue
            else:
                converted = res.json()
        else:
            subtype = plug.values.get("type")
            if subtype == "chartjs":
                converted = {"c": p.data}
            else:
                logger.error(
                    f"convertExportData failed for csPlugin; unknown subtype: {subtype}"
                )
                continue

        post_answer_impl(plug.task_id.doc_task, converted, {}, {}, u, (), [], None)


@celery.task
def sync_user_group_memberships(email: str, user_memberships: list[str]):
    do_send_user_group_info(email, user_memberships)


def do_send_user_group_info(email: str, user_memberships: list[str]):
    logger.info(f"Sending user group info for {email}: {user_memberships}")
    sync_hosts = app.config["SYNC_USER_GROUPS_HOSTS"]
    sync_secret = app.config["SYNC_USER_GROUPS_SEND_SECRET"]
    session = FuturesSession()
    futures: list[Future] = []
    for host in sync_hosts:
        f = session.post(
            f"{host}/backup/user/memberships",
            json={
                "email": email,
                "secret": sync_secret,
                "memberships": user_memberships,
            },
        )
        futures.append(f)

    for f in futures:
        try:
            res = f.result()
        except Exception as e:
            logger.error(f"Failed to sync user group memberships: {e}")
        else:
            if res.status_code != 200:
                logger.error(
                    f"Failed to sync user group memberships: {res.status_code}: {res.text}"
                )


@celery.task
def send_unlock_op(
    email: str,
    target: list[str],
):
    from timApp.item.distribute_rights import UnlockOp
    from timApp.item.distribute_rights import register_op_to_hosts

    op = UnlockOp(type="unlock", email=email, timestamp=get_current_time())
    return register_op_to_hosts(op, target, is_receiving_backup=False)


@celery.task
def send_answer_backup(exported_answer: dict[str, Any]):
    return do_send_answer_backup(exported_answer)


def do_send_answer_backup(exported_answer: dict[str, Any]):
    backup_hosts = app.config["BACKUP_ANSWER_HOSTS"]
    session = FuturesSession()
    futures: list[Future] = []
    for h in backup_hosts:
        f = session.post(
            f"{h}/backup/answer",
            json={
                "answer": exported_answer,
                "token": app.config["BACKUP_ANSWER_SEND_SECRET"],
            },
        )
        futures.append(f)
    return collect_errors_from_hosts(futures, backup_hosts)


@celery.task(ignore_result=True)
def cleanup_verifications():
    """
    Remove old verifications to save on space.
    Verification expiration is defined by two variables:

    VERIFICATION_UNREACTED_CLEANUP_INTERVAL - cleanup interval for unverified verifications.
    VERIFICATION_REACTED_CLEANUP_INTERVAL - cleanup internval for reacted verifications.
    """
    max_unreacted_interval = app.config["VERIFICATION_UNREACTED_CLEANUP_INTERVAL"]
    max_reacted_interval = app.config["VERIFICATION_REACTED_CLEANUP_INTERVAL"]
    now = get_current_time()
    end_time_unreacted = now - timedelta(seconds=max_unreacted_interval)
    end_time_reacted = now - timedelta(seconds=max_reacted_interval)
    run_sql(
        delete(Verification).where(
            (Verification.requested_at < end_time_unreacted)
            & (Verification.reacted_at == None)
        )
    )
    run_sql(
        delete(Verification).where(
            (Verification.requested_at < end_time_reacted)
            & (Verification.reacted_at != None)
        )
    )

    db.session.commit()


@celery.task(ignore_result=True)
def cleanup_oauth2_tokens():
    """
    Remove expired OAuth2 tokens.

    While authlib prevents usage of expired tokens automatically,
    it does not clean up the database on token expiration.
    Moreover, some applications may not cache the token and instead might request it for every launch,
    which can cause tokens to accumulate.
    """
    from timApp.auth.oauth2.oauth2 import delete_expired_oauth2_tokens

    delete_expired_oauth2_tokens()


@celery.task(ignore_result=True)
def apply_pending_userselect_actions() -> None:
    from timApp.plugin.userselect.action_queue import apply_pending_actions_impl

    apply_pending_actions_impl()


@celery.task(ignore_result=True)
def relay_dist_rights(dist_json: str, dist_hosts: list[str]) -> None:
    from timApp.item.distribute_rights import relay_dist_rights_impl

    relay_dist_rights_impl(dist_json, dist_hosts)
