"""
Contains initialization of Celery distributed task queue and task functions.
Note: Add new tasks here. For scheduling add parameters to defaultconfig as well.
"""
import json
import logging
from concurrent.futures import Future
from copy import copy
from logging import Logger
from typing import Any, Dict, List

from celery import Celery
from celery.signals import after_setup_logger
from celery.utils.log import get_task_logger
from marshmallow import EXCLUDE, ValidationError

from timApp.answer.routes import post_answer_impl, AnswerRouteResult
from timApp.document.usercontext import UserContext
from timApp.document.viewcontext import default_view_ctx
from timApp.item.distribute_rights import UnlockOp, register_op_to_hosts
from timApp.notification.notify import process_pending_notifications
from timApp.plugin.containerLink import call_plugin_generic
from timApp.plugin.exportdata import WithOutData, WithOutDataSchema
from timApp.plugin.plugin import Plugin
from timApp.plugin.pluginexception import PluginException
from timApp.tim_app import app
from timApp.user.user import User
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
    cel = Celery(appl.import_name, backend=appl.config['CELERY_RESULT_BACKEND'],
                    broker=appl.config['CELERY_BROKER_URL'])
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
    logging.getLogger('celery').setLevel(logging.INFO)


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
def run_user_function(user_id: int, task_id: str, plugin_input: Dict[str, Any]):
    do_run_user_function(user_id, task_id, plugin_input)


def do_run_user_function(user_id: int, task_id: str, plugin_input: Dict[str, Any]):
    next_runner = task_id
    encountered_runners = set()
    u = User.get_by_id(user_id)
    step = 0
    while next_runner:
        step += 1
        encountered_runners.add(next_runner)
        logger.info(f'Plugin run: {u.name}, {next_runner}, step {step}')
        result = post_answer_impl(next_runner, copy(plugin_input), {}, {}, u, (), [], None)

        try:
            wod: WithOutData = WithOutDataSchema.load(result.result.get('web'), unknown=EXCLUDE)
        except ValidationError:
            pass
        else:
            handle_exportdata(result, u, wod)

        # The user-provided parameters go only to the first plugin. Others will get no parameters.
        plugin_input = {}

        next_runner = result.plugin.values.get('nextRunner')
        if isinstance(next_runner, str):
            next_runner = f'{result.plugin.task_id.doc_id}.{next_runner}'
            if next_runner in encountered_runners:
                logger.warning(f'Cycle in runners: {encountered_runners}')
                break
        elif next_runner is not None:
            logger.warning(f'Invalid type for nextRunner: {next_runner}')
            break


def handle_exportdata(result: AnswerRouteResult, u: User, wod: WithOutData) -> None:
    d = None
    for p in wod.outdata.exportdata:
        if not p.save:
            continue
        plug, d = Plugin.from_task_id(
            f'{result.plugin.task_id.doc_id}.{p.plugin}',
            user_ctx=UserContext.from_one_user(u),
            view_ctx=default_view_ctx,
            cached_doc=d,
        )

        # csPlugin always returns status 200 for (almost?) any request, so we must handle it separately.
        if plug.type != 'csPlugin':
            try:
                res = call_plugin_generic(
                    plug.type,
                    'post',
                    'convertExportData',
                    json.dumps(p.data),
                    headers={'Content-type': 'application/json'},
                )
            except PluginException as e:
                logger.error(f'convertExportData call failed: {e}')
                continue
            if res.status_code != 200:
                logger.error(f'convertExportData returned status {res.status_code}')
                continue
            else:
                converted = res.json()
        else:
            subtype = plug.values.get('type')
            if subtype == 'chartjs':
                converted = {'c': p.data}
            else:
                logger.error(f'convertExportData failed for csPlugin; unknown subtype: {subtype}')
                continue

        post_answer_impl(plug.task_id.doc_task, converted, {}, {}, u, (), [], None)


@celery.task
def send_unlock_op(
        email: str,
        target: List[str],
):
    op = UnlockOp(type='unlock', email=email, timestamp=get_current_time())
    return register_op_to_hosts(op, target, is_receiving_backup=False)


@celery.task
def send_answer_backup(
        exported_answer: Dict[str, Any]
):
    return do_send_answer_backup(exported_answer)


def do_send_answer_backup(exported_answer: Dict[str, Any]):
    backup_hosts = app.config["BACKUP_ANSWER_HOSTS"]
    session = FuturesSession()
    futures: List[Future] = []
    for h in backup_hosts:
        f = session.post(
            f'{h}/backup/answer',
            json={'answer': exported_answer, 'token': app.config['BACKUP_ANSWER_SEND_SECRET']},
        )
        futures.append(f)
    return collect_errors_from_hosts(futures, backup_hosts)
