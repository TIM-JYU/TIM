"""
Contains initialization of Celery distributed task queue and task functions.
Note: Add new tasks here. For scheduling add parameters to defaultconfig as well.
"""
import logging
from copy import copy
from logging import Logger
from typing import Any, Dict

from celery import Celery
from celery.signals import after_setup_logger
from celery.utils.log import get_task_logger

from timApp.answer.routes import post_answer_impl
from timApp.notification.notify import process_pending_notifications
from timApp.tim_app import app
from timApp.user.user import User
from timApp.util.flask.search import create_search_files

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
