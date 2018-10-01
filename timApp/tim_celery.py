"""
Contains initialization of Celery distributed task queue and task functions.
Note: Add new tasks here. For scheduling add parameters to defaultconfig as well.
"""

from celery import Celery

from timApp.notification.notify import process_pending_notifications
from timApp.tim_app import app
from timApp.util.flask.search import create_search_files


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


celery = make_celery(app)


@celery.task
def update_search_files():
    """
    Calls function to create title and content search files. Meant to be scheduled.
    """
    print("Started updating search cache.")
    create_search_files()
    print('Finished updating search cache.')
    return True


@celery.task
def process_notifications():
    """
    Processes pending notifications.
    """
    print("Processing pending notifications.")
    process_pending_notifications()
    print('Finished processing pending notifications.')
    return True
