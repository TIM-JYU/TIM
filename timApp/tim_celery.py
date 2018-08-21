from celery import Celery

from timApp.tim_app import app
from timApp.util.flask.search import create_search_files


def make_celery(app):
    celery = Celery(app.import_name, backend=app.config['CELERY_RESULT_BACKEND'],
                    broker=app.config['CELERY_BROKER_URL'])
    celery.conf.update(app.config)
    TaskBase = celery.Task

    class ContextTask(TaskBase):
        abstract = True

        def __call__(self, *args, **kwargs):
            with app.app_context():
                return TaskBase.__call__(self, *args, **kwargs)

    celery.Task = ContextTask
    return celery


celery = make_celery(app)


@celery.task
def update_search_files():
    """
    Calls function to create title and content search files. Meant to be scheduled.
    :return: Response, currently dummy value.
    """
    print("Beginning 'create_search_files'...")
    msg = create_search_files()
    print(msg)
    return True
