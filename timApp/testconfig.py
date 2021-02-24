import logging
import os
from datetime import timedelta
from typing import List, Optional, Dict, TypedDict

from celery.schedules import crontab

from timApp.util.utils import pycharm_running

DEBUG = True
PROFILE = False
TIM_NAME = 'tim-test'
DB_URI = f"postgresql://postgres:postgresql@postgresql-test:5432/{TIM_NAME}"
FILES_PATH = '/tmp/doctest_files'
LOG_DIR = "/tmp/tim_logs"
LOG_FILE = "timLog.log"
LOG_LEVEL = logging.ERROR
LOG_LEVEL_STDOUT = logging.ERROR
LOG_PATH = os.path.join(LOG_DIR, LOG_FILE)
TESTING = True
SQLALCHEMY_DATABASE_URI = DB_URI

# Webassets seems to have a weird bug that it cannot find the cache files if the paths are not default,
# so we cannot modify them. And without the cache, running the tests is twice as slow.
# ASSETS_DIRECTORY = '/tmp/doctest_files'
# ASSETS_CACHE = '.webassets-cache'

SQLALCHEMY_POOL_SIZE = 50
SQLALCHEMY_MAX_OVERFLOW = 100
LAST_EDITED_BOOKMARK_LIMIT = 3
TRAP_HTTP_EXCEPTIONS = True
PROPAGATE_EXCEPTIONS = True
SELENIUM_BROWSER_URL = os.environ.get('SELENIUM_BROWSER_URL', 'http://caddy:' +
                                      ('81' if pycharm_running() else '82'))
LIVESERVER_PORT = 5001
QST_PLUGIN_PORT = LIVESERVER_PORT
PERMANENT_SESSION_LIFETIME = timedelta(weeks=9999)


class Schedule(TypedDict):
    task: str
    schedule: crontab


CELERYBEAT_SCHEDULE: Dict[str, Schedule] = {
    # don't schedule anything while testing
}
WTF_CSRF_METHODS: List[str] = []
SCIM_USERNAME = 't'
SCIM_PASSWORD = 'pass'
SISU_CERT_PATH: Optional[str] = None
HOME_ORGANIZATION = 'jyu.fi'

SAML_PATH = '/service/timApp/auth/saml/test'

SESSION_COOKIE_SECURE = False  # Test running does not have HTTPS, so secure cookie can't be used.
