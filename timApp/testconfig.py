import logging
import os

from utils import pycharm_running

DEBUG = True
PROFILE = False
TIM_NAME = 'tim-test'
DATABASE = "postgresql://postgres@postgresql:5432/{0}".format(TIM_NAME)
FILES_PATH = '/tmp/doctest_files'
LOG_DIR = "/tmp/tim_logs"
LOG_FILE = "timLog.log"
LOG_LEVEL = logging.ERROR
LOG_LEVEL_STDOUT = logging.ERROR
LOG_PATH = os.path.join(LOG_DIR, LOG_FILE)
TESTING = True
OLD_SQLITE_DATABASE = None
SQLALCHEMY_BINDS = {
    'tim_main': DATABASE,
    'tempdb': "postgresql://postgres@postgresql-tempdb:5432/tempdb_{0}".format(TIM_NAME)
}
SASS_GEN_PATH = 'testgen'

# Webassets seems to have a weird bug that it cannot find the cache files if the paths are not default,
# so we cannot modify them. And without the cache, running the tests is twice as slow.
# ASSETS_DIRECTORY = '/tmp/doctest_files'
# ASSETS_CACHE = '.webassets-cache'

SQLALCHEMY_POOL_SIZE = 50
LAST_EDITED_BOOKMARK_LIMIT = 3
TRAP_HTTP_EXCEPTIONS = True
PROPAGATE_EXCEPTIONS = True
SELENIUM_REMOTE_URL = os.environ.get('SELENIUM_REMOTE_URL', 'http://chrome')
SELENIUM_BROWSER_URL = os.environ.get('SELENIUM_BROWSER_URL', 'http://' +
                                      ('tim' if pycharm_running() else 'tests'))
LIVESERVER_PORT = 5001
QST_PLUGIN_PORT = LIVESERVER_PORT
