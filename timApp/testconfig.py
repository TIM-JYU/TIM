import logging
import os

DEBUG = True
PROFILE = False
PLUGIN_CONNECTIONS = "nginx"
DATABASE = "postgresql://postgres@postgresql:5432/tempdb_" + 'timtest'
FILES_PATH = '/tmp/doctest_files'
LOG_DIR = "/tmp/tim_logs"
LOG_FILE = "timLog.log"
LOG_LEVEL = logging.ERROR
LOG_LEVEL_STDOUT = logging.ERROR
LOG_PATH = os.path.join(LOG_DIR, LOG_FILE)
TESTING = True
TIM_NAME = 'timtest'
OLD_SQLITE_DATABASE = None
SQLALCHEMY_BINDS = {
    'tim_main': DATABASE,
    'tempdb': "postgresql://postgres@postgresql:5432/tempdb_" + TIM_NAME
}
SASS_GEN_PATH = 'testgen'

# Webassets seems to have a weird bug that it cannot find the cache files if the paths are not default,
# so we cannot modify them. And without the cache, running the tests is twice as slow.
# ASSETS_DIRECTORY = '/tmp/doctest_files'
# ASSETS_CACHE = '.webassets-cache'
