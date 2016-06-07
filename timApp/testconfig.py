import os

CONTRACTS_ENABLED = True
DEBUG = True
PROFILE = False
PLUGIN_CONNECTIONS = "nginx"
DATABASE = '/tmp/doctest_files/tim.db'
FILES_PATH = '/tmp/doctest_files'
LOG_DIR = "/tmp/tim_logs"
LOG_FILE = "timLog.log"
LOG_PATH = os.path.join(LOG_DIR, LOG_FILE)
TESTING = True
TIM_NAME = 'timtest'
SQLALCHEMY_BINDS = {
    'tim_main': 'sqlite:///' + DATABASE,
    'tempdb': "postgresql://docker:docker@postgre:5432/tempdb_" + TIM_NAME
}
