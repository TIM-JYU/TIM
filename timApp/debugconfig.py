import os

DEBUG = True
PROFILE = False
SQLALCHEMY_POOL_SIZE = 20
DEBUG_SQL = False
MINIMUM_SCHEDULED_FUNCTION_INTERVAL = 5

if os.environ.get("RUN_MAILMAN_DEV", "0") == "1":
    MESSAGE_LISTS_ENABLED = True
    MAILMAN_URL = "http://mailman-core:8001/3.1"
    MAILMAN_USER = "restadmin"
    MAILMAN_PASS = "restpass"