import os

DEBUG = True
PROFILE = False
SQLALCHEMY_POOL_SIZE = 20
DEBUG_SQL = False
MINIMUM_SCHEDULED_FUNCTION_INTERVAL = 5

if "dev_mailman" in os.environ["COMPOSE_PROFILES"]:
    MESSAGE_LISTS_ENABLED = True
    MAILMAN_URL = "http://mailman-core:8001/3.1"
    MAILMAN_USER = "restadmin"
    MAILMAN_PASS = "restpass"