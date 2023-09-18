import logging
import os
from datetime import timedelta
from typing import TypedDict

from celery.schedules import crontab

DEBUG = True
PROFILE = False
TIM_NAME = "tim-test"
DB_URI = f"postgresql://postgres:postgresql@postgresql-test:5432/{TIM_NAME}"
FILES_PATH = "/tmp/doctest_files"
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

LAST_EDITED_BOOKMARK_LIMIT = 3
TRAP_HTTP_EXCEPTIONS = True
PROPAGATE_EXCEPTIONS = True
SELENIUM_BROWSER_URL = os.environ.get("SELENIUM_BROWSER_URL", "http://caddy:82")
LIVESERVER_PORT = 5001
QST_PLUGIN_PORT = LIVESERVER_PORT
PERMANENT_SESSION_LIFETIME = timedelta(weeks=9999)


class Schedule(TypedDict):
    task: str
    schedule: crontab


CELERYBEAT_SCHEDULE: dict[str, Schedule] = {
    # don't schedule anything while testing
}
WTF_CSRF_METHODS: list[str] = []
SCIM_USERNAME = "t"
SCIM_PASSWORD = "pass"
SISU_CERT_PATH: str | None = None
HOME_ORGANIZATION = "jyu.fi"

SAML_PATH = "/service/timApp/auth/saml/test"
SAML_VERIFY_METADATA = False

SESSION_COOKIE_SECURE = (
    False  # Test running does not have HTTPS, so secure cookie can't be used.
)

MINIMUM_SCHEDULED_FUNCTION_INTERVAL = 1

INTERNAL_PLUGIN_DOMAIN = "localhost"

MESSAGE_LISTS_ENABLED = True
MAILMAN_URL = "http://mailman-test:8001/3.1"
MAILMAN_USER = "restadmin"
MAILMAN_PASS = "restpass"
MAILMAN_EVENT_API_USER = "apiuser"
MAILMAN_EVENT_API_KEY = "apikey"

os.environ["AUTHLIB_INSECURE_TRANSPORT"] = "true"
OAUTH2_CLIENTS = [
    {
        "client_id": "tim_test",
        "client_secret": "test",
        "client_name": "TIM Test",
        "redirect_urls": ["http://tim/ping"],
        "allowed_scopes": ["profile"],
        "response_types": ["code", "token"],
        "grant_types": ["authorization_code"],
    }
]

SECURITY_INFO = {
    "extra_contacts": ["https://tim/contact"],
    "acknowledgements_url": "https://tim/acknowledgements",
    "preferred_languages": ["en", "fi"],
    "extra_canonical_hosts": ["https://tim"],
    "security_policy_url": "https://tim/security_policy",
}
