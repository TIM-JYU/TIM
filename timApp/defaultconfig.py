import logging
import multiprocessing
import os
import subprocess
from datetime import timedelta
from pathlib import Path

from celery.schedules import crontab

# NOTE: If you are a different organization (other than JYU), please don't modify this file directly.
# This avoids merge conflicts. Override the values with prodconfig.py instead.

ALLOWED_DOCUMENT_UPLOAD_MIMETYPES = ['text/plain']
COMPRESS_DEBUG = True
COMPRESS_MIMETYPES = ['text/html', 'text/css', 'text/xml', 'application/json', 'application/javascript']
COMPRESS_MIN_SIZE = 50
DEBUG = False
FILES_PATH = '/tim_files'
LOG_DIR = "/service/tim_logs/"
LOG_FILE = "timLog.log"
LOG_LEVEL = logging.INFO
LOG_LEVEL_STDOUT = logging.INFO
LOG_PATH = os.path.join(LOG_DIR, LOG_FILE)
MAX_CONTENT_LENGTH = 50 * 1024 * 1024
PROFILE = False
SECRET_KEY = '85db8764yhfZz7-U.-y968buyn89b54y8y45tg'
PERMANENT_SESSION_LIFETIME = timedelta(days=14)
SQLALCHEMY_TRACK_MODIFICATIONS = False
IMMEDIATE_PRELOAD = False
LIBSASS_STYLE = "compressed"
LIBSASS_INCLUDES = ["node_modules/bootstrap-sass/assets/stylesheets",
                    "node_modules/eonasdan-bootstrap-datetimepicker/src/sass",
                    "static"]
TIM_NAME = os.environ.get('COMPOSE_PROJECT_NAME', 'tim')
TIM_HOST = os.environ.get('TIM_HOST', 'http://localhost')
DB_PASSWORD = 'postgresql'
DB_URI = f"postgresql://postgres:{DB_PASSWORD}@postgresql:5432/{TIM_NAME}"
SASS_GEN_PATH = Path('generated')
TEMPLATES_AUTO_RELOAD = True
SQLALCHEMY_DATABASE_URI = DB_URI
cpus = multiprocessing.cpu_count()

# If PG_MAX_CONNECTIONS is not defined (possible when running from IDE), we use a default value that gives
# pool size 2.
PG_MAX_CONNECTIONS = os.environ.get('PG_MAX_CONNECTIONS')
max_pool_all_workers = int(PG_MAX_CONNECTIONS or cpus * 3 + 5) - 5
SQLALCHEMY_POOL_SIZE = (max_pool_all_workers // cpus) - 1
SQLALCHEMY_POOL_TIMEOUT = 15
SQLALCHEMY_MAX_OVERFLOW = (max_pool_all_workers - SQLALCHEMY_POOL_SIZE * cpus) // cpus
LAST_EDITED_BOOKMARK_LIMIT = 15
LAST_READ_BOOKMARK_LIMIT = 15

PLUGIN_COUNT_LAZY_LIMIT = 20
QST_PLUGIN_PORT = 5000
PLUGIN_CONNECT_TIMEOUT = 0.5

# When enabled, the readingtypes on_screen and hover_par will not be saved in the database.
DISABLE_AUTOMATIC_READINGS = False
HELP_EMAIL = 'tim@jyu.fi'
ERROR_EMAIL = 'timwuff.group@korppi.jyu.fi'
WUFF_EMAIL = 'wuff@tim.jyu.fi'
NOREPLY_EMAIL = 'no-reply@tim.jyu.fi'
GLOBAL_NOTIFICATION_FILE = '/tmp/global_notification.html'


GIT_LATEST_COMMIT_TIMESTAMP = subprocess.run(["git", "log", "-1", "--date=format:%d.%m.%Y %H:%M:%S", "--format=%cd"],
                                             stdout=subprocess.PIPE).stdout.decode().strip()
GIT_BRANCH = subprocess.run(["git", "rev-parse", "--abbrev-ref", "HEAD"], stdout=subprocess.PIPE).stdout.decode().strip()
OPENID_IDENTITY_URL = 'https://korppi.jyu.fi/openid'

CELERY_BROKER_URL = 'redis://redis:6379',
CELERY_RESULT_BACKEND = 'redis://redis:6379'
CELERY_IMPORTS = ('timApp.tim_celery',)
CELERYBEAT_SCHEDULE = {
    'update-search-files': {
        'task': 'timApp.tim_celery.update_search_files',
        'schedule': crontab(hour='*/12', minute=0),
    },
    'process-notifications': {
        'task': 'timApp.tim_celery.process_notifications',
        'schedule': crontab(minute='*/5'),
    }
}
MAIL_HOST = "smtp.jyu.fi"
MAIL_SIGNATURE = "\n\n-- \nThis message was automatically sent by TIM"
WTF_CSRF_METHODS = ['POST', 'PUT', 'PATCH', 'DELETE']
WTF_CSRF_HEADERS = ['X-XSRF-TOKEN']
WTF_CSRF_TIME_LIMIT = None
MIN_PASSWORD_LENGTH = 10
PROXY_WHITELIST = [
    'korppi.jyu.fi',
    'plus.cs.aalto.fi',
]
SISU_ASSESSMENTS_URL = 'https://s2s.apitest.jyu.fi/assessments/'
SISU_CERT_PATH = '/service/certs/sisu.pem'

SAML_PATH = '/service/timApp/auth/saml/dev'
HAKA_METADATA_URL = 'https://haka.funet.fi/metadata/haka_test_metadata_signed.xml'
HAKA_METADATA_FINGERPRINT = 'a5956f3e3ad011be4a5f7ac549ac49d1a31fd65c'

# In production, copy these to prodconfig.py and remove the "_PROD" suffix.
SAML_PATH_PROD = '/service/timApp/auth/saml/prod'
HAKA_METADATA_URL_PROD = 'https://haka.funet.fi/metadata/haka-metadata.xml'
HAKA_METADATA_FINGERPRINT_PROD = '6212391dfbd2874425a7fdd5ac8dd0bd5cd50d2f'

HOME_ORGANIZATION = 'jyu.fi'

HAS_HTTPS = TIM_HOST.startswith('https:')
SESSION_COOKIE_SAMESITE = 'None' if HAS_HTTPS else None  # Required for Aalto iframe to work.
SESSION_COOKIE_SECURE = HAS_HTTPS  # Required by Chrome due to SameSite=None setting.

BOOKMARKS_ENABLED = True

# If False, only admins can create folders and documents.
ALLOW_CREATE_DOCUMENTS = True

EMAIL_REGISTRATION_ENABLED = True
HAKA_ENABLED = True

LOG_HOST = False

MAX_ANSWER_CONTENT_SIZE = 200 * 1024  # bytes
