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

# Default sender address for email.
MAIL_FROM = 'tim@jyu.fi'

ERROR_EMAIL = 'timwuff.group@korppi.jyu.fi'
WUFF_EMAIL = 'wuff@tim.jyu.fi'
NOREPLY_EMAIL = 'no-reply@tim.jyu.fi'
GLOBAL_NOTIFICATION_FILE = '/tmp/global_notification.html'

GIT_LATEST_COMMIT_TIMESTAMP = subprocess.run(["git", "log", "-1", "--date=format:%d.%m.%Y %H:%M:%S", "--format=%cd"],
                                             stdout=subprocess.PIPE).stdout.decode().strip()
GIT_BRANCH = subprocess.run(["git", "rev-parse", "--abbrev-ref", "HEAD"],
                            stdout=subprocess.PIPE).stdout.decode().strip()

CELERY_BROKER_URL = 'redis://redis:6379'
CELERY_RESULT_BACKEND = 'redis://redis:6379'
CELERY_IMPORTS = ('timApp.tim_celery',)
CELERYBEAT_SCHEDULE = {
    'update-search-files': {
        'task': 'timApp.tim_celery.update_search_files',
        'schedule': crontab(hour='*/12', minute='0'),
    },
    'process-notifications': {
        'task': 'timApp.tim_celery.process_notifications',
        'schedule': crontab(minute='*/5'),
    }
}
# This makes the log format a little less verbose by omitting the Celery task id (which is an UUID).
CELERYD_TASK_LOG_FORMAT = "[%(asctime)s: %(levelname)s/%(processName)s] %(task_name)s: %(message)s"
BEAT_DBURI = DB_URI

MAIL_HOST = "smtp.jyu.fi"
MAIL_SIGNATURE = "\n\n-- \nThis message was automatically sent by TIM"
WTF_CSRF_METHODS = ['POST', 'PUT', 'PATCH', 'DELETE']
WTF_CSRF_HEADERS = ['X-XSRF-TOKEN']
WTF_CSRF_TIME_LIMIT = None
MIN_PASSWORD_LENGTH = 10
PROXY_WHITELIST = [
    'korppi.jyu.fi',
    'plus.cs.aalto.fi',
    'gitlab.com',
    'gitlab.jyu.fi',
    'tim.jyu.fi',
]

# Whitelist of /getproxy domains that don't require login.
PROXY_WHITELIST_NO_LOGIN = {}

SISU_ASSESSMENTS_URL = 'https://s2s.apitest.jyu.fi/assessments/'
SISU_CERT_PATH = '/service/certs/sisu.pem'

SAML_PATH = '/service/timApp/auth/saml/dev'
HAKA_METADATA_URL = 'https://haka.funet.fi/metadata/haka_test_metadata_signed.xml'
HAKA_METADATA_FINGERPRINT = '811dd04e5bde0976be6c7aa6a62e2e633d3de37807642e6c532019674545d019'

# In production, copy these to prodconfig.py and remove the "_PROD" suffix.
SAML_PATH_PROD = '/service/timApp/auth/saml/prod'
HAKA_METADATA_URL_PROD = 'https://haka.funet.fi/metadata/haka-metadata.xml'
HAKA_METADATA_FINGERPRINT_PROD = '70a9058262190cc23f8b0b14d6f0b7c0c74648e8b979bf4258eb7e23674a52f8'
# Fingerprint for the upcoming (1.12.2020) v5 certificate.
HAKA_METADATA_FINGERPRINT_NEW_PROD = 'a2c1eff331849cbfbfc920924861e03c8a56414ec003bf919e7f1b1a7dbc3169'

HOME_ORGANIZATION = 'jyu.fi'

LOAD_STUDENT_IDS_IN_TEACHER = False

HAS_HTTPS = TIM_HOST.startswith('https:')
SESSION_COOKIE_SAMESITE = 'None' if HAS_HTTPS else None  # Required for Aalto iframe to work.
SESSION_COOKIE_SECURE = HAS_HTTPS  # Required by Chrome due to SameSite=None setting.

BOOKMARKS_ENABLED = True

# If False, only admins can create folders and documents.
ALLOW_CREATE_DOCUMENTS = True

EMAIL_REGISTRATION_ENABLED = True
HAKA_ENABLED = True

# If False, resetting password is not allowed.
PASSWORD_RESET_ENABLED = True

# When enabled, the email login and signup processes are unified so that:
#
# * only email is asked first
# * then the password is requested and TIM asks to check email if the user has not logged in before.
SIMPLE_EMAIL_LOGIN = False

# Whether to use a Studyinfo message for help text after email is given.
# The point is to warn that TIM will only send the password if the account exists (and password is null)
# and the email corresponds to the one in Studyinfo.
# This only makes sense with EMAIL_REGISTRATION_ENABLED = False.
SIMPLE_LOGIN_USE_STUDY_INFO_MESSAGE = False

LOG_HOST = False

MAX_ANSWER_CONTENT_SIZE = 200 * 1024  # bytes

SCIM_ALLOWED_IP = '127.0.0.1'

# Whether to allow creation of messages lists via GUI. At this moment requires Mailman to be configured.
MESSAGE_LISTS_ENABLED = False
# Settings for mailmanclient-library. Set properly in production.
MAILMAN_URL = None
MAILMAN_USER = None
MAILMAN_PASS = None
# Settings for mailman-rest-events library. Set properly in production.
MAILMAN_EVENT_API_USER = None
MAILMAN_EVENT_API_KEY = None
# Link prefix to Postorius Web-UI. If used as is, directs to the mailing lists page.
MAILMAN_UI_LINK_PREFIX = "https://timlist.it.jyu.fi/postorius/lists/"
# Permitted file extensions allowed on message lists. If this grows large, maybe move to an external file and modify
# getting attachment file extensions from the file instead.
PERMITTED_ATTACHMENTS = ["doc", "docx", "htm", "html", "jpeg", "jpg", "pdf", "png", "ppt", "pptx", "tex", "txt", "xls",
                         "xlsx"]
# These names are reserved from the pool of names for message lists. If need arises, split into TIM and message
# channel specific reserved names.
RESERVED_NAMES = ["postmaster", "listmaster", "admin"]

# If true, prints all SQL statements with tracebacks.
DEBUG_SQL = False

MINIMUM_SCHEDULED_FUNCTION_INTERVAL = 3600

INTERNAL_PLUGIN_DOMAIN = 'tim'

# BACKUP_ANSWER_* variables are related to backing up answers by sending them to another host on the fly.

# When sending an answer to another host, use this secret for authentication.
BACKUP_ANSWER_SEND_SECRET = None

# When receiving an answer from another host, make sure that the given secret matches this one.
BACKUP_ANSWER_RECEIVE_SECRET = None

# In the receiving host, the filename where the answers will be stored, one JSON string per line.
BACKUP_ANSWER_FILE = 'answers.backup'

# The hosts where to back up the answers. Every entry should start with "https://".
BACKUP_ANSWER_HOSTS = None

# DIST_RIGHTS_* variables are related to distributing rights.

# A mapping of target identifiers to lists of hosts.
# Example:
# {
#     'some_exam': {
#         'hosts': ['https://machine1.example.com', 'https://machine2.example.com'],
#         'item': 'path/to/some/exam/here',
#     },
# }
DIST_RIGHTS_HOSTS = {

}

# When registering a right that is going to be distributed, make sure that the given secret matches this one.
DIST_RIGHTS_REGISTER_SECRET = None

# When sending a right to another host, send this secret.
DIST_RIGHTS_SEND_SECRET = None

# When receiving a right from the distributor host, make sure that the given secret matches this one.
DIST_RIGHTS_RECEIVE_SECRET = None

# A list of documents on this TIM instance that can register and distribute rights directly
DIST_RIGHTS_MODERATION_DOCS = [

]

# Map of items that should trigger rights distribution when unlocking the item.
DIST_RIGHTS_UNLOCK_TARGETS = {
    # 'path/to/item': ['some_target'],
}

# List of hosts to send /register calls.
DIST_RIGHTS_REGISTER_HOSTS = []

# When calling /register, send this secret.
DIST_RIGHTS_REGISTER_SEND_SECRET = None

# The group that is allowed to call /changeStartTime.
DIST_RIGHTS_START_TIME_GROUP = None

# Whether this host is the rights distributor.
DIST_RIGHTS_IS_DISTRIBUTOR = False

# The set of allowed IP networks. The following actions are restricted:
# * Login and email registration are denied for non-admins.
# * Answer route is blocked.
IP_BLOCK_ALLOWLIST = None

# The informational message to display in TIM header if the IP is outside the allowlist.
IP_BLOCK_MESSAGE = None

# If true, IPs that are:
# * outside allowed networks and
# * not in blocklist
# are not blocked but only logged.
IP_BLOCK_LOG_ONLY = False

# The set of documents for which the right is inherited from its containing folder.
INHERIT_FOLDER_RIGHTS_DOCS = {}
