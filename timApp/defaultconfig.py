"""
List and defaults for all configuration options in TIM.

.. note:: Please don't modify this file directly in your server or local development setup.
          This avoids merge conflicts. Override the values with prodconfig.py or devconfig.py instead.
"""

import logging
import multiprocessing
import os
from datetime import timedelta
from pathlib import Path

from celery.schedules import crontab

from timApp.document.translation.deepl import (
    DeeplTranslationService,
    DeeplProTranslationService,
)
from timApp.user.special_group_names import TEACHERS_GROUPNAME
from timApp.util.git_utils import get_latest_commit_timestamp, get_current_branch

# NOTE: If you are a different organization (other than JYU), please don't modify this file directly.
# This avoids merge conflicts. Override the values with prodconfig.py instead.

# Path to TIM document containing the privacy notice. The link to the document is shown in page footer.
# If None, link to the document is not shown
PRIVACY_NOTICE_DOC = "tim/tietosuojailmoitus"

# Path to TIM document containing the accessibility notice. The link to the document is shown in page footer.
# If None, link to the document is not shown
ACCESSIBILITY_STATEMENT_DOC = "tim/saavutettavuusseloste"

# Path to TIM document containing the Terms of service The link to the document is shown in page footer.
# If None, link to the document is not shown
TERMS_OF_SERVICE_DOC = None

ALLOWED_DOCUMENT_UPLOAD_MIMETYPES = ["text/plain"]
COMPRESS_DEBUG = True
COMPRESS_MIMETYPES = [
    "text/html",
    "text/css",
    "text/xml",
    "application/json",
    "application/javascript",
]
COMPRESS_MIN_SIZE = 50
DEBUG = False
FILES_PATH = "/tim_files"
LOG_DIR = "/service/tim_logs/"
LOG_FILE = "timLog.log"
LOG_LEVEL = logging.INFO
LOG_LEVEL_STDOUT = logging.INFO
LOG_PATH = os.path.join(LOG_DIR, LOG_FILE)
# If True, requests are also logged before they are processed.
# This is useful sometimes to profile calls that never complete.
LOG_BEFORE_REQUESTS = False
MAX_CONTENT_LENGTH = 50 * 1024 * 1024
PROFILE = False
SECRET_KEY = "85db8764yhfZz7-U.-y968buyn89b54y8y45tg"
PERMANENT_SESSION_LIFETIME = timedelta(days=14)
SQLALCHEMY_TRACK_MODIFICATIONS = False
IMMEDIATE_PRELOAD = False
LIBSASS_STYLE = "compressed"
LIBSASS_INCLUDES = [
    "node_modules/bootstrap-sass/assets/stylesheets",
    "node_modules/eonasdan-bootstrap-datetimepicker/src/sass",
    "static",
]
TIM_NAME = os.environ.get("COMPOSE_PROJECT_NAME", "tim")
TIM_HOST = os.environ.get("TIM_HOST", "http://localhost")
DB_PASSWORD = "postgresql"
DB_URI = f"postgresql://postgres:{DB_PASSWORD}@postgresql:5432/{TIM_NAME}"
SASS_GEN_PATH = Path("generated")
TEMPLATES_AUTO_RELOAD = True
SQLALCHEMY_DATABASE_URI = DB_URI

LAST_EDITED_BOOKMARK_LIMIT = 15
LAST_READ_BOOKMARK_LIMIT = 15

PLUGIN_COUNT_LAZY_LIMIT = 20
QST_PLUGIN_PORT = 5000
PLUGIN_CONNECT_TIMEOUT = 0.5

# When enabled, the readingtypes on_screen and hover_par will not be saved in the database.
DISABLE_AUTOMATIC_READINGS = False
HELP_EMAIL = "tim@jyu.fi"

# Default sender address for email.
MAIL_FROM = "tim@jyu.fi"
MAIL_PASSWORD = None

CONTENT_REPORT_EMAIL = "tim@jyu.fi"

ERROR_EMAIL = "wuff-reports@tim.jyu.fi"
WUFF_EMAIL = "wuff@tim.jyu.fi"
NOREPLY_EMAIL = "no-reply@tim.jyu.fi"
GLOBAL_NOTIFICATION_FILE = "/tmp/global_notification.html"

WUFF_MAX_SAME_COUNT = None
"""How many times the same error can be reported before it is muted. Set to None to disable muting."""
WUFF_MAX_SAME_INTERVAL = 60 * 5  # 5 minutes
"""How long the same error can be reported before it is muted. Duration in seconds."""
WUFF_MAX_SAME_MUTE_DURATION = 60 * 60  # 1 hour
"""How long the same error is muted after it has been reported too many times. Duration in seconds."""

GIT_LATEST_COMMIT_TIMESTAMP = get_latest_commit_timestamp()
GIT_BRANCH = get_current_branch()

CELERY_BROKER_URL = "redis://redis:6379"
CELERY_RESULT_BACKEND = "redis://redis:6379"
CELERY_IMPORTS = ("timApp.tim_celery",)
CELERYBEAT_SCHEDULE = {
    "update-search-files": {
        "task": "timApp.tim_celery.update_search_files",
        "schedule": crontab(hour="*/12", minute="0"),
    },
    "process-notifications": {
        "task": "timApp.tim_celery.process_notifications",
        "schedule": crontab(minute="*/5"),
    },
    "cleanup-expired-oauth2-tokens": {
        "task": "timApp.tim_celery.cleanup_oauth2_tokens",
        "schedule": crontab(hour="*/24", minute="0"),
    },
    "cleanup-verifications": {
        "task": "timApp.tim_celery.cleanup_verifications",
        "schedule": crontab(minute="*/10"),
    },
}
# This makes the log format a little less verbose by omitting the Celery task id (which is an UUID).
CELERYD_TASK_LOG_FORMAT = (
    "[%(asctime)s: %(levelname)s/%(processName)s] %(task_name)s: %(message)s"
)
BEAT_DBURI = DB_URI

USERSELECT_QUEUED_ACTIONS_CELERY = True
"""
If enabled, UserSelect will automatically schedule running the queued actions with celery.
When disabled, the actions must be run manually with /userSelect/applyPendingActions
"""

MAIL_HOST = "smtpauth2.jyu.fi"
MAIL_PORT = 0
MAIL_SIGNATURE = "\n\n-- \nThis message was automatically sent by TIM"
WTF_CSRF_METHODS = ["POST", "PUT", "PATCH", "DELETE"]
WTF_CSRF_HEADERS = ["X-XSRF-TOKEN"]
WTF_CSRF_TIME_LIMIT = None
MIN_PASSWORD_LENGTH = 10
PROXY_WHITELIST = [
    "korppi.jyu.fi",
    "plus.cs.aalto.fi",
    "gitlab.com",
    "github.com",
    "gitlab.jyu.fi",
    "tim.jyu.fi",
    "www.foreca.com",
]

# Whitelist of /getproxy domains that don't require login.
PROXY_WHITELIST_NO_LOGIN = {}

SISU_ASSESSMENTS_DISABLED_MESSAGE = "Assessments are disabled at the moment"
SISU_ASSESSMENTS_URL = "https://s2s.apitest.jyu.fi/assessments/"
SISU_CERT_PATH = "/service/certs/sisu.pem"

SAML_PATH = "/service/timApp/auth/saml/dev"
SAML_VERIFY_METADATA = False
HAKA_METADATA_URL = "https://haka.funet.fi/metadata/haka_test_metadata_signed.xml"

# In production, copy these to prodconfig.py and remove the "_PROD" suffix.
SAML_PATH_PROD = "/service/timApp/auth/saml/prod"
HAKA_METADATA_URL_PROD = "https://haka.funet.fi/metadata/haka-metadata.xml"

HOME_ORGANIZATION = "jyu.fi"

LOAD_STUDENT_IDS_IN_TEACHER = False

HAS_HTTPS = TIM_HOST.startswith("https:")

# Following current web dev recommendations, first-party cookies must be marked with either
# SameSite=Lax or SameSite=Strict. See
# https://web.dev/articles/first-party-cookie-recipes#the_good_first-party_cookie_recipe
# TODO: For cases where sessions are used across different domains, we need to provide
#     a list of allowed domains for which to allow the session cookie as a third-party cookie.
#     Or see e.g., https://developers.google.com/privacy-sandbox/cookies/chips
SESSION_COOKIE_SAMESITE = "Lax" if HAS_HTTPS else None
SESSION_COOKIE_SECURE = HAS_HTTPS  # Require HTTPS or localhost for session cookies

DEFAULT_UI_LANGUAGE = None
"""
The default UI language to use when user's language is not explicitly set. 
If not set, determines the language based on the user's browser settings.
"""

BOOKMARKS_ENABLED = True

# If False, only admins can create folders and documents.
ALLOW_CREATE_DOCUMENTS = True

# When enabled, display sign up/account registration option in login dialog.
EMAIL_REGISTRATION_ENABLED = True

# When enabled, display Haka identity provider/federation service as a login option.
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

# Custom login message to show globally when simple login is enabled
SIMPLE_LOGIN_CUSTOM_LOGIN_MESSAGE = None

LOGIN_CODES_ENABLED = False
"""
If true, enables logging in via special temporary login codes.
"""

LOG_HOST = False

MAX_ANSWER_CONTENT_SIZE = 200 * 1024  # bytes

SCIM_ALLOWED_IP = {"127.0.0.1"}
SCIM_ALLOW_UPDATE_HAKA_USER_INFO = False

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
# Link to the Mailman mail templates. Used by mailman.
MAILMAN_TEMPLATES_URL = (
    "http://localhost/postorius/api/templates/list/{list_id}/{template_name}"
)
# Permitted file extensions allowed on message lists. If this grows large, maybe move to an external file and modify
# getting attachment file extensions from the file instead.
PERMITTED_ATTACHMENTS = [
    "doc",
    "docx",
    "htm",
    "html",
    "jpeg",
    "jpg",
    "pdf",
    "png",
    "ppt",
    "pptx",
    "tex",
    "txt",
    "xls",
    "xlsx",
]
# These names are reserved from the pool of names for message lists. If need arises, split into TIM and message
# channel specific reserved names.
RESERVED_NAMES = ["postmaster", "listmaster", "admin"]

# If true, prints all SQL statements with tracebacks.
DEBUG_SQL = False

MINIMUM_SCHEDULED_FUNCTION_INTERVAL = 3600

INTERNAL_PLUGIN_DOMAIN = "tim"

# BACKUP_ANSWER_* variables are related to backing up answers by sending them to another host on the fly.

# When sending an answer to another host, use this secret for authentication.
BACKUP_ANSWER_SEND_SECRET = None

# When receiving an answer from another host, make sure that the given secret matches this one.
BACKUP_ANSWER_RECEIVE_SECRET = None

# In the receiving host, the filename where the answers will be stored, one JSON string per line.
BACKUP_ANSWER_FILE = "answers.backup"

# The hosts where to back up the answers. Every entry should start with "https://".
BACKUP_ANSWER_HOSTS = None

ANSWER_LOG_FILE = None

SYNC_USER_GROUPS_SEND_SECRET = None
"""
Secret to use to when syncing user group info. If None, no user group memberships.

..note: Right now, syncing only is done in UserSelect.
"""

SYNC_USER_GROUPS_HOSTS = []
"""Groups to sync user group info to."""

SYNC_USER_GROUPS_RECEIVE_SECRET = None
"""Secret to check against when syncing group info."""

# DIST_RIGHTS_* variables are related to distributing rights.

# A mapping of target identifiers to lists of hosts.
# Example:
# {
#     'some_exam': {
#         'hosts': ['https://machine1.example.com', 'https://machine2.example.com'],
#         'item': 'path/to/some/exam/here',
#     },
# }
DIST_RIGHTS_HOSTS = {}

# A description of the right distribution network.
# The setting defines all machines that are part of the network, how they are grouped and the specific rules
# for distributing rights
#
# Example:
# {
#     "hosts": {
#       "host1": "https://machine1.example.com",
#       "host2": "https://machine2.example.com",
#       "host3": "https://machine3.example.com",
#     },
#     "distribution_groups": {
#         "group1": ["host1", "host2"],
#         "group2": ["host3"],
#     },
#     "distribute_targets": {
#         "some_exam": [
#             {
#                 "target": "host1",
#                 "distribute_group": "group1",
#             },
#             {
#                 "target": "host3",
#                 "distribute_group": "group2",
#             },
#         ],
#     },
# }
DIST_RIGHTS_NETWORK = {
    "hosts": {},
    "distribute_groups": {},
    "distribute_targets": {},
}

# Current host identifier in the distribution network.
DIST_RIGHTS_HOST_ID = None

# When registering a right that is going to be distributed, make sure that the given secret matches this one.
DIST_RIGHTS_REGISTER_SECRET = None

# When sending a right to another host, send this secret.
DIST_RIGHTS_SEND_SECRET = None

# When receiving a right from the distributor host, make sure that the given secret matches this one.
DIST_RIGHTS_RECEIVE_SECRET = None

# A list of documents on this TIM instance that can register and distribute rights directly
DIST_RIGHTS_MODERATION_DOCS = []

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

# Number of threads to use when distributing rights via HTTP
DIST_RIGHTS_WORKER_THREADS = multiprocessing.cpu_count()

# The set of allowed IP networks. The following actions are restricted:
# * Login and email registration are denied for non-admins.
# * Answer route is blocked.
IP_BLOCK_ALLOWLIST = None

# The informational message to display in TIM header if the IP is outside the allowlist.
IP_BLOCK_MESSAGE = None

# The message sent as reply whenever a blocked route is called by an IP outside the allowlist.
IP_BLOCK_ROUTE_MESSAGE = None

# If true, IPs that are:
# * outside allowed networks and
# * not in blocklist
# are not blocked but only logged.
IP_BLOCK_LOG_ONLY = False

# The set of documents for which the right is inherited from its containing folder.
INHERIT_FOLDER_RIGHTS_DOCS = {}

LOG_USER_SELECT_ACTIONS = False
"""Log any actions applied via UserSelect component."""

# A list of OAuth2 applications that can authenticate with TIM
# Refer to OAuth2Client class in timApp/auth/oauth2/models.py for documentation of each field
# Example:
# [
#     {
#        'client_id': 'example',
#        'client_secret': 'secret',
#        'client_name': 'Example application',
#        'redirect_urls': ['https://example.com/login/callback'],
#        'allowed_scopes': ['profile'],
#        'response_types': ['code', 'token'],
#        'grant_types': ['authorization_code'],
#     }
# ]
OAUTH2_CLIENTS = []

# Name of user that is used for displaying model/example answers.
MODEL_ANSWER_USER_NAME = "mallivastaus"

# User groups who are allowed to log in as model answer user with quickLogin route.
QUICKLOGIN_ALLOWED_MODEL_ANSWER_GROUPS = {
    "ohj1",
    TEACHERS_GROUPNAME,
}

# How long unreacted verifications should be persisted for in seconds
# Default: 1 hour
VERIFICATION_UNREACTED_CLEANUP_INTERVAL = 10 * 60
# How long reacted verifications should be persisted for in seconds
# Default: 30 days
VERIFICATION_REACTED_CLEANUP_INTERVAL = 30 * 24 * 60 * 60

# Supported languages for document translation.
# Database entries for languages are created when TIM is started/re-started.
# Pre-existing entries are skipped.
# Default list includes languages supported by the translation service
# DeepL: https://www.deepl.com/docs-api/translating-text/
# Custom language syntax is the following:
# {
#   "lang_code": "<standardized tag>",
#   "lang_name": "<name in English>",
#   "autonym": "<name in its language>",
# }
# The standardized tag lang_code should adhere to IETF BCP47,
# detailed in RFC5646 (https://www.rfc-editor.org/info/rfc5646).
LANGUAGES = [
    "Bulgarian",
    "Czech",
    "Danish",
    "German",
    "Greek",
    "American English",
    "British English",
    "Spanish",
    "English",
    "Estonian",
    "Finnish",
    "French",
    "Hungarian",
    "Indonesian",
    "Italian",
    "Japanese",
    "Korean",
    "Lithuanian",
    "Latvian",
    "Norwegian Bokmål",
    "Dutch",
    "Polish",
    "Portuguese",
    "Brazilian Portuguese",
    "Romanian",
    "Russian",
    "Slovak",
    "Slovenian",
    "Swedish",
    "Turkish",
    "Ukrainian",
    "Chinese",
    # TODO Change lang_code to the accurate tag-format of Simple Finnish.
    {"lang_code": "fi-simple", "lang_name": "Simple Finnish", "autonym": "selkosuomi"},
    # TODO Remove this from list in production and during automatic tests.
    # REVERSE_LANG,
]

# Translation services with their initialization values are listed below.
# Syntax for inserting a new translation service is the following:
# (
#   <Class of the TranslationService>,
#   <Some type (Any), that is used in initializing the TranslationService>
# )

MACHINE_TRANSLATORS = [
    (
        DeeplTranslationService,
        {
            "service_url": "https://api-free.deepl.com/v2",
            "ignore_tag": "_ignore",
        },
    ),
    (
        DeeplProTranslationService,
        {
            "service_url": "https://api.deepl.com/v2",
            "ignore_tag": "_ignore",
        },
    ),
    # TODO Remove this from list in production and during automatic tests.
    # (
    #    ReversingTranslationService,
    #    None,
    # ),
]

# FuturesSession options for machine translators,
# used to control the number of parallel translation requests,
# and how many times a failed request will be re-sent
TRANSLATION_SESSION_OPTIONS = {
    "DeepL Free": {
        "max_workers": 4,
        "max_retries": 4,
    },
    "DeepL Pro": {
        "max_workers": 4,
        "max_retries": 4,
    },
}


# Options related to session management

SESSIONS_ENABLE = False
"""If enabled, session management information will be stored and logged."""

SESSIONS_MAX_CONCURRENT_SESSIONS_PER_USER = 1
"""
How many concurrent sessions are allowed for a single user for a protected document.
If this limit is reached, the user is not given an active session which prevents accessing documents.

If None, there is no limit.
"""

SESSION_BLOCK_IGNORE_DOCUMENTS = {}
"""
Paths of documents that should be ignored when blocking sessions.
All users will be able to access the document regardless of the session blocking.
"""

SECURITY_INFO = {
    "extra_contacts": [],
    "acknowledgements_url": None,
    "preferred_languages": [],
    "extra_canonical_hosts": [],
    "security_policy_url": None,
}
"""
Information about security and privacy of the instance.
See https://securitytxt.org/ for more information.

Note: The encryption and decryption keys will be stored in a keyring located at <tim root>/certs/tim/gpg

extra_contacts: List of email addresses or URLs that should be contacted in case of security issues.
encryption_key: URL to the PGP encryption key used for encrypting any security-related messages.
acknowledgements_url: URL to the acknowledgements page.
preferred_languages: List of languages in order of preference for contacting.
extra_canonical_hosts: List of extra canonical hosts for the instance. Used to specify canonical URLs for security.txt
security_policy_url: URL to the security policy page.
"""

GLOBAL_DOCUMENT_CACHING = False
"""
Global default value for document caching. The value allows globally enabling or disabling document cache.
"""

RESTRICT_ROBOTS = False
RESTRICT_ROBOTS_METHODS = {
    "restrict_global": True,
    "global_unavailable_date": "2023-10-20",
    "global": ["noindex", "nofollow", "noarchive"],
    "bots": {
        "googlebot": [],  # Google
        "bingbot": [],  # Microsoft
        "facebot": [],  # Facebook/Meta
        "duckduckbot": [],  # Duck-Duck Go
        "slurp": [],  # Yahoo
        "baiduspider": [],  # Baidu
        "yandexbot": [],  # Yandex
        "otherbot": [],
    },
}


# LaTeX printing settings

PRINT_MAX_LATEX_MEMORY = 5000000
"""
Maximum amount of memory that can be used by a single LaTeX process to typeset PDFs.
"""

# Group addition settings

GROUPS_MISSING_USER_CREATE_ALLOW = False
"""
If True, yet non-existing users can be added to groups.
In that case, a new user account is created with the email address of the user.
"""

GROUPS_EXISTING_USER_ADD_NOTIFY_EXISTING_HEAD = (
    "TIM: You’ve Been Added to '{{group_name}}'"
)
"""
When notifying existing users about being added to the group,
this will be the subject of the email.
"""
GROUPS_EXISTING_USER_ADD_NOTIFY_EXISTING_BODY = """
Hi!

You’ve been added to the TIM group '{{group_name}}'.
As a member, you now have access to all group resources available at <{{host}}>.

If you have any questions, please contact {{support_contact}}.
"""
"""
When notifying existing users about being added to the group,
this will be the body of the email.
"""

GROUPS_MISSING_USER_ADD_NOTIFY_HEAD = (
    "TIM: You have been added to group '{{group_name}}' – Complete Your Sign-Up"
)
"""
When notifying new users about being added to the group,
this will be the subject of the email.
"""
GROUPS_MISSING_USER_ADD_NOTIFY_BODY = """
Hi!

You’ve been added to the TIM group '{{group_name}}' using this email address. 
An account has been created for you, but a password was not set during the process.

To complete your sign-up and access your account:
1. Visit <{{host}}> and click Log in on the top of the page.
2. Use email login with this email and use the "Forgot password" option to set your password.

Once your password is set, you’ll have full access to the group’s resources.

If you do not recognize this email, please ignore it.
If you have any questions or need assistance, feel free to contact {{support_contact}}.
"""
"""
When notifying new users about being added to the group,
this will be the body of the email.
"""

ADMIN_ACCESS_WARNING = None
"""
A warning message shown to admins when they use the platform.
This can be used to remind admins to use the platform responsibly.
"""
