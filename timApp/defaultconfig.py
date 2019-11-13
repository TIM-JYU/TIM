import logging
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
SECRET_FILE_PATH = './tim_secret.py'
PERMANENT_SESSION_LIFETIME = timedelta(days=14)
SQLALCHEMY_TRACK_MODIFICATIONS = False
IMMEDIATE_PRELOAD = False
LIBSASS_STYLE = "compressed"
LIBSASS_INCLUDES = ["static/scripts/jspm_packages/github/twbs/bootstrap-sass@3.3.7/assets/stylesheets",
                    "static/scripts/jspm_packages/npm/eonasdan-bootstrap-datetimepicker@4.17.47/src/sass",
                    "static"]
TIM_NAME = os.environ.get('COMPOSE_PROJECT_NAME', 'tim')
TIM_HOST = os.environ.get('TIM_HOST', 'http://localhost')
DB_HOST = 'postgresql'
DATABASE = f"postgresql://postgres@{DB_HOST}:5432/{TIM_NAME}"
SASS_GEN_PATH = Path('generated')
TEMPLATES_AUTO_RELOAD = True
SQLALCHEMY_DATABASE_URI = DATABASE
SQLALCHEMY_POOL_SIZE = 2
SQLALCHEMY_POOL_TIMEOUT = 15
SQLALCHEMY_MAX_OVERFLOW = 100
LAST_EDITED_BOOKMARK_LIMIT = 15
LAST_READ_BOOKMARK_LIMIT = 15
PLUGIN_COUNT_LAZY_LIMIT = 20
UPLOADER_NGINX_URL = TIM_HOST + ":41419/"
UPLOADER_CONTAINER_URL = "http://uploader:41419/"

# When enabled, the readingtypes on_screen and hover_par will not be saved in the database.
DISABLE_AUTOMATIC_READINGS = False
HELP_EMAIL = 'tim@jyu.fi'
ERROR_EMAIL = 'timwuff.group@korppi.jyu.fi'
WUFF_EMAIL = 'wuff@tim.jyu.fi'
NOREPLY_EMAIL = 'no-reply@tim.jyu.fi'
GLOBAL_NOTIFICATION_FILE = '/tmp/global_notification.html'

QST_PLUGIN_PORT = 5000
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
        'schedule': crontab(minute='*/30'),  # Repeat every 30 mins.
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
HAKA_METADATA_CERT_URL = 'https://wiki.eduuni.fi/download/attachments/27297886/haka_testi_2018_sha2.crt?version=1&modificationDate=1525431417507&api=v2'
HAKA_METADATA_FINGERPRINT = 'a5956f3e3ad011be4a5f7ac549ac49d1a31fd65c'

# In production, copy these to prodconfig.py and remove the "_PROD" suffix.
SAML_PATH_PROD = '/service/timApp/auth/saml/prod'
HAKA_METADATA_URL_PROD = 'https://haka.funet.fi/metadata/haka-metadata.xml'
HAKA_METADATA_CERT_URL_PROD = 'https://wiki.eduuni.fi/download/attachments/27297775/haka-sign-v4.pem?version=1&modificationDate=1511941560853&api=v2'
HAKA_METADATA_FINGERPRINT_PROD = '6212391dfbd2874425a7fdd5ac8dd0bd5cd50d2f'

HOME_ORGANIZATION = 'jyu.fi'
