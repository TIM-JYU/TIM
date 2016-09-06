import logging
import os
from datetime import timedelta

COMPRESS_DEBUG     = True
COMPRESS_MIMETYPES = ['text/html', 'text/css', 'text/xml', 'application/json', 'application/javascript']
COMPRESS_MIN_SIZE  = 50
DEBUG              = False
FILES_PATH         = 'tim_files'
LOG_DIR            = "../tim_logs/"
LOG_FILE           = "timLog.log"
LOG_LEVEL          = logging.DEBUG
LOG_LEVEL_STDOUT   = logging.INFO
LOG_PATH           = os.path.join(LOG_DIR, LOG_FILE)
MAX_CONTENT_LENGTH = 50 * 1024 * 1024
PROFILE            = False
SECRET_KEY         = '85db8764yhfZz7-U.-y968buyn89b54y8y45tg'
SECRET_FILE_PATH   = './tim_secret.py'
PERMANENT_SESSION_LIFETIME = timedelta(days=14)
SQLALCHEMY_TRACK_MODIFICATIONS = False
IMMEDIATE_PRELOAD  = False
LIBSASS_STYLE      = "compressed"
LIBSASS_INCLUDES   = ["static/scripts/bower_components/bootstrap-sass/assets/stylesheets",
                      "static/scripts/bower_components/jquery-ui/themes/base",
                      "static"]
TIM_NAME = os.environ.get('TIM_NAME', 'timlocal')
TIM_HOST = os.environ.get('TIM_HOST', 'http://localhost')
OLD_SQLITE_DATABASE       = 'tim_files/tim.db'
DATABASE           = "postgresql://postgres@postgresql-{0}:5432/{0}".format(TIM_NAME)
SQLALCHEMY_BINDS = {
    'tim_main': DATABASE,
    'tempdb': "postgresql://postgres:postgres@postgresql-{0}:5432/tempdb_{0}".format(TIM_NAME)
}
SASS_GEN_PATH = 'gen'
TEMPLATES_AUTO_RELOAD = True
SQLALCHEMY_DATABASE_URI = DATABASE
SQLALCHEMY_POOL_SIZE = 1
