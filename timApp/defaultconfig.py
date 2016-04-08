import os
from datetime import timedelta

COMPRESS_DEBUG     = True
COMPRESS_MIMETYPES = ['text/html', 'text/css', 'text/xml', 'application/json', 'application/javascript']
COMPRESS_MIN_SIZE  = 50
CONTRACTS_ENABLED  = False
DATABASE           = './tim_files/tim.db'
DEBUG              = False
FILES_PATH         = 'tim_files'
LOG_DIR            = "../tim_logs/"
LOG_FILE           = "timLog.log"
LOG_PATH           = os.path.join(LOG_DIR, LOG_FILE)
MAX_CONTENT_LENGTH = 50 * 1024 * 1024
PASSWORD           = '4t95MHJj9h89y'
PROFILE            = False
SECRET_KEY         = '85db8764yhfZz7-U.-y968buyn89b54y8y45tg'
UPLOAD_FOLDER      = "./media/images/"
USERNAME           = 'admin'
SECRET_FILE_PATH   = './tim_secret.py'
PERMANENT_SESSION_LIFETIME = timedelta(days=14)
SQLALCHEMY_TRACK_MODIFICATIONS = False
IMMEDIATE_PRELOAD  = False
SASS_STYLE         = "compressed"
SASS_LOAD_PATHS    = [".", "../scripts/bower_components/bootstrap-sass/assets/stylesheets"]
