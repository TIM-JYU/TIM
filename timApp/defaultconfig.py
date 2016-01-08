import os
from datetime import timedelta
from subprocess import check_output

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
MAX_CONTENT_LENGTH = 16 * 1024 * 1025
PASSWORD           = '4t95MHJj9h89y'
PROFILE            = False
SECRET_KEY         = '85db8764yhfZz7-U.-y968buyn89b54y8y45tg'
UPLOAD_FOLDER      = "./media/images/"
USERNAME           = 'admin'
SECRET_FILE_PATH   = './tim_secret.py'
PERMANENT_SESSION_LIFETIME = timedelta(days=14)
SQLALCHEMY_TRACK_MODIFICATIONS = False
DOCKER_BRIDGE_IP   = check_output("ip ro get 8.8.8.8 | grep -oP '(?<=via )([\d\.]+)'",
                                  shell=True).decode('utf-8')[:-1]
DOCKER_BRIDGE      = 'http://' + DOCKER_BRIDGE_IP
IMMEDIATE_PRELOAD  = False
