"""Creates the Flask application for TIM.
Insert only configuration-related things in this file.
Do NOT define routes here.
"""
import mimetypes
import sys

from flask import Flask
from flask_sqlalchemy import SQLAlchemy

from routes.filters import map_format, timdate
from routes.logger import setup_logging, log_info, log_warning
from utils import datestr_to_relative, date_to_relative

sys.setrecursionlimit(10000)
app = Flask(__name__)

app.jinja_env.auto_reload = True # uncoment this to autorealod templates

app.jinja_env.trim_blocks = True
app.jinja_env.lstrip_blocks = True
app.config.from_pyfile('defaultconfig.py', silent=False)
app.config.from_envvar('TIM_SETTINGS', silent=True)
setup_logging(app)
log_info('Using database: {}'.format(app.config['DATABASE']))
default_secret = app.config['SECRET_KEY']
if not app.config.from_pyfile(app.config['SECRET_FILE_PATH'], silent=True):
    log_warning('secret file not found, using default values - do not run in production!')
else:
    assert default_secret != app.config['SECRET_KEY']

# Compress(app)
db = SQLAlchemy(app)

app.jinja_env.filters['map_format'] = map_format
app.jinja_env.filters['datestr_to_relative'] = datestr_to_relative
app.jinja_env.filters['date_to_relative'] = date_to_relative
app.jinja_env.filters['timdate'] = timdate

mimetypes.add_type('text/plain', '.scss')
