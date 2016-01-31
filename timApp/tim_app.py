"""Creates the Flask application for TIM.
Insert only configuration-related things in this file.
Do NOT define routes here.
"""

import os
import sys

from flask import Flask

from routes.filters import map_format

sys.setrecursionlimit(10000)
app = Flask(__name__)
app.jinja_env.trim_blocks = True
app.jinja_env.lstrip_blocks = True
app.config.from_pyfile('defaultconfig.py', silent=False)
app.config.from_envvar('TIM_SETTINGS', silent=True)
default_secret = app.config['SECRET_KEY']
if not app.config.from_pyfile(app.config['SECRET_FILE_PATH'], silent=True):
    print('WARNING: secret file not found, using default values - do not run in production!')
else:
    assert default_secret != app.config['SECRET_KEY']

# Compress(app)

timname = os.environ.get('TIM_NAME', 'tim')
app.config['TIM_NAME'] = timname
app.config['SQLALCHEMY_DATABASE_URI'] = "postgresql://docker:docker@postgre:5432/tempdb_" + timname

app.jinja_env.filters['map_format'] = map_format
