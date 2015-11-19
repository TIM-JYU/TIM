"""Creates the Flask application for TIM.
Insert only configuration-related things in this file.
Do NOT define routes here.
"""

import os
from flask import Flask


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

timname = None
if "TIM_NAME" in os.environ:
    timname = os.environ.get("TIM_NAME")
else:
    print("Missing TIM_NAME environment variable. Exiting..")
    exit()

app.config.from_envvar('TIM_NAME', silent=True)
app.config['SQLALCHEMY_DATABASE_URI'] = "postgresql://docker:docker@postgre:5432/tempdb_" + timname
