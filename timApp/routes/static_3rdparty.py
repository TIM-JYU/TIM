from flask import Blueprint

static_blueprint = Blueprint('3rdparty',
                             __name__,
                             static_folder='../static/scripts/jspm_packages/npm',
                             static_url_path='/static/scripts/jspm_packages/npm')
