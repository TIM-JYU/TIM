from flask import Blueprint

static_bp = Blueprint('3rdparty',
                      __name__,
                      static_folder='/service/timApp/static/scripts/jspm_packages',
                      static_url_path='/static/scripts/jspm_packages')
