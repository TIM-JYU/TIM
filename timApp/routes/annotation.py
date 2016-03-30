from flask import Blueprint
from .common import *

annotations = Blueprint('annotations',
                    __name__,
                    url_prefix='')

# Does not work yet!
@annotations.route("/addannotation", methods=['GET'])
def add_annotation(version_id: int, place_start: int, place_end: int):
    return