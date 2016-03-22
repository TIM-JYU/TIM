"""Routes for the clipboard"""

from flask import Blueprint
from .common import *


clipboard = Blueprint('clipboard',
                      __name__,
                      url_prefix='')  # TODO: Better URL prefix.


@clipboard.route('/clipboard/copy/<int:doc_id>/<from_par>/<to_par>', methods=['POST'])
def copy_to_clipboard(doc_id, from_par, to_par):
    return okJsonResponse()


@clipboard.route('/clipboard/paste/<int:doc_id>/<after_par>', methods=['POST'])
def paste_from_clipboard(doc_id, after_par):
    return okJsonResponse()

