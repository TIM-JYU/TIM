import re

import magic
from bs4 import UnicodeDammit
from flask import current_app
from werkzeug.exceptions import abort

from timApp.common import has_special_chars
from timApp.sessioninfo import logged_in, get_current_user_object
from timApp.timdb.models.docentry import DocEntry
from timApp.timdb.models.folder import Folder
from timApp.timdb.timdbexception import TimDbException
from timApp.utils import split_location


def validate_item(item_path: str, item_type: str):
    if not logged_in():
        abort(403, f'You have to be logged in to perform this action.')

    if item_path is None:
        abort(400, 'item_path was None')

    if not all(part for part in item_path.split('/')):
        abort(400, f'The {item_type} path cannot have empty parts.')

    if re.match('^(\d)*$', item_path) is not None:
        abort(400, f'The {item_type} path can not be a number to avoid confusion with document id.')

    if has_special_chars(item_path):
        abort(400,
              f'The {item_type} path has invalid characters. Only letters, numbers, underscores and dashes are allowed.')

    if DocEntry.find_by_path(item_path, try_translation=True) is not None or Folder.find_by_path(item_path) is not None:
        abort(403, 'Item with a same name already exists.')

    f = Folder.find_first_existing(item_path)
    if not f:
        abort(403)
    if not get_current_user_object().can_write_to_folder(f):
        abort(403, f'You cannot create {item_type}s in this folder.')


def validate_item_and_create(item_path: str, item_type: str, owner_group_id: int):
    validate_item(item_path, item_type)
    item_path, _ = split_location(item_path)
    try:
        Folder.create(item_path, owner_group_id, apply_default_rights=True)
    except TimDbException as e:
        abort(403, str(e))


def validate_uploaded_document_content(file_content):
    raw = file_content.read()
    mime = magic.Magic(mime=True)
    mimetype = mime.from_buffer(raw)
    if mimetype not in current_app.config['ALLOWED_DOCUMENT_UPLOAD_MIMETYPES']:
        abort(400, f'Only markdown files are allowed. This file appears to be {mimetype}.')

    # UnicodeDammit gives incorrect results if the encoding is UTF-8 without BOM,
    # so try the built-in function first.
    try:
        content = raw.decode('utf-8')
    except UnicodeDecodeError:
        content = UnicodeDammit(raw).unicode_markup
    return content
