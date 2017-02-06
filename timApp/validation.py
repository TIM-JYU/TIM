import re

import magic
from bs4 import UnicodeDammit
from flask import current_app
from werkzeug.exceptions import abort

from accesshelper import can_write_to_folder
from common import has_special_chars
from dbaccess import get_timdb
from sessioninfo import logged_in
from timdb.models.docentry import DocEntry
from timdb.models.folder import Folder


def validate_item(item_path, item_type):
    if not logged_in():
        abort(403, 'You have to be logged in to perform this action.'.format(item_type))

    if item_path is None:
        abort(400, 'item_name was None')

    if not all(part for part in item_path.split('/')):
        abort(400, 'The {} path cannot have empty parts.'.format(item_type))

    if re.match('^(\d)*$', item_path) is not None:
        abort(400, 'The {} path can not be a number to avoid confusion with document id.'.format(item_type))

    if has_special_chars(item_path):
        abort(400, 'The {} path has invalid characters. Only letters, numbers, underscores and dashes are allowed.'.format(item_type))

    timdb = get_timdb()
    if DocEntry.find_by_path(item_path, try_translation=True) is not None or timdb.folders.get_folder_id(item_path) is not None:
        abort(403, 'Item with a same name already exists.')

    if not can_write_to_folder(item_path):
        abort(403, 'You cannot create {}s in this folder.'.format(item_type))


def validate_item_and_create(item_name, item_type, owner_group_id):
    timdb = get_timdb()
    validate_item(item_name, item_type)
    item_path, _ = timdb.folders.split_location(item_name)
    Folder.create(item_path, owner_group_id, apply_default_rights=True)


def validate_uploaded_document_content(file_content):
    raw = file_content.read()
    mime = magic.Magic(mime=True)
    mimetype = mime.from_buffer(raw)
    if mimetype not in current_app.config['ALLOWED_DOCUMENT_UPLOAD_MIMETYPES']:
        abort(400, 'Only markdown files are allowed. This file appears to be {}.'.format(mimetype))

    # UnicodeDammit gives incorrect results if the encoding is UTF-8 without BOM,
    # so try the built-in function first.
    try:
        content = raw.decode('utf-8')
    except UnicodeDecodeError:
        content = UnicodeDammit(raw).unicode_markup
    return content
