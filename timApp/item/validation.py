import re

import attr
import magic
from bs4 import UnicodeDammit
from flask import current_app
from werkzeug.exceptions import abort

from timApp.auth.sessioninfo import logged_in, get_current_user_object
from timApp.item.block import BlockType
from timApp.timdb.exceptions import ItemAlreadyExistsException
from timApp.document.docentry import DocEntry
from timApp.folder.folder import Folder
from timApp.user.usergroup import UserGroup
from timApp.util.utils import split_location


@attr.s(auto_attribs=True)
class ItemValidationRule:
    """Rules for item validation."""

    check_write_perm: bool = True
    """Whether to check for write permission of containing folder."""
    require_login: bool = True



def validate_item(item_path: str,
                  item_type: BlockType,
                  validation_rule: ItemValidationRule = None):
    if not validation_rule:
        validation_rule = ItemValidationRule()
    item_type_str = item_type.name.lower()
    if validation_rule.require_login and not logged_in():
        abort(403, f'You have to be logged in to perform this action.')

    if item_path is None:
        abort(400, 'item_path was None')

    if not all(part for part in item_path.split('/')):
        abort(400, f'The {item_type_str} path cannot have empty parts.')

    if re.match(r'^(\d)*$', item_path) is not None:
        abort(400, f'The {item_type_str} path can not be a number to avoid confusion with document id.')

    if has_special_chars(item_path):
        abort(400,
              f'The {item_type_str} path has invalid characters. Only letters, numbers, underscores and dashes are allowed.')

    if DocEntry.find_by_path(item_path) is not None or Folder.find_by_path(item_path) is not None:
        raise ItemAlreadyExistsException('Item with a same name already exists.')

    f = Folder.find_first_existing(item_path)
    if not f:
        abort(403)
    if validation_rule.check_write_perm:
        if not get_current_user_object().can_write_to_folder(f):
            abort(403, f'You cannot create {item_type_str}s in this folder.')


def validate_item_and_create_intermediate_folders(item_path: str,
                                                  item_type: BlockType,
                                                  owner_group: UserGroup=None,
                                                  validation_rule: ItemValidationRule = None):
    validate_item(item_path, item_type, validation_rule)
    item_path, _ = split_location(item_path)
    Folder.create(item_path, owner_group, apply_default_rights=True)


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


def has_special_chars(item_path: str):
    return set(item_path.lower()) - set('abcdefghijklmnopqrstuvwxyz0123456789/-_')
