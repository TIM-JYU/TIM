import re

import attr
import magic
from bs4 import UnicodeDammit
from flask import current_app

from timApp.auth.accesshelper import AccessDenied
from timApp.auth.sessioninfo import logged_in, get_current_user_object
from timApp.document.docentry import DocEntry
from timApp.folder.createopts import FolderCreationOptions
from timApp.folder.folder import Folder
from timApp.item.block import BlockType
from timApp.timdb.exceptions import ItemAlreadyExistsException
from timApp.user.usergroup import UserGroup
from timApp.util.flask.requesthelper import RouteException
from timApp.util.utils import split_location


@attr.s(auto_attribs=True)
class ItemValidationRule:
    """Rules for item validation."""

    check_write_perm: bool = True
    """Whether to check for write permission of containing folder."""
    require_login: bool = True


def validate_item(
    item_path: str, item_type: BlockType, validation_rule: ItemValidationRule = None
):
    if not validation_rule:
        validation_rule = ItemValidationRule()
    item_type_str = item_type.name.lower()
    if validation_rule.require_login and not logged_in():
        raise AccessDenied(f"You have to be logged in to perform this action.")

    if item_path is None:
        raise RouteException("item_path was None")

    segments = item_path.split("/")

    if not all(part for part in segments):
        raise RouteException(f"The {item_type_str} path cannot have empty parts.")

    if any("." in part for part in segments[:-1]):
        raise RouteException(f"Item path segment cannot have dots.")

    short_name = segments[-1]
    if item_type == BlockType.Folder:
        if "." in short_name:
            raise RouteException(f"Folder short name cannot have dots.")
    else:
        if re.match(r"^\.+$", short_name):
            raise RouteException(f"Document short name cannot consist of merely dots.")
        if short_name.startswith(".") or short_name.endswith("."):
            raise RouteException(f"Document short name cannot start or end with a dot.")

    if re.match(r"^(\d)*$", item_path) is not None:
        raise RouteException(
            f"The {item_type_str} path can not be a number to avoid confusion with document id."
        )

    if has_special_chars(item_path):
        raise RouteException(
            f"The {item_type_str} path has invalid characters. Only letters, numbers, underscores and dashes are allowed."
        )

    if (
        DocEntry.find_by_path(item_path) is not None
        or Folder.find_by_path(item_path) is not None
    ):
        raise ItemAlreadyExistsException("Item with a same name already exists.")

    f = Folder.find_first_existing(item_path)
    if not f:
        raise AccessDenied()
    if validation_rule.check_write_perm:
        if not get_current_user_object().can_write_to_folder(f):
            raise AccessDenied(f"You cannot create {item_type_str}s in this folder.")


def validate_item_and_create_intermediate_folders(
    item_path: str,
    item_type: BlockType,
    owner_group: UserGroup | None = None,
    validation_rule: ItemValidationRule | None = None,
    apply_default_rights: bool = True,
):
    validate_item(item_path, item_type, validation_rule)
    item_path, _ = split_location(item_path)
    Folder.create(
        item_path,
        owner_group,
        creation_opts=FolderCreationOptions(apply_default_rights=apply_default_rights),
    )


def validate_uploaded_document_content(file_content):
    raw = file_content.read()
    mime = magic.Magic(mime=True)
    mimetype = mime.from_buffer(raw)
    if mimetype not in current_app.config["ALLOWED_DOCUMENT_UPLOAD_MIMETYPES"]:
        raise RouteException(
            f"Only markdown files are allowed. This file appears to be {mimetype}."
        )

    # UnicodeDammit gives incorrect results if the encoding is UTF-8 without BOM,
    # so try the built-in function first.
    try:
        content = raw.decode("utf-8")
    except UnicodeDecodeError:
        content = UnicodeDammit(raw).unicode_markup
    return content


def has_special_chars(item_path: str):
    return set(item_path.lower()) - set("abcdefghijklmnopqrstuvwxyz0123456789/-_.")
