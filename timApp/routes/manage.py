"""Routes for manage view."""
from datetime import timezone

from flask import Blueprint, render_template
from flask import g
from isodate import Duration
from isodate import parse_duration

from options import get_option
from routes.accesshelper import verify_manage_access, verify_ownership, verify_view_access
from routes.sessioninfo import get_current_user_object
from timdb.blocktypes import from_str
from timdb.item import Item
from timdb.models.usergroup import UserGroup
from timdb.tim_models import db
from utils import remove_path_special_chars
from .common import *

manage_page = Blueprint('manage_page',
                        __name__,
                        url_prefix='')  # TODO: Better URL prefix.


@manage_page.route("/manage/<path:path>")
def manage(path):
    if has_special_chars(path):
        return redirect(remove_path_special_chars(request.path) + '?' + request.query_string.decode('utf8'))
    timdb = get_timdb()
    is_folder = False
    doc = DocEntry.find_by_path(path, fallback_to_id=True, try_translation=True)
    folder = None
    if doc is None:
        folder = Folder.find_by_path(path, fallback_to_id=True)
        if folder is None:
            abort(404)
        is_folder = True
        block_id = folder.id
    else:
        block_id = doc.id

    if not verify_manage_access(block_id, require=False, check_duration=True):
        verify_view_access(block_id)
        flash("Did someone give you a wrong link? Showing normal view instead of manage view.")
        return redirect('/view/' + path)

    access_types = timdb.users.get_access_types()

    if is_folder:
        item = folder
    else:
        item = doc
        item.serialize_content = True
        item.changelog_length = get_option(request, 'history', 100)

    return render_template('manage_folder.html' if is_folder else 'manage_document.html',
                           route='manage',
                           translations=doc.translations if not is_folder else None,
                           item=item,
                           access_types=access_types)


@manage_page.route("/changelog/<int:doc_id>/<int:length>")
def get_changelog(doc_id, length):
    verify_manage_access(doc_id)
    doc = DocEntry.find_by_id(doc_id, try_translation=True)
    return jsonResponse({'versions': doc.get_changelog_with_names(length)})


@manage_page.route("/permissions/add/<int:item_id>/<group_name>/<perm_type>", methods=["PUT"])
def add_permission(item_id, group_name, perm_type):
    is_owner, group_ids, acc_from, acc_to, dur_from, dur_to, duration = verify_and_get_params(item_id, group_name, perm_type)
    if get_current_user_object().get_personal_folder().id == item_id:
        if perm_type == 'owner':
            abort(403, 'You cannot add owners to your personal folder.')
    timdb = get_timdb()
    try:
        for group_id in group_ids:
            timdb.users.grant_access(group_id,
                                     item_id,
                                     perm_type,
                                     accessible_from=acc_from,
                                     accessible_to=acc_to,
                                     duration_from=dur_from,
                                     duration_to=dur_to,
                                     duration=duration,
                                     commit=False)
        timdb.commit()
    except KeyError:
        abort(400, 'Invalid permission type.')
    check_ownership_loss(is_owner, item_id, perm_type)
    return okJsonResponse()


@manage_page.route("/permissions/remove/<int:item_id>/<int:group_id>/<perm_type>", methods=["PUT"])
def remove_permission(item_id, group_id, perm_type):
    timdb = get_timdb()
    had_ownership = verify_permission_edit_access(item_id, perm_type)
    try:
        timdb.users.remove_access(group_id, item_id, perm_type)
    except KeyError:
        abort(400, 'Unknown permission type')
    check_ownership_loss(had_ownership, item_id, perm_type)
    return okJsonResponse()


def check_ownership_loss(had_ownership, item_id, perm_type):
    # delete cached ownership information because it may have changed now
    if hasattr(g, 'owned'):
        delattr(g, 'owned')
    if had_ownership and not has_ownership(item_id):
        timdb = get_timdb()
        timdb.users.grant_access(get_current_user_group(), item_id, perm_type)
        abort(403, 'You cannot remove ownership from yourself.')


@manage_page.route("/alias/<int:doc_id>", methods=["GET"])
def get_doc_names(doc_id):
    verify_manage_access(doc_id)
    return jsonResponse(DocEntry.find_by_id(doc_id, try_translation=True).aliases)


@manage_page.route("/alias/<int:doc_id>/<path:new_alias>", methods=["PUT"])
def add_alias(doc_id, new_alias):
    verify_manage_access(doc_id)
    timdb = get_timdb()
    is_public = bool(request.get_json()['public'])

    new_alias = new_alias.strip('/')

    d = DocEntry.find_by_id(doc_id, try_translation=True)

    if not d:
        return abort(404, 'The document does not exist!')

    validate_item(new_alias, 'alias')

    parent_folder, _ = timdb.folders.split_location(new_alias)
    Folder.create(parent_folder, get_current_user_group())
    alias = d.add_alias(new_alias, is_public)
    db.session.commit()
    return okJsonResponse()


@manage_page.route("/alias/<path:alias>", methods=["POST"])
def change_alias(alias):
    timdb = get_timdb()
    alias = alias.strip('/')
    new_alias = request.get_json()['new_name'].strip('/')
    is_public = bool(request.get_json()['public'])

    doc = DocEntry.find_by_path(alias)
    if doc is None:
        return abort(404, 'The document does not exist!')

    verify_manage_access(doc.id)

    new_parent, _ = timdb.folders.split_location(new_alias)

    if alias != new_alias:
        if DocEntry.find_by_path(new_alias, try_translation=True) is not None or timdb.folders.get_folder_id(new_alias) is not None:
            return abort(403, 'Item with a same name already exists.')
        parent, _ = timdb.folders.split_location(alias)
        if not can_write_to_folder(parent):
            return abort(403, "You don't have permission to write to the source folder.")

    if not can_write_to_folder(new_parent):
        return abort(403, "You don't have permission to write to the destination folder.")

    Folder.create(new_parent, get_current_user_group())
    doc.name = new_alias
    doc.public = is_public
    db.session.commit()
    return okJsonResponse()


@manage_page.route("/alias/<path:alias>", methods=["DELETE"])
def remove_alias(alias):
    timdb = get_timdb()
    alias = alias.strip('/')

    doc = DocEntry.find_by_path(alias)
    if doc is None:
        return abort(404, 'The document does not exist!')

    verify_manage_access(doc.id)

    if len(doc.aliases) <= 1:
        return abort(403, "You can't delete the only name the document has.")

    parent_folder, _ = timdb.folders.split_location(alias)

    if not can_write_to_folder(parent_folder):
        return abort(403, "You don't have permission to write to that folder.")

    db.session.delete(doc)
    db.session.commit()
    return okJsonResponse()


@manage_page.route("/rename/<int:doc_id>", methods=["PUT"])
def rename_folder(doc_id):
    timdb = get_timdb()
    new_name = request.get_json()['new_name'].strip('/')

    if timdb.documents.exists(doc_id):
        return abort(403, 'Rename route is no longer supported for documents.')

    if not timdb.folders.exists(doc_id):
        return abort(404, 'The folder does not exist!')

    if not timdb.users.has_manage_access(get_current_user_id(), doc_id):
        return abort(403, "You don't have permission to rename this object.")

    parent, _ = timdb.folders.split_location(new_name)
    parent_id = timdb.folders.get_folder_id(parent)

    if parent_id is None:
        # Maybe do a recursive create with permission checks here later?
        return abort(403, "The location does not exist.")

    if parent_id == doc_id:
        return abort(403, "A folder cannot contain itself.")

    validate_item(new_name, 'folder')

    timdb.folders.rename(doc_id, new_name)
    return jsonResponse({'new_name': new_name})


@manage_page.route("/permissions/get/<int:doc_id>")
def get_permissions(doc_id):
    timdb = get_timdb()
    verify_manage_access(doc_id)
    grouprights = timdb.users.get_rights_holders(doc_id)
    return jsonResponse({'grouprights': grouprights, 'accesstypes': timdb.users.get_access_types()})


@manage_page.route("/defaultPermissions/<object_type>/get/<int:folder_id>")
def get_default_document_permissions(folder_id, object_type):
    timdb = get_timdb()
    verify_manage_access(folder_id)
    grouprights = timdb.users.get_default_rights_holders(folder_id, from_str(object_type))
    return jsonResponse({'grouprights': grouprights})


@manage_page.route("/defaultPermissions/<object_type>/add/<int:folder_id>/<group_name>/<perm_type>", methods=["PUT"])
def add_default_doc_permission(folder_id, group_name, perm_type, object_type):
    _, group_ids, acc_from, acc_to, dur_from, dur_to, duration = verify_and_get_params(folder_id, group_name, perm_type)
    timdb = get_timdb()
    timdb.users.grant_default_access(group_ids,
                                     folder_id,
                                     perm_type,
                                     from_str(object_type),
                                     accessible_from=acc_from,
                                     accessible_to=acc_to,
                                     duration_from=dur_from,
                                     duration_to=dur_to,
                                     duration=duration)
    return okJsonResponse()


@manage_page.route("/defaultPermissions/<object_type>/remove/<int:folder_id>/<int:group_id>/<perm_type>", methods=["PUT"])
def remove_default_doc_permission(folder_id, group_id, perm_type, object_type):
    verify_manage_access(folder_id)
    timdb = get_timdb()
    timdb.users.remove_default_access(group_id, folder_id, perm_type, from_str(object_type))
    return okJsonResponse()


def verify_and_get_params(item_id, group_name, perm_type):
    is_owner = verify_permission_edit_access(item_id, perm_type)
    groups = UserGroup.query.filter(UserGroup.name.in_(group_name.split(';'))).all()
    if len(groups) == 0:
        abort(404, 'No user group with this name was found.')
    group_ids = [group.id for group in groups]
    req_json = request.get_json()
    if req_json is None:
        abort(400)
    access_type = req_json.get('type', 'always')

    try:
        accessible_from = dateutil.parser.parse(req_json.get('from')) if access_type == 'range' else None
    except TypeError:
        accessible_from = None
    try:
        accessible_to = dateutil.parser.parse(req_json.get('to')) if access_type == 'range' else None
    except TypeError:
        accessible_to = None

    try:
        duration_accessible_from = dateutil.parser.parse(req_json.get('durationFrom')) if access_type == 'duration' else None
    except TypeError:
        duration_accessible_from = None
    try:
        duration_accessible_to = dateutil.parser.parse(req_json.get('durationTo')) if access_type == 'duration' else None
    except TypeError:
        duration_accessible_to = None

    duration = parse_duration(req_json.get('duration')) if access_type == 'duration' else None

    if access_type == 'always' or (accessible_from is None and duration is None):
        accessible_from = datetime.now(tz=timezone.utc)

    # SQLAlchemy doesn't know how to adapt Duration instances, so we convert it to timedelta.
    if isinstance(duration, Duration):
        try:
            duration = duration.totimedelta(start=datetime.min)
        except (OverflowError, ValueError):
            abort(400, 'Duration is too long.')
    return is_owner, group_ids, accessible_from, accessible_to, duration_accessible_from, duration_accessible_to, duration


def verify_permission_edit_access(item_id: int, perm_type: str) -> bool:
    """Verifies that the user has right to edit a permission.
    :param item_id: The item id to check for permission.
    :param perm_type: The permission type.
    :return: True if the user has ownership, False if just manage access.
    """
    if perm_type == 'owner':
        verify_ownership(item_id)
        return True
    else:
        verify_manage_access(item_id)
        return False


@manage_page.route("/documents/<int:doc_id>", methods=["DELETE"])
def delete_document(doc_id):
    timdb = get_timdb()
    if not timdb.documents.exists(doc_id):
        return abort(404, 'Document does not exist.')
    verify_ownership(doc_id)
    abort(403, 'Deleting documents has been disabled until a proper backup mechanism is implemented. '
               'Please contact TIM administrators if you really want to delete this document. '
               'You can hide this document from others by removing all permissions.')
    timdb.documents.delete(doc_id)
    return okJsonResponse()


@manage_page.route("/folders/<int:doc_id>", methods=["DELETE"])
def delete_folder(doc_id):
    timdb = get_timdb()
    if not timdb.folders.exists(doc_id):
        return abort(404, 'Folder does not exist.')
    verify_ownership(doc_id)
    if not timdb.folders.is_empty(doc_id):
        return abort(403, "The folder is not empty. Only empty folders can be deleted.")

    timdb.folders.delete(doc_id)
    return okJsonResponse()


@manage_page.route("/changeTitle/<int:item_id>", methods=["PUT"])
def change_title(item_id):
    verify_manage_access(item_id)
    item = Item.find_by_id(item_id)
    new_title, = verify_json_params('new_title')
    item.title = new_title
    db.session.commit()
    return okJsonResponse()
