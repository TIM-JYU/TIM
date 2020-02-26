"""The module handles the main logic related to velps, velp groups and labels. This includes adding and modifiying velps
and labels as well as adding new velp groups. The module also retrieves or creates the default velp group for the
document and the personal default group for the user. Velp groups can be set to shown or shown as default in specific
element (or in the whole document) through this module. The module also retrieves the velps, velp groups and labels to
the document.

:authors: Joonas Lattu, Petteri Palojärvi
:copyright: 2016 Timber project members
:version: 1.0.0

"""
from typing import Dict, List, Optional

from flask import Blueprint
from flask import request

from timApp.auth.accesshelper import verify_logged_in, has_edit_access, has_manage_access, \
    get_doc_or_abort, verify_edit_access, AccessDenied
from timApp.auth.sessioninfo import get_current_user_object, get_current_user_id, get_current_user_group_object
from timApp.document.docentry import DocEntry, get_documents_in_folder, get_documents
from timApp.document.docinfo import DocInfo
from timApp.folder.folder import Folder
from timApp.timdb.sqa import db
from timApp.user.user import User
from timApp.user.users import get_rights_holders
from timApp.user.userutils import grant_access
from timApp.util.flask.requesthelper import RouteException
from timApp.util.flask.responsehelper import json_response, no_cache_json_response, ok_response
from timApp.util.logger import log_warning
from timApp.util.utils import split_location
from timApp.velp.velp_folders import check_velp_group_folder_path, check_personal_velp_folder
from timApp.velp.velp_models import Velp, VelpGroup, VelpGroupSelection, VelpLabel, VelpLabelContent
from timApp.velp.velpgroups import create_default_velp_group, create_velp_group, get_groups_from_document_table, \
    make_document_a_velp_group, add_groups_to_document, change_selection, change_all_target_area_default_selections, \
    change_all_target_area_selections, change_default_selection, add_groups_to_selection_table, \
    get_default_selections_for_velp_groups, get_personal_selections_for_velp_groups
from timApp.velp.velps import create_velp_version, create_new_velp, \
    create_velp_content, update_velp, update_velp_labels, add_labels_to_velp, get_latest_velp_version, \
    get_velp_content_for_document, get_velp_label_content_for_document, add_velp_label_translation

velps = Blueprint('velps',
                  __name__,
                  url_prefix='')


# TODO: Add document handling for all velp group related stuff
# TODO: Done create velp, get velp groups from folders (get_velp_groups),
# TODO: make default velp group and necessary folder (velpabc)
# TODO: Add route for deleting velp (at least a velp without any annotations). Remove by setting valid_until attribute.


@velps.route("/<int:doc_id>/get_default_velp_group", methods=['GET'])
def get_default_velp_group(doc_id: int):
    """Get default velp group ID and if  velp group doesn't exist yet, create one.

    :param doc_id: ID of document
    :return: Dictionary containing default velp group's ID and name

    """
    user = get_current_user_object()

    doc = get_doc_or_abort(doc_id)
    full_path = doc.path
    doc_path, doc_name = split_location(full_path)
    edit_access = False
    if has_edit_access(doc):
        edit_access = True
    else:
        return no_cache_json_response(
            {
                "id": -1,
                "name": "No access to default group",
                "edit_access": edit_access,
            })

    # Check if document's path contains velp groups folder and if it does, make document its own default velp group.
    # This default document declaration isn't saved to database (else eventually all velp groups might be defaults).
    if "velp-groups/" in full_path:
        if has_edit_access(doc):
            edit_access = True
        else:
            return no_cache_json_response(
                {
                    "id": -1,
                    "name": "No access to default group",
                    "edit_access": edit_access,
                })
        vg = make_document_a_velp_group(doc_name, doc_id)
        add_groups_to_document([vg], doc, user)
        add_groups_to_selection_table(vg, doc_id, user.id, target_id='0', target_type=0)
        db.session.commit()
        log_warning("Document is a velp group, made default velp group to point itself")
        return no_cache_json_response({
            "id": doc_id,
            "name": doc_name,
            "edit_access": edit_access,
        })

    if doc_path != "":
        found_velp_groups = get_documents_in_folder(doc_path + "/velp-groups/" + doc_name)
    else:  # Documents in root folder don't like / after empty path
        found_velp_groups = get_documents_in_folder("velp-groups/" + doc_name)
    velp_groups = []
    for v in found_velp_groups:
        # if has_view_access(user_id, timdb.documents.get_document_id(v['name'])):
        velp_groups.append(v.id)
    def_velp_groups: List[VelpGroup] = VelpGroup.query.filter(VelpGroup.id.in_(velp_groups) & VelpGroup.default_group == True).all()
    if def_velp_groups:
        default_group = def_velp_groups[0]
        return no_cache_json_response({
            "id": default_group.id,
            "name": default_group.name,
            "edit_access": bool(has_edit_access(default_group.block)),
        })

    return no_cache_json_response({
        "id": -1,
        "name": doc_name + "_default",
        "edit_access": edit_access,
    })


@velps.route("/get_default_personal_velp_group", methods=['GET'])
def get_default_personal_velp_group():
    """Get default personal velp group ID and if velp group doesn't exist yet, create one.

    :return: Dictionary containing personal velp group data.

    """
    user = get_current_user_object()

    personal_velp_group_path = user.get_personal_folder().path + "/velp-groups"
    found_velp_groups = get_documents_in_folder(personal_velp_group_path)
    velp_groups = []
    for v in found_velp_groups:
        velp_groups.append(v.id)
    default_group = VelpGroup.query.filter(VelpGroup.id.in_(velp_groups) & VelpGroup.default_group == True).first()
    if default_group is not None:
        return no_cache_json_response(default_group)
    group_name = "Personal-default"
    new_group_path = personal_velp_group_path + "/" + group_name
    group = DocEntry.find_by_path(new_group_path)
    if group:
        vg: VelpGroup = VelpGroup.query.get(group.id)
        vg.default_group = True
        vg.valid_until = None
        created_new = False
    else:
        user_group = get_current_user_group_object()
        group = create_default_velp_group(group_name, user_group, new_group_path)
        created_new = True

    created_velp_group = dict()
    created_velp_group['id'] = group.id
    created_velp_group['target_type'] = 0
    created_velp_group['target_id'] = "0"
    created_velp_group['name'] = group_name
    created_velp_group['location'] = new_group_path
    created_velp_group['show'] = True
    created_velp_group['default'] = False
    created_velp_group['edit_access'] = True
    created_velp_group['default_group'] = True
    created_velp_group['created_new_group'] = created_new
    db.session.commit()

    return no_cache_json_response(created_velp_group)


@velps.route("/<int:doc_id>/get_velps", methods=['GET'])
def get_velps(doc_id: int):
    """Get all velps for document user has access to.

    :param doc_id: ID of document
    :return: List of velps as dictionaries containing all needed information

    """
    user_id = get_current_user_id()
    velp_content = get_velp_content_for_document(doc_id, user_id)

    return no_cache_json_response(velp_content)


@velps.route("/<int:doc_id>/get_velp_groups", methods=['GET'])
def get_velp_groups(doc_id: int):
    """Gets all velp groups for document user has access to by using VelpGroupSelection table.

    :param doc_id: ID of document
    :return: List of dictionaries containing velp group information

    """
    user = get_current_user_object()
    doc = get_doc_or_abort(doc_id)
    velp_groups = get_velp_groups_from_tree(doc)
    add_groups_to_document(velp_groups, doc, user)

    all_velp_groups = get_groups_from_document_table(doc_id, user.id)
    db.session.commit()
    return no_cache_json_response([{
        **vg.to_json(),
        'edit_access': bool(has_edit_access(vg.block))
    } for vg in all_velp_groups])


@velps.route("/<int:doc_id>/get_velp_group_personal_selections", methods=['GET'])
def get_velp_group_personal_selections(doc_id: int) -> Dict:
    """Gets default velp group selections for velp groups user has access to in document.

    :param doc_id: ID of document
    :return: Dictionary containing list of selected velp groups for each target area IDs

    """
    user_id = get_current_user_id()
    velp_group_selections = get_personal_selections_for_velp_groups(doc_id, user_id)

    return no_cache_json_response(velp_group_selections)


@velps.route("/<int:doc_id>/get_velp_group_default_selections", methods=['GET'])
def get_velp_group_default_selections(doc_id: int) -> Dict:
    """Gets default velp group selections for velp groups user has access to in document.

    :param doc_id: ID of document
    :return: Dictionary containing list of default velp groups for each target area IDs

    """
    velp_group_defaults = get_default_selections_for_velp_groups(doc_id)

    return no_cache_json_response(velp_group_defaults)


@velps.route("/<int:doc_id>/get_velp_labels", methods=['GET'])
def get_velp_labels(doc_id: int) -> 'str':
    """Gets all velp labels for document user has access to by using VelpGroupSelection table.

    :param doc_id: ID of document
    :return: List of dicts containing velp label IDs and content for the document

    """
    # Todo select language.
    label_data = get_velp_label_content_for_document(doc_id, get_current_user_id())

    return no_cache_json_response(label_data)


@velps.route("/add_velp", methods=['POST'])
def add_velp() -> int:
    """Creates a new velp and adds it to velp groups user chose.

    Required key(s):
        - content: content of the new velp
        - velp_groups: list of velp group IDs of the new velp.

    Optional key(s):
        - points: velp points
        - comment: default comment
        - language_id: language ID
        - color: HEX color
        - valid_until: time stamp to until velp is still valid
        - labels: labels of the velp
        - visible_to: visibility group of the velp (1-4)

    :return: ID of new velp

    """
    json_data = request.get_json()
    try:
        velp_content = json_data['content']
        velp_group_ids = json_data['velp_groups']
    except KeyError as e:
        raise RouteException("Missing data: " + e.args[0])
    if not velp_content:
        raise RouteException("Empty content string.")

    # Optional stuff
    default_points = json_data.get('points')
    default_comment = json_data.get('default_comment')
    language_id = json_data.get('language_id')
    valid_until = json_data.get('valid_until')
    velp_labels = json_data.get('labels')
    visible_to = json_data.get('visible_to')
    color = json_data.get('color')

    default_points = float(default_points) if default_points is not None else None

    verify_logged_in()
    current_user_id = get_current_user_id()

    # Check where user has edit rights and only add new velp to those
    velp_groups: List[VelpGroup] = [
        vg for vg in VelpGroup.query.filter(VelpGroup.id.in_(velp_group_ids)).all() if has_edit_access(vg.block)
    ]

    if not velp_groups:
        raise RouteException("Can't add velp without any velp groups")

    new_velp, _ = create_new_velp(
        current_user_id,
        velp_content,
        default_points,
        default_comment,
        valid_until,
        language_id,
        visible_to,
        color,
    )

    if velp_labels is not None:
        add_labels_to_velp(new_velp.id, velp_labels)
    for vg in velp_groups:
        vg.velps[new_velp.id] = new_velp
    db.session.commit()
    return json_response(new_velp.id)


@velps.route("/<int:doc_id>/update_velp", methods=['POST'])
def update_velp_route(doc_id: int):
    """Updates the velp's data.

    Required key(s):
        - id: velp ID
        - content: velp content
        - language_id: language ID
        - velp groups: list of velp group IDs.

    Optional key(s):
        - points: velp points
        - default_comment: velp default comment
        - labels: velp labels

    :param doc_id: ID of document
    """

    try:
        json_data = request.get_json()
        velp_id = json_data.get('id')
        new_content = json_data.get('content')
        language_id = json_data.get('language_id')
        velp_group_ids = json_data['velp_groups']
    except KeyError as e:
        raise RouteException("Missing data " + e.args[0])
    if not new_content:
        raise RouteException("Empty content string.")

    default_points = json_data.get('points')
    default_comment = json_data.get('default_comment')
    color = json_data.get('color')
    new_labels = set(json_data.get('labels') or [])
    visible_to = json_data.get('visible_to')
    verify_logged_in()
    user_id = get_current_user_id()
    edit_access = False
    velp: Velp = Velp.query.get(velp_id)
    if not velp:
        raise RouteException('Velp not found')
    all_velp_groups = velp.groups

    # Check that user has edit access to velp via any velp group in database
    for group in all_velp_groups.values():
        if has_edit_access(group.block):
            edit_access = True
            break
    if not edit_access:
        raise AccessDenied("No edit access to velp via any velp group.")

    # Add all velp group ids user has edit access to in a document to a remove list
    groups_to_remove = [vg for vg in get_groups_from_document_table(doc_id, user_id) if has_edit_access(vg.block)]

    # Check that user has edit access to velp groups in given velp group list and add them to an add list
    groups_to_add: List[VelpGroup] = [
        vg for vg in VelpGroup.query.filter(VelpGroup.id.in_(velp_group_ids)).all() if has_edit_access(vg.block)
    ]

    # Add and remove velp from velp groups
    if groups_to_add:
        for g in groups_to_remove:
            velp.groups.pop(g.id, None)
        for g in groups_to_add:
            velp.groups[g.id] = g

    old_velp = get_latest_velp_version(velp_id, language_id)
    old_content = old_velp.content

    old_default_comment = old_velp.default_comment

    old_labels = set(lbl.id for lbl in velp.labels.values())
    if old_content != new_content or old_default_comment != default_comment:
        # Todo this does not really work correctly, now any update to any language creates a new version, and we can not
        # produce different contents with the same version but different language.

        version = create_velp_version(velp)
        create_velp_content(version, language_id, new_content, default_comment)
    if old_labels != new_labels:
        update_velp_labels(velp_id, new_labels)
    update_velp(velp_id, default_points, color, visible_to)
    db.session.commit()
    return ok_response()


@velps.route("/add_velp_label", methods=["POST"])
def add_label() -> int:
    """Creates new velp label.

    Required key(s):
        - content: label content

    Optional key(s):
        - language_id: language ID of the label.

    :return: ID of new velp label

    """
    json_data = request.get_json()
    try:
        content = json_data['content']
    except KeyError as e:
        raise RouteException("Missing data: " + e.args[0])
    language_id = json_data.get('language_id')
    language_id = "FI" if language_id is None else language_id

    label = VelpLabel(creator=get_current_user_object())
    db.session.add(label)
    add_velp_label_translation(label, language_id, content)
    db.session.commit()
    return json_response({'id': label.id})


@velps.route("/update_velp_label", methods=["POST"])
def update_velp_label_route():
    """Updates velp label content.

    Required key(s):
        - content: label content
        - id: label ID.

    """
    json_data = request.get_json()
    try:
        content = json_data['content']
        velp_label_id = json_data['id']
    except KeyError as e:
        raise RouteException("Missing data: " + e.args[0])
    language_id = json_data.get('language_id')
    language_id = "FI" if language_id is None else language_id

    vlc: Optional[VelpLabelContent] = VelpLabelContent.query.filter_by(
        language_id=language_id,
        velplabel_id=velp_label_id,
    ).first()
    if not vlc:
        raise RouteException('Velp label not found')
    u = get_current_user_object()
    if not u.is_admin and vlc.velplabel.creator != u:
        raise AccessDenied('Only label creator can modify the label.')
    vlc.content = content
    db.session.commit()
    return ok_response()


@velps.route("/<int:doc_id>/change_selection", methods=["POST"])
def change_selection_route(doc_id: int):
    """Change selection for velp group in users VelpGroupSelection in current document.

    Required key(s):
        - id: velp group iD
        - target_type: target type of the selection (document, paragraph)
        - target_id: target id of the selection (paragraph id or 0 for the whole document)
        - selection_type: 'show' or 'default'.

    :param doc_id: ID of document
    """

    json_data = request.get_json()
    try:
        velp_group_id = json_data['id']
        target_type = json_data['target_type']
        target_id = json_data['target_id']
        selection_type = json_data['selection_type']
    except KeyError as e:
        raise RouteException("Missing data: " + e.args[0])
    verify_logged_in()
    user_id = get_current_user_id()
    d = get_doc_or_abort(doc_id)
    if selection_type == "show":
        try:
            selection = json_data['show']
        except KeyError as e:
            raise RouteException("Missing data: " + e.args[0])
        change_selection(doc_id, velp_group_id, target_type, target_id, user_id, selection)
    elif selection_type == "default" and has_manage_access(d):
        try:
            selection = json_data['default']
        except KeyError as e:
            raise RouteException("Missing data: " + e.args[0])
        change_default_selection(doc_id, velp_group_id, target_type, target_id, selection)

    db.session.commit()
    return ok_response()


@velps.route("/<int:doc_id>/change_all_selections", methods=["POST"])
def change_all_selections(doc_id: int):
    """Change selection for velp group in users VelpGroupSelection in current document.

    Required key(s):
        - selection: 1 or 0 (true or false)
        - target_type: target type of the selection (document, paragraph)
        - target_id: target id of the selection (paragraph id or 0 for the whole document)
        - selection_type: 'show' or 'default'.

    :param doc_id: ID of document
    """

    json_data = request.get_json()
    try:
        selection = json_data['selection']
        target_type = json_data['target_type']
        target_id = json_data['target_id']
        selection_type = json_data['selection_type']
    except KeyError as e:
        raise RouteException("Missing data: " + e.args[0])
    verify_logged_in()
    user_id = get_current_user_id()
    d = get_doc_or_abort(doc_id)
    if selection_type == "show":
        change_all_target_area_selections(doc_id, target_type, target_id, user_id, selection)
    elif selection_type == "default" and has_manage_access(d):
        change_all_target_area_default_selections(doc_id, target_type, target_id, user_id, selection)

    db.session.commit()
    return ok_response()


@velps.route("/<int:doc_id>/reset_target_area_selections_to_defaults", methods=['POST'])
def reset_target_area_selections_to_defaults(doc_id: int):
    """Changes user's personal velp group selections in target area to defaults.

    Required key(s):
        - target_id: target id of the selection (paragraph id or 0 for the whole document)

    :param doc_id: ID of document
    """

    json_data = request.get_json()
    try:
        target_id = json_data['target_id']
    except KeyError as e:
        raise RouteException("Missing data: " + e.args[0])

    user_id = get_current_user_id()

    VelpGroupSelection.query.filter_by(doc_id=doc_id, user_id=user_id, target_id=target_id).delete()
    db.session.commit()
    return ok_response()


@velps.route("/<int:doc_id>/reset_all_selections_to_defaults", methods=['POST'])
def reset_all_selections_to_defaults(doc_id: int):
    """Changes user's all personal velp group selections in document to defaults.

    :param doc_id: ID of document
    """

    user_id = get_current_user_id()
    VelpGroupSelection.query.filter_by(doc_id=doc_id, user_id=user_id).delete()
    db.session.commit()

    return ok_response()


@velps.route("/<int:doc_id>/create_velp_group", methods=['POST'])
def create_velp_group_route(doc_id: int) -> Dict:
    """Creates a new velp group.

    Required key(s):
        - name: velp group name
        - target_type: document, folder or personal group.

    :param doc_id: ID of the document
    :return: Dictionary containing information of new velp group.

    """

    json_data = request.get_json()
    try:
        velp_group_name = json_data.get('name')
        target_type = json_data.get('target_type')
    except KeyError as e:
        raise RouteException("Missing data: " + e.args[0])

    doc = get_doc_or_abort(doc_id)
    full_path = doc.path
    doc_path, doc_name = split_location(full_path)

    # valid_until = json_data.get('valid_until')

    verify_logged_in()
    user_group = get_current_user_group_object()
    user = get_current_user_object()

    # Create a new velp group / document in users/username/velp groups folder
    if target_type == 0:
        user = get_current_user_object()
        user_velp_path = check_personal_velp_folder(user)
        new_group_path = user_velp_path + "/" + velp_group_name
        group_exists = DocEntry.find_by_path(new_group_path)
        if group_exists is None:
            velp_group = create_velp_group(velp_group_name, user_group, new_group_path)
        else:
            raise RouteException("Velp group with same name and location exists already.")

    else:
        if target_type == 2:
            target = Folder.find_by_path(doc_path)
            if not target:
                raise RouteException(f'Folder not found: {doc_path}')
            doc_name = ""
        elif target_type == 1:
            target = doc
        else:
            raise RouteException("Unknown velp group target type.")

        if not has_edit_access(target):
            raise AccessDenied("Edit access is required.")

        # Gives path to either velp groups or velp groups/document name folder
        velps_folder_path = check_velp_group_folder_path(doc_path, user_group, doc_name)

        new_group_path = velps_folder_path + "/" + velp_group_name
        group_exists = DocEntry.find_by_path(new_group_path)  # Check name so no duplicates are made
        if group_exists is None:
            original_owner = target.owners[0]
            velp_group = create_velp_group(velp_group_name, original_owner, new_group_path)
            rights = get_rights_holders(target.id)
            # Copy all rights but view
            for right in rights:
                if not right.atype.name == 'view':
                    grant_access(right.usergroup, velp_group.block, right.atype.to_enum())
        else:
            raise RouteException("Velp group with same name and location exists already.")

    created_velp_group = dict()
    created_velp_group['id'] = velp_group.id
    created_velp_group['target_type'] = 0
    created_velp_group['target_id'] = "0"
    created_velp_group['name'] = velp_group_name
    created_velp_group['location'] = new_group_path
    created_velp_group['selected'] = True
    created_velp_group['show'] = True
    created_velp_group['edit_access'] = True
    created_velp_group['default_group'] = False

    add_groups_to_document([velp_group], doc, user)
    # TODO Do we want to make just created velp group selected in document immediately?
    add_groups_to_selection_table(velp_group, doc_id, user.id, target_id='0', target_type=0)
    db.session.commit()
    return json_response(created_velp_group)


@velps.route("/<int:doc_id>/create_default_velp_group", methods=['POST'])
def create_default_velp_group_route(doc_id: int):
    """Creates a default velp group document or changes existing document to default velp group.

    :param doc_id: ID of document
    :return: Dictionary containing information of new default velp group.

    """

    doc = get_doc_or_abort(doc_id)
    full_path = doc.path
    doc_path, doc_name = split_location(full_path)

    verify_logged_in()
    user_group = doc.block.owners[0]

    verify_edit_access(doc)

    velps_folder_path = check_velp_group_folder_path(doc_path, user_group, doc_name)
    velp_group_name = doc_name + "_default"

    new_group_path = velps_folder_path + "/" + velp_group_name
    group_exists = DocEntry.find_by_path(new_group_path)  # Check name so no duplicates are made
    if group_exists is None:
        velp_group = create_default_velp_group(velp_group_name, user_group, new_group_path)
        created_new_group = True
        rights = get_rights_holders(doc_id)
        # Copy all rights but view
        for right in rights:
            # TODO once someone implements a grant_access that takes access ids instead of strings, change to that
            # function.
            if not right.atype.name == 'view':
                grant_access(right.usergroup, velp_group, right.atype.to_enum())

    else:
        default = DocEntry.find_by_path(new_group_path)
        vg = make_document_a_velp_group(velp_group_name, default.id, None, True)
        velp_group = default
        vg.default_group = True
        vg.valid_until = None
        created_new_group = False

    created_velp_group = dict()
    created_velp_group['id'] = velp_group.id
    created_velp_group['target_type'] = 0
    created_velp_group['target_id'] = "0"
    created_velp_group['name'] = velp_group_name
    created_velp_group['location'] = new_group_path
    created_velp_group['selected'] = True
    created_velp_group['show'] = True
    created_velp_group['default'] = False
    created_velp_group['edit_access'] = True
    created_velp_group['default_group'] = True
    created_velp_group['created_new_group'] = created_new_group

    add_groups_to_document([velp_group], doc, get_current_user_object())

    # TODO Do we want to make just created default velp group selected in document immediately?
    # timdb.velp_groups.add_groups_to_selection_table([created_velp_group], doc_id, user_id)
    db.session.commit()
    return json_response(created_velp_group)


def get_velp_groups_from_tree(doc: DocInfo):
    """Returns all velp groups found from tree from document to root and from users own velp folder.

    Checks document's own velp group folder first, then default velp group folders going up all the
    way to root. Doesn't branch side ways or down, only checks parents. After root has been reached,
    finally checks users own velp group folder.

    Checks that user has minimum of view right for velp groups.

    :return: List of document / velp group information of found hits.

    """

    full_path = doc.path
    doc_path, doc_name = split_location(full_path)
    velp_group_folder = "velp-groups"

    current_path = doc_path
    velp_groups_path = current_path + "/" + velp_group_folder
    doc_velp_path = velp_groups_path + "/" + doc_name
    user = get_current_user_object()
    personal_velps_path = user.get_personal_folder().path + "/" + velp_group_folder

    velp_groups: List[DocEntry] = []

    # Velp groups for areas, plugins etc
    folders = Folder.get_all_in_path(doc_velp_path)
    for path in folders:
        full_path = path.get_full_path()
        velp_groups += get_folder_velp_groups(full_path, user)

    # Document's own velp group
    velp_groups += get_folder_velp_groups(current_path + "/" + velp_group_folder + "/" + doc_name, user)

    # Velp group folder when going towards root in tree
    while True:
        velp_groups += get_folder_velp_groups(current_path + "/" + velp_group_folder, user)
        if current_path == '':
            break
        current_path, _ = split_location(current_path)

    # User's own velp groups
    velp_groups += get_folder_velp_groups(personal_velps_path, user)

    # remove duplicates
    velp_group_ids = set()
    results = []
    for v in velp_groups:
        if v.id not in velp_group_ids:
            velp_group_ids.add(v.id)
            results.append(v)

    # Add found documents to VelpGroup table if they weren't there yet
    for result in results:
        is_velp_group = VelpGroup.query.get(result.id)
        if not is_velp_group:
            _, group_name = split_location(result.path)
            make_document_a_velp_group(group_name, result.id)

    return results


def get_folder_velp_groups(folder, u: User) -> List[DocEntry]:
    return get_documents(include_nonpublic=False,
                         filter_folder=folder,
                         search_recursively=False,
                         filter_user=u)
