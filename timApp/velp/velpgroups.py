"""The module contains the database functions related to velp groups and their default and show selections. This
includes adding new velp groups and editing the information of their default and show selections in the document (and
its paragraphs). The module also retrieves or creates the default and personal velp groups. Information about velp group
selections are managed through this module. The module also retrieves the velp groups and their default and show
selections from the database.

:authors: Joonas Lattu, Petteri Palojärvi
:copyright: 2016 Timber project members
:version: 1.0.0

"""

import copy
from typing import Optional, List, Union

from timApp.document.docentry import DocEntry
from timApp.document.docinfo import DocInfo
from timApp.timdb.sqa import db
from timApp.user.user import User
from timApp.user.usergroup import UserGroup
from timApp.velp.velp_models import VelpGroup, VelpGroupsInDocument, VelpGroupSelection, VelpGroupDefaults


def create_default_velp_group(name: str, owner_group: UserGroup, default_group_path: str) -> DocInfo:
    """Creates default velp group for document.

    :param name: Name of the new default velp group.
    :param owner_group: The id of the owner group.
    :param default_group_path: Path of new document / velp group
    :return:

    """

    # Create new document and add its ID to VelpGroupTable
    new_group = DocEntry.create(default_group_path, owner_group)
    new_group_id = new_group.id
    valid_until = None
    vg = VelpGroup(id=new_group_id, name=name, valid_until=valid_until, default_group=True)
    db.session.add(vg)
    return new_group


def create_velp_group(name: str, owner_group: UserGroup, new_group_path: str, valid_until: Optional[str] = None) -> VelpGroup:
    """Create a velp group.

    :param name: Name of the created group.
    :param owner_group: The id of the owner group.
    :param new_group_path: Path of new document / velp group
    :param valid_until: How long velp group is valid (None is forever).
    :return: new velp group ID

    """

    # Create new document and add its ID to VelpGroupTable
    new_group = DocEntry.create(new_group_path, owner_group)
    new_group_id = new_group.id
    vg = VelpGroup(id=new_group_id, name=name, valid_until=valid_until)
    db.session.add(vg)
    return vg


def get_groups_from_document_table(doc_id: int, user_id: int) -> List[VelpGroup]:
    """Gets velp groups from VelpGroupsInDocument table of specific document / user combo.

    :param doc_id: ID of document
    :param user_id: ID of user
    :return: velp groups in document that user has access to.

    """
    return (
        VelpGroupsInDocument.query
            .filter_by(user_id=user_id, doc_id=doc_id)
            .join(VelpGroup)
            .with_entities(VelpGroup)
            .all()
    )


def make_document_a_velp_group(name: str, velp_group_id: int, valid_until: Optional[str] = None,
                               default_group: Optional[bool] = False) -> VelpGroup:
    """Adds document to VelpGroup table.

    :param name: Name of the created group.
    :param velp_group_id: ID of new velp group (and existing document)
    :param valid_until: How long velp group is valid (None is forever)
    :param default_group: Boolean whether velp group should be default or not
    :return: velp group ID

    """
    vg = VelpGroup.query.get(velp_group_id)
    if vg:
        return vg
    vg = VelpGroup(id=velp_group_id, name=name, valid_until=valid_until, default_group=default_group)
    db.session.add(vg)
    return vg


VelpGroupOrDocInfo = Union[VelpGroup, DocInfo]


def add_groups_to_document(velp_groups: List[VelpGroupOrDocInfo], doc: DocInfo, user: User):
    """Adds velp groups to VelpGroupsInDocument table.

    """
    existing: List[VelpGroupsInDocument] = VelpGroupsInDocument.query.filter_by(user_id=user.id, doc_id=doc.id).all()
    existing_ids = set(vgd.velp_group_id for vgd in existing)
    for velp_group in velp_groups:
        velp_group_id = velp_group.id
        if velp_group_id not in existing_ids:
            vgd = VelpGroupsInDocument(user_id=user.id, doc_id=doc.id, velp_group_id=velp_group_id)
            db.session.add(vgd)


def change_selection(doc_id: int, velp_group_id: int, target_type: int, target_id: str, user_id: int,
                     selected: bool):
    """Changes selection for velp group in VelpGroupSelection for specific user / document / target combo.

    :param doc_id: ID of document
    :param velp_group_id: ID of velp group
    :param target_type: 0 document, 1 paragraph
    :param target_id: ID of targeted area
    :param user_id: ID of user
    :param selected: Boolean whether group is selected or not

    """
    vgs: Optional[VelpGroupSelection] = VelpGroupSelection.query.filter_by(
        user_id=user_id,
        doc_id=doc_id,
        velp_group_id=velp_group_id,
        target_id=target_id,
    ).first()
    if vgs:
        vgs.target_type = target_type
        vgs.selected = selected
    else:
        vgs = VelpGroupSelection(
            velp_group_id=velp_group_id,
            user_id=user_id,
            doc_id=doc_id,
            target_type=target_type,
            target_id=target_id,
            selected=selected,
        )
        db.session.add(vgs)


def change_all_target_area_default_selections(doc_id: int, target_type: int, target_id: str, user_id: int,
                                              selected: bool):
    """Change all default selections to True or False for currently chose area (document or paragraph)

    :param doc_id: ID of document
    :param target_type: Currently 0 = document, 1 = paragraph
    :param target_id: ID of target ('0' for documents)
    :param user_id: ID of user (with manage access) to get all defaults from that user's selection table
    :param selected: True or False

    """
    VelpGroupDefaults.query.filter_by(doc_id=doc_id, target_type=target_type, target_id=target_id).delete()
    vgids: List[VelpGroupsInDocument] = VelpGroupsInDocument.query.filter_by(doc_id=doc_id, user_id=user_id).all()
    for vgid in vgids:
        vgd = VelpGroupDefaults(
            doc_id=doc_id,
            target_type=target_type,
            target_id=target_id,
            velp_group_id=vgid.velp_group_id,
            selected=selected,
        )
        db.session.add(vgd)


def change_all_target_area_selections(doc_id: int, target_type: int, target_id: str, user_id: int,
                                      selected: bool):
    """Change all personal selections to True or False for currently chose area (document or paragraph)

    :param doc_id: ID of document
    :param target_type: Currently 0 = document, 1 = paragraph
    :param target_id: ID of target ('0' for documents)
    :param user_id: ID of user
    :param selected: True or False

    """
    if target_type == 0:
        for vgs in VelpGroupSelection.query.filter_by(doc_id=doc_id, target_id=target_id, user_id=user_id).all():
            vgs.selected = selected
    elif target_type == 1:
        VelpGroupSelection.query.filter_by(doc_id=doc_id, target_id=target_id, user_id=user_id, target_type=target_type).delete()
        # target_type is 0 because only 0 always contains all velp groups user has access to.
        # Other target types will get added to database only after they've been clicked once in interface.
        vgss: List[VelpGroupSelection] = VelpGroupSelection.query.filter_by(
            doc_id=doc_id,
            user_id=user_id,
            target_type=0,
        ).all()
        for vgs in vgss:
            nvgs = VelpGroupSelection(
                user_id=user_id,
                doc_id=doc_id,
                target_type=target_type,
                target_id=target_id,
                velp_group_id=vgs.velp_group_id,
                selected=selected,
            )
            db.session.add(nvgs)


def change_default_selection(doc_id: int, velp_group_id: int, target_type: int, target_id: str,
                             selected: bool):
    """Changes selection for velp group's default selection in target area.

    :param doc_id: ID of document
    :param target_type: 0 document, 1 paragraph
    :param target_id: ID of targeted area
    :param velp_group_id: ID of velp group
    :param selected: Boolean whether group is selected or not

    """
    vgd: VelpGroupDefaults = VelpGroupDefaults.query.filter_by(
        doc_id=doc_id,
        velp_group_id=velp_group_id,
        target_id=target_id,
    ).first()
    if vgd:
        vgd.selected = selected
        vgd.target_type = target_type
    else:
        vgd = VelpGroupDefaults(
            doc_id=doc_id,
            target_type=target_type,
            target_id=target_id,
            selected=selected,
            velp_group_id=velp_group_id,
        )
        db.session.add(vgd)


def add_groups_to_selection_table(velp_group: VelpGroup, doc_id: int, user_id: int, target_type: int, target_id: str):
    """Adds velp groups to VelpGroupSelection table.
    """
    vgs = VelpGroupSelection.query.filter_by(
        user_id=user_id,
        doc_id=doc_id,
        velp_group_id=velp_group.id,
        target_id=target_id,
    ).first()
    if vgs:
        vgs.selected = True
        vgs.target_type = target_type
    else:
        vgs = VelpGroupSelection(
            user_id=user_id,
            doc_id=doc_id,
            velp_group_id=velp_group.id,
            target_id=target_id,
            selected=True,
            target_type=target_type,
        )
        db.session.add(vgs)


def process_selection_info(vgss: Union[List[VelpGroupSelection], List[VelpGroupDefaults]]):
    if vgss:
        target_id = vgss[0].target_id
        list_help = []
        target_dict = dict()
        group_dict = dict()
        if target_id != '0':
            target_dict['0'] = []
        for i in range(len(vgss)):
            next_id = vgss[i].target_id
            if next_id != target_id:
                target_dict[target_id] = copy.deepcopy(list_help)
                target_id = next_id
                del list_help[:]
                group_dict['id'] = vgss[i].velp_group_id
                if vgss[i].selected:
                    group_dict['selected'] = True
                else:
                    group_dict['selected'] = False
                list_help.append(copy.deepcopy(group_dict))
                group_dict.clear()
            else:
                group_dict['id'] = vgss[i].velp_group_id
                if vgss[i].selected:
                    group_dict['selected'] = True
                else:
                    group_dict['selected'] = False
                list_help.append(copy.deepcopy(group_dict))
                group_dict.clear()
            if i == len(vgss) - 1:
                target_dict[target_id] = copy.deepcopy(list_help)
        return target_dict
    else:
        return {'0': []}


def get_personal_selections_for_velp_groups(doc_id: int, user_id: int):
    """Gets all velp group personal selections for document.

    :param doc_id: ID of document
    :param user_id: ID of user
    :return: Dict with following info { target_id: [{velp_group_id, selected}, etc], etc }

    """
    vgss = VelpGroupSelection.query.filter_by(doc_id=doc_id, user_id=user_id).order_by(VelpGroupSelection.target_id).all()
    return process_selection_info(vgss)


def get_default_selections_for_velp_groups(doc_id: int):
    """Gets all velp group default selections for document.

    :param doc_id: ID of document
    :return: Dict with following info { target_id: [{velp_group_id, selected}, etc], etc }

    """
    vgds = VelpGroupDefaults.query.filter_by(doc_id=doc_id).order_by(VelpGroupDefaults.target_id).all()
    return process_selection_info(vgds)
