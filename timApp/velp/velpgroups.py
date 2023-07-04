"""The module contains the database functions related to velp groups and their default and show selections. This
includes adding new velp groups and editing the information of their default and show selections in the document (and
its paragraphs). The module also retrieves or creates the default and personal velp groups. Information about velp group
selections are managed through this module. The module also retrieves the velp groups and their default and show
selections from the database.

:authors: Joonas Lattu, Petteri Palojärvi
:copyright: 2016 Timber project members
:version: 1.0.0

"""
from dataclasses import dataclass, field
from typing import Union

from sqlalchemy import select, delete

from timApp.auth.accesstype import AccessType
from timApp.document.docentry import DocEntry
from timApp.document.docinfo import DocInfo
from timApp.timdb.sqa import db
from timApp.user.user import User
from timApp.user.usergroup import UserGroup
from timApp.user.users import get_rights_holders
from timApp.user.userutils import grant_access
from timApp.util.utils import split_location
from timApp.velp.velp_folders import (
    check_velp_group_folder_path,
)
from timApp.velp.velp_models import (
    VelpGroup,
    VelpGroupsInDocument,
    VelpGroupSelection,
    VelpGroupDefaults,
)


@dataclass
class CreatedVelpGroup:
    """Represents a velp group. Used for passing velp group data to the user interface."""

    id: int
    name: str
    location: str
    target_type: int = 0
    target_id: str = "0"
    edit_access: bool = True
    show: bool = True
    selected: bool = True
    default: bool = False
    default_group: bool = False
    created_new_group: bool = True


@dataclass
class GroupSelection:
    """Stores information on a velp group's selection status"""

    id: int
    selected: bool

    def to_json(self) -> dict:
        return {"id": self.id, "selected": self.selected}


@dataclass
class VelpGroupSelectionInfo:
    """Stores information on selected and default velp groups for a document. Used for
    passing velp group selection data to the user interface.
    """

    target_ids: list[str] = field(default_factory=list)
    selections: list[list[GroupSelection]] = field(default_factory=list)

    def append(self, target_id: str, gs: GroupSelection) -> None:
        if not target_id:
            return
        index = next(
            (i for i, tid in enumerate(self.target_ids) if target_id == tid), -1
        )
        if index < 0:
            # if the index was not found, we need to create a new entry
            self.target_ids.append(target_id)
            self.selections.append([])
            index = self.target_ids.index(target_id)
        self.selections[index].append(gs)

    def to_json(self) -> dict:
        if not self.target_ids:
            return {"0": []}
        result = dict()
        for t_id, selects in zip(self.target_ids, self.selections):
            result[t_id] = list(map(GroupSelection.to_json, selects))
        return result


def create_default_velp_group(
    name: str, owner_group: UserGroup, default_group_path: str
) -> DocInfo:
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
    vg = VelpGroup(
        id=new_group_id, name=name, valid_until=valid_until, default_group=True
    )
    db.session.add(vg)
    return new_group


def set_default_velp_group_rights(doc_id: int, velp_group: DocInfo) -> None:
    rights = get_rights_holders(doc_id)
    # Copy all rights but view
    for right in rights:
        if right.access_type != AccessType.view:
            grant_access(right.usergroup, velp_group, right.access_type)


def get_document_default_velp_group_info(doc_info: DocInfo) -> tuple[str, str]:
    """
    Returns path and name for a document's default group
    """
    full_path = doc_info.path_without_lang
    doc_path, doc_name = split_location(full_path)
    user_group = doc_info.block.owners[0]
    velps_folder_path = check_velp_group_folder_path(doc_path, user_group, doc_name)
    velp_group_name = doc_name + "_default"
    return velps_folder_path + "/" + velp_group_name, velp_group_name


def get_document_default_velp_group(
    doc_info: DocInfo,
) -> tuple[DocInfo | None, str, str]:
    """
    Returns document default velp group, default velp group path and default name for velp group
    """
    velp_group_path, velp_group_name = get_document_default_velp_group_info(doc_info)
    return DocEntry.find_by_path(velp_group_path), velp_group_path, velp_group_name


def set_default_velp_group_selected_and_visible(doc_info: DocInfo) -> None:
    """
    Makes document's default velp group visible and selected for everyone
    """
    (
        velp_group,
        default_group_path,
        default_group_name,
    ) = get_document_default_velp_group(doc_info)
    if not velp_group:
        velp_group = create_default_velp_group(
            default_group_name, doc_info.block.owners[0], default_group_path
        )
        set_default_velp_group_rights(doc_info.document.id, velp_group)
    grant_access(UserGroup.get_logged_in_group(), velp_group, AccessType.view)
    change_default_selection(doc_info.document.id, velp_group.id, 0, "0", True)
    db.session.commit()


def create_velp_group(
    name: str,
    owner_group: UserGroup,
    new_group_path: str,
    valid_until: str | None = None,
) -> VelpGroup:
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


def get_groups_from_document_table(doc_id: int, user_id: int | None) -> list[VelpGroup]:
    """Gets velp groups from VelpGroupsInDocument table of specific document / user combo.
       If user_id is none, fetches all velp groups attached to the document.

    :param doc_id: ID of document
    :param user_id: ID of user
    :return: velp groups in document that user has access to, or all velp groups in document if user_id is None

    """
    if not user_id:
        return (
            db.session.execute(
                select(VelpGroupsInDocument)
                .filter_by(doc_id=doc_id)
                .join(VelpGroup)
                .with_only_columns(VelpGroup)
            )
            .scalars()
            .all()
        )
    return (
        db.session.execute(
            select(VelpGroupsInDocument)
            .filter_by(user_id=user_id, doc_id=doc_id)
            .join(VelpGroup)
            .with_only_columns(VelpGroup)
        )
        .scalars()
        .all()
    )


def make_document_a_velp_group(
    name: str,
    velp_group_id: int,
    valid_until: str | None = None,
    default_group: bool | None = False,
) -> VelpGroup:
    """Adds document to VelpGroup table.

    :param name: Name of the created group.
    :param velp_group_id: ID of new velp group (and existing document)
    :param valid_until: How long velp group is valid (None is forever)
    :param default_group: Boolean whether velp group should be default or not
    :return: velp group ID

    """
    vg = db.session.get(VelpGroup, velp_group_id)
    if vg:
        return vg
    vg = VelpGroup(
        id=velp_group_id,
        name=name,
        valid_until=valid_until,
        default_group=default_group,
    )
    db.session.add(vg)
    return vg


VelpGroupOrDocInfo = Union[VelpGroup, DocInfo]


def add_groups_to_document(
    velp_groups: list[VelpGroupOrDocInfo], doc: DocInfo, user: User
) -> None:
    """Adds velp groups to VelpGroupsInDocument table."""
    existing: list[VelpGroupsInDocument] = (
        db.session.execute(
            select(VelpGroupsInDocument).filter_by(user_id=user.id, doc_id=doc.id)
        )
        .scalars()
        .all()
    )
    existing_ids = {vgd.velp_group_id for vgd in existing}
    for velp_group in velp_groups:
        velp_group_id = velp_group.id
        if velp_group_id not in existing_ids:
            vgd = VelpGroupsInDocument(
                user_id=user.id, doc_id=doc.id, velp_group_id=velp_group_id
            )
            db.session.add(vgd)


def change_selection(
    doc_id: int,
    velp_group_id: int,
    target_type: int,
    target_id: str,
    user_id: int,
    selected: bool,
) -> None:
    """Changes selection for velp group in VelpGroupSelection for specific user / document / target combo.

    :param doc_id: ID of document
    :param velp_group_id: ID of velp group
    :param target_type: 0 document, 1 paragraph
    :param target_id: ID of targeted area
    :param user_id: ID of user
    :param selected: Boolean whether group is selected or not

    """
    vgs: VelpGroupSelection | None = (
        db.session.execute(
            select(VelpGroupSelection)
            .filter_by(
                user_id=user_id,
                doc_id=doc_id,
                velp_group_id=velp_group_id,
                target_id=target_id,
            )
            .limit(1)
        )
        .scalars()
        .first()
    )
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


def change_all_target_area_default_selections(
    doc_id: int, target_type: int, target_id: str, user_id: int, selected: bool
) -> None:
    """Change all default selections to True or False for currently chose area (document or paragraph)

    :param doc_id: ID of document
    :param target_type: Currently 0 = document, 1 = paragraph
    :param target_id: ID of target ('0' for documents)
    :param user_id: ID of user (with manage access) to get all defaults from that user's selection table
    :param selected: True or False

    """
    db.session.execute(
        delete(VelpGroupDefaults).where(
            (VelpGroupDefaults.doc_id == doc_id)
            & (VelpGroupDefaults.target_type == target_type)
            & (VelpGroupDefaults.target_id == target_id)
        )
    )
    vgids: list[VelpGroupsInDocument] = (
        db.session.execute(
            select(VelpGroupsInDocument).filter_by(doc_id=doc_id, user_id=user_id)
        )
        .scalars()
        .all()
    )
    for vgid in vgids:
        vgd = VelpGroupDefaults(
            doc_id=doc_id,
            target_type=target_type,
            target_id=target_id,
            velp_group_id=vgid.velp_group_id,
            selected=selected,
        )
        db.session.add(vgd)


def change_all_target_area_selections(
    doc_id: int, target_type: int, target_id: str, user_id: int, selected: bool
) -> None:
    """Change all personal selections to True or False for currently chose area (document or paragraph)

    :param doc_id: ID of document
    :param target_type: Currently 0 = document, 1 = paragraph
    :param target_id: ID of target ('0' for documents)
    :param user_id: ID of user
    :param selected: True or False

    """
    if target_type == 0:
        for vgs in (
            db.session.execute(
                select(VelpGroupSelection).filter_by(
                    doc_id=doc_id, target_id=target_id, user_id=user_id
                )
            )
            .scalars()
            .all()
        ):
            vgs.selected = selected
    elif target_type == 1:
        db.session.execute(
            delete(VelpGroupSelection).where(
                (VelpGroupSelection.doc_id == doc_id)
                & (VelpGroupSelection.target_id == target_id)
                & (VelpGroupSelection.user_id == user_id)
                & (VelpGroupSelection.target_type == target_type)
            )
        )
        # target_type is 0 because only 0 always contains all velp groups user has access to.
        # Other target types will get added to database only after they've been clicked once in interface.
        vgss: list[VelpGroupSelection] = (
            db.session.execute(
                select(VelpGroupSelection).filter_by(
                    doc_id=doc_id,
                    user_id=user_id,
                    target_type=0,
                )
            )
            .scalars()
            .all()
        )
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


def change_default_selection(
    doc_id: int, velp_group_id: int, target_type: int, target_id: str, selected: bool
) -> None:
    """Changes selection for velp group's default selection in target area.

    :param doc_id: ID of document
    :param target_type: 0 document, 1 paragraph
    :param target_id: ID of targeted area
    :param velp_group_id: ID of velp group
    :param selected: Boolean whether group is selected or not

    """
    vgd: VelpGroupDefaults = (
        db.session.execute(
            select(VelpGroupDefaults)
            .filter_by(
                doc_id=doc_id,
                velp_group_id=velp_group_id,
                target_id=target_id,
            )
            .limit(1)
        )
        .scalars()
        .first()
    )
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


def add_groups_to_selection_table(
    velp_group: VelpGroup,
    doc_id: int,
    user_id: int,
    target_type: int,
    target_id: str,
) -> None:
    """Adds velp groups to VelpGroupSelection table."""
    vgs = (
        db.session.execute(
            select(VelpGroupSelection)
            .filter_by(
                user_id=user_id,
                doc_id=doc_id,
                velp_group_id=velp_group.id,
                target_id=target_id,
            )
            .limit(1)
        )
        .scalars()
        .first()
    )
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


def process_selection_info(
    vgss: list[VelpGroupSelection] | list[VelpGroupDefaults],
) -> VelpGroupSelectionInfo:
    groups = VelpGroupSelectionInfo()
    if vgss:
        target_id: str = "0"
        i: int = 0
        while i < len(vgss):
            if target_id == vgss[i].target_id:
                selection = GroupSelection(
                    id=vgss[i].velp_group_id, selected=vgss[i].selected
                )
                groups.append(target_id, selection)
                i += 1
            else:
                target_id = vgss[i].target_id

    return groups


def get_personal_selections_for_velp_groups(
    doc_id: int, user_id: int
) -> VelpGroupSelectionInfo:
    """Gets all velp group personal selections for document.

    :param doc_id: ID of document
    :param user_id: ID of user
    :return: Dict with following info { target_id: [{velp_group_id, selected}, etc], etc }

    """
    vgss = (
        db.session.execute(
            select(VelpGroupSelection)
            .filter_by(doc_id=doc_id, user_id=user_id)
            .order_by(VelpGroupSelection.target_id)
        )
        .scalars()
        .all()
    )
    return process_selection_info(vgss)


def get_default_selections_for_velp_groups(
    doc_id: int,
) -> VelpGroupSelectionInfo:
    """Gets all velp group default selections for document.

    :param doc_id: ID of document
    :return: Dict with following info { target_id: [{velp_group_id, selected}, etc], etc }

    """
    vgds = (
        db.session.execute(
            select(VelpGroupDefaults)
            .filter_by(doc_id=doc_id)
            .order_by(VelpGroupDefaults.target_id)
        )
        .scalars()
        .all()
    )
    return process_selection_info(vgds)
