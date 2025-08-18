from collections import defaultdict
from typing import Sequence

from sqlalchemy import func, select, Row
from sqlalchemy.orm import selectinload, joinedload

from timApp.auth.accesstype import AccessType
from timApp.auth.auth_models import BlockAccess
from timApp.folder.folder import Folder
from timApp.item.block import Block, BlockType
from timApp.timdb.sqa import db, run_sql
from timApp.user.special_group_names import (
    ANONYMOUS_USERNAME,
    ANONYMOUS_GROUPNAME,
    LOGGED_IN_GROUPNAME,
    LOGGED_IN_USERNAME,
    ADMIN_GROUPNAME,
    TEACHERS_GROUPNAME,
    GROUPADMIN_GROUPNAME,
)
from timApp.user.user import User, UserInfo, ItemOrBlock
from timApp.user.usergroup import UserGroup
from timApp.user.userutils import (
    get_default_right_document,
    get_or_create_default_right_document,
)


def remove_access(group: UserGroup, i: ItemOrBlock, access_type: AccessType):
    b = i if isinstance(i, Block) else i.block
    return b.accesses.pop((group.id, access_type.value), None)


RightsList = list[BlockAccess]


def get_rights_holders(block_id: int) -> RightsList:
    return get_rights_holders_all([block_id])[block_id]


def get_rights_holders_all(block_ids: list[int], order_by=None):
    if not order_by:
        order_by = User.name
    result: Sequence[Row[BlockAccess, UserGroup, User | None]] = run_sql(
        select(BlockAccess)
        .options(
            joinedload(BlockAccess.usergroup)
            .joinedload(UserGroup.admin_doc)
            .selectinload(Block.docentries)
        )
        .options(selectinload(BlockAccess.atype))
        .filter(BlockAccess.block_id.in_(block_ids))
        .join(UserGroup)
        .outerjoin(User, User.name == UserGroup.name)
        .with_only_columns(BlockAccess, UserGroup, User)
        .order_by(order_by)
    ).all()
    results = defaultdict(list)
    for acc, ug, user in result:
        if user:
            ug.personal_user = user
        results[acc.block_id].append(acc)
    return results


def get_default_rights_holders(folder: Folder, object_type: BlockType) -> RightsList:
    doc = get_default_right_document(folder, object_type)
    if doc is None:
        return []
    return get_rights_holders(doc.id)


def remove_default_access(
    group, folder: Folder, access_type: AccessType, object_type: BlockType
):
    doc = get_or_create_default_right_document(folder, object_type)
    remove_access(group, doc, access_type)


def create_special_usergroups(sess):
    """Creates all special usergroups."""

    anon = User(id=0, name=ANONYMOUS_USERNAME, real_name="Anonymous user")
    logged = User(name=LOGGED_IN_USERNAME)
    anon_group = UserGroup(name=ANONYMOUS_GROUPNAME)
    logged_group = UserGroup(id=0, name=LOGGED_IN_GROUPNAME)
    admin_group = UserGroup(name=ADMIN_GROUPNAME)
    teachers_group = UserGroup(name=TEACHERS_GROUPNAME)
    groupadmin_group = UserGroup(name=GROUPADMIN_GROUPNAME)
    anon.groups.append(anon_group)
    logged.groups.append(logged_group)
    sess.add(anon)
    sess.add(logged)
    sess.add(admin_group)
    sess.add(teachers_group)
    sess.add(groupadmin_group)


def create_anonymous_user(
    name: str,
    real_name: str,
    add_id_to_real_name: bool = False,
) -> User:
    """Creates a new anonymous user.

    :param name: The name of the user to be created.
    :param real_name: The real name of the user.
    :param add_id_to_real_name: If True, the user's ID is appended to the real name.
    :returns: The id of the newly created user.

    """

    next_id = db.session.scalar(select(func.min(User.id))) - 1
    full_name = real_name if not add_id_to_real_name else f"{real_name} {abs(next_id)}"
    u, _ = User.create_with_group(
        UserInfo(username=f"{name}{str(abs(next_id))}", full_name=full_name),
        uid=next_id,
    )
    db.session.add(u)
    return u
