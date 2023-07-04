import hashlib
from datetime import datetime, timedelta
from enum import Enum

import bcrypt
from sqlalchemy import select

from timApp.auth.accesstype import AccessType
from timApp.auth.auth_models import AccessTypeModel, BlockAccess
from timApp.document.docinfo import DocInfo
from timApp.document.specialnames import TEMPLATE_FOLDER_NAME
from timApp.folder.folder import Folder
from timApp.item.block import BlockType, Block
from timApp.item.item import ItemBase
from timApp.timdb.exceptions import TimDbException
from timApp.timdb.sqa import db
from timApp.user.special_group_names import ANONYMOUS_GROUPNAME, ANONYMOUS_USERNAME
from timApp.user.usergroup import UserGroup
from timApp.util.utils import get_current_time

ANON_USER_ID: int | None = None
LOGGED_USER_ID: int | None = None
ANON_GROUP_ID: int | None = None
LOGGED_GROUP_ID: int | None = None
ADMIN_GROUP_ID: int | None = None
KORPPI_GROUP_ID: int | None = None
DOC_DEFAULT_RIGHT_NAME = "DefaultDocumentRights"
FOLDER_DEFAULT_RIGHT_NAME = "DefaultFolderRights"

access_type_map: dict[str, int] = {}

default_right_paths = {
    BlockType.Document: f"{TEMPLATE_FOLDER_NAME}/{DOC_DEFAULT_RIGHT_NAME}",
    BlockType.Folder: f"{TEMPLATE_FOLDER_NAME}/{FOLDER_DEFAULT_RIGHT_NAME}",
}


class NoSuchUserException(TimDbException):
    def __init__(self, user_id: int) -> None:
        super().__init__(f"No such user: {user_id}")
        self.user_id = user_id


class DeletedUserException(Exception):
    pass


def get_anon_group_id() -> int:
    global ANON_GROUP_ID
    if ANON_GROUP_ID is not None:
        return ANON_GROUP_ID
    ANON_GROUP_ID = get_usergroup_by_name(ANONYMOUS_GROUPNAME)
    assert ANON_GROUP_ID is not None
    return ANON_GROUP_ID


def get_anon_user_id() -> int:
    global ANON_USER_ID
    if ANON_USER_ID is not None:
        return ANON_USER_ID
    ANON_USER_ID = get_user_id_by_name(ANONYMOUS_USERNAME)
    assert ANON_USER_ID is not None
    return ANON_USER_ID


def get_access_type_id(access_type: str) -> int:
    if not access_type_map:
        result = db.session.execute(select(AccessTypeModel)).scalars().all()
        for row in result:
            access_type_map[row.name] = row.id
    return access_type_map[access_type]


class ReplaceAccessAction(Enum):
    AlwaysPreserve = "always_preserve"
    UpdateDuration = "update_duration"
    AlwaysReplace = "always_replace"


def expire_access(
    group: UserGroup,
    block: ItemBase | Block,
    access_type: AccessType,
) -> tuple[BlockAccess | None, bool]:
    """Expires access to a group for a block.

    :param group: The group to which to grant view access.
    :param block: The block for which to grant view access.
    :param access_type: The kind of access. Possible values are listed in accesstype table.
    :return: The BlockAccess object if there was previous access. Also returns whether the access was expired before.
    """
    ba: BlockAccess | None = (
        db.session.execute(
            select(BlockAccess)
            .filter_by(
                type=access_type.value,
                block_id=block.id,
                usergroup_id=group.id,
            )
            .limit(1)
        )
        .scalars()
        .first()
    )
    if not ba:
        return None, False
    if ba.expired:
        return ba, True
    ba.accessible_to = get_current_time()
    if ba.duration:
        ba.duration = None
        ba.duration_from = None
        ba.duration_to = None
    return ba, False


def grant_access(
    group: UserGroup,
    block: ItemBase | Block,
    access_type: AccessType,
    accessible_from: datetime | None = None,
    accessible_to: datetime | None = None,
    duration_from: datetime | None = None,
    duration_to: datetime | None = None,
    duration: timedelta | None = None,
    require_confirm: bool | None = None,
    replace_active_duration: bool
    | ReplaceAccessAction = ReplaceAccessAction.AlwaysReplace,
) -> BlockAccess:
    """Grants access to a group for a block.

    :param require_confirm: Whether this access needs to be later confirmed by someone with manage access.
    :param duration_from: The optional start time for duration unlock.
    :param duration_to: The optional end time for duration unlock.
    :param accessible_from: The optional start time for the permission.
    :param accessible_to: The optional end time for the permission.
    :param duration: The optional duration for the permission.
    :param group: The group to which to grant view access.
    :param block: The block for which to grant view access.
    :param access_type: The kind of access. Possible values are listed in accesstype table.
    :param replace_active_duration: If true (or ReplaceAccessAction.AlwaysPreserve), replaces any existing access with the new one.
            If false (or ReplaceAccessAction.UpdateDuration)  and the access being granted is duration, modifies the end time of the permission
            to account for new duration.
            Also supports ReplaceAccessAction enum values.
    :return: The BlockAccess object.

    """

    if accessible_from is None and duration is None and not require_confirm:
        # the delta is to ease testing; the clocks of container and PostgreSQL are not perfectly in sync
        accessible_from = get_current_time() - timedelta(milliseconds=50)

    access_id = access_type.value
    block = block if isinstance(block, Block) else block.block
    key = (block.id, access_id)
    b = group.accesses_alt.get(key)

    replace_action = ReplaceAccessAction.AlwaysReplace
    if isinstance(replace_active_duration, bool):
        replace_action = (
            ReplaceAccessAction.AlwaysReplace
            if replace_active_duration
            else ReplaceAccessAction.UpdateDuration
        )
    else:
        replace_action = replace_active_duration

    if b:
        if replace_action == ReplaceAccessAction.AlwaysPreserve:
            return b
        if (
            replace_action == ReplaceAccessAction.UpdateDuration
            and not b.expired
            and not b.unlockable
            and b.accessible_from is not None
            and duration is not None
        ):
            b.accessible_to = b.accessible_from + duration
            if duration_to:
                b.accessible_to = min(b.accessible_to, duration_to)
            return b
        b.accessible_from = accessible_from
        b.accessible_to = accessible_to
        b.duration = duration
        b.duration_from = duration_from
        b.duration_to = duration_to
        b.require_confirm = require_confirm
        return b
    ba = BlockAccess(
        block_id=block.id,
        type=access_id,
        accessible_from=accessible_from,
        accessible_to=accessible_to,
        duration_from=duration_from,
        duration_to=duration_to,
        duration=duration,
        require_confirm=require_confirm,
    )
    group.accesses_alt[key] = ba
    return ba


def get_usergroup_by_name(name: str) -> int | None:
    from timApp.user.usergroup import UserGroup

    ug = UserGroup.get_by_name(name)
    if ug:
        return ug.id
    return None


def get_user_id_by_name(name: str) -> int | None:
    """Gets the id of the specified username.

    :param name: The name of the user.
    :returns: The id of the user or None if the user does not exist.

    """
    from timApp.user.user import User

    u = User.get_by_name(name)
    if u:
        return u.id
    return None


def get_or_create_default_right_document(
    folder: Folder,
    object_type: BlockType,
) -> DocInfo:
    d = get_default_right_document(folder, object_type, create_if_not_exist=True)
    assert d is not None
    return d


def is_some_default_right_document(doc: DocInfo) -> bool:
    for r in default_right_paths.values():
        if doc.path.endswith(r):
            return True
    return False


def get_default_right_document(
    folder: Folder,
    object_type: BlockType,
    create_if_not_exist: bool = False,
) -> DocInfo | None:
    right_doc_path = default_right_paths.get(object_type)
    if right_doc_path is None:
        raise TimDbException(f"Unsupported object type: {object_type}")

    # we don't want to have an owner in the default rights by default
    doc = folder.get_document(
        right_doc_path, create_if_not_exist=create_if_not_exist, creator_group=None
    )
    return doc


def grant_default_access(
    groups: list[UserGroup],
    folder: Folder,
    access_type: AccessType,
    object_type: BlockType,
    accessible_from: datetime | None = None,
    accessible_to: datetime | None = None,
    duration_from: datetime | None = None,
    duration_to: datetime | None = None,
    duration: timedelta | None = None,
) -> list[BlockAccess]:
    doc = get_or_create_default_right_document(folder, object_type)
    accesses = []
    for group in groups:
        accesses.append(
            grant_access(
                group,
                doc,
                access_type,
                accessible_from=accessible_from,
                accessible_to=accessible_to,
                duration_from=duration_from,
                duration_to=duration_to,
                duration=duration,
            )
        )
    return accesses


def hash_password_old(password: str) -> str:
    return hashlib.sha256(password.encode()).hexdigest()


def create_password_hash(password: str) -> str:
    h = bcrypt.hashpw(password.encode(), bcrypt.gensalt()).decode()
    return h


def check_password_hash(password: str, password_hash: str) -> bool:
    try:
        is_bcrypt_ok = bcrypt.checkpw(password.encode(), password_hash.encode())
    except ValueError:
        is_bcrypt_ok = False
    return is_bcrypt_ok


def check_password_hash_old(password: str, password_hash: str) -> bool:
    return password_hash == hash_password_old(password)
