import hashlib
from datetime import datetime, timedelta
from typing import Optional, List, Union

import bcrypt

from timApp.auth.auth_models import AccessTypeModel, BlockAccess
from timApp.document.specialnames import TEMPLATE_FOLDER_NAME
from timApp.folder.folder import Folder
from timApp.item.block import BlockType, Block
from timApp.item.item import ItemBase
from timApp.timdb.exceptions import TimDbException
from timApp.timdb.sqa import db
from timApp.user.special_group_names import ANONYMOUS_GROUPNAME, \
    ANONYMOUS_USERNAME
from timApp.util.utils import get_current_time

ANON_USER_ID = None
LOGGED_USER_ID = None
ANON_GROUP_ID = None
LOGGED_GROUP_ID = None
ADMIN_GROUP_ID = None
KORPPI_GROUP_ID = None
DOC_DEFAULT_RIGHT_NAME = 'DefaultDocumentRights'
FOLDER_DEFAULT_RIGHT_NAME = 'DefaultFolderRights'

access_type_map = {}

default_right_paths = {BlockType.Document: f'{TEMPLATE_FOLDER_NAME}/{DOC_DEFAULT_RIGHT_NAME}',
                       BlockType.Folder: f'{TEMPLATE_FOLDER_NAME}/{FOLDER_DEFAULT_RIGHT_NAME}'}


class NoSuchUserException(TimDbException):
    def __init__(self, user_id):
        super().__init__(f'No such user: {user_id}')
        self.user_id = user_id


def get_anon_group_id() -> int:
    global ANON_GROUP_ID
    if ANON_GROUP_ID is not None:
        return ANON_GROUP_ID
    ANON_GROUP_ID = get_usergroup_by_name(ANONYMOUS_GROUPNAME)
    return ANON_GROUP_ID


def get_anon_user_id() -> int:
    global ANON_USER_ID
    if ANON_USER_ID is not None:
        return ANON_USER_ID
    ANON_USER_ID = get_user_id_by_name(ANONYMOUS_USERNAME)
    return ANON_USER_ID


def get_access_type_id(access_type):
    if not access_type_map:
        result = AccessTypeModel.query.all()
        for row in result:
            access_type_map[row.name] = row.id
    return access_type_map[access_type]


def grant_edit_access(group, block):
    """Grants edit access to a group for a block.

    :param group: The group to which to grant view access.
    :param block: The block for which to grant view access.

    """

    grant_access(group, block, 'edit')


def grant_view_access(group, block):
    """Grants view access to a group for a block.

    :param group: The group to which to grant view access.
    :param block: The block for which to grant view access.

    """

    grant_access(group, block, 'view')


def grant_access(group,
                 block: Union[ItemBase, Block],
                 access_type: str,
                 accessible_from: Optional[datetime] = None,
                 accessible_to: Optional[datetime] = None,
                 duration_from: Optional[datetime] = None,
                 duration_to: Optional[datetime] = None,
                 duration: Optional[timedelta] = None,
                 commit: bool = True) -> BlockAccess:
    """Grants access to a group for a block.

    :param duration_from: The optional start time for duration unlock.
    :param duration_to: The optional end time for duration unlock.
    :param accessible_from: The optional start time for the permission.
    :param accessible_to: The optional end time for the permission.
    :param duration: The optional duration for the permission.
    :param commit: Whether to commit changes immediately.
    :param group: The group to which to grant view access.
    :param block: The block for which to grant view access.
    :param access_type: The kind of access. Possible values are listed in accesstype table.
    :return: The BlockAccess object.

    """

    if accessible_from is None and duration is None:
        # the delta is to ease testing; the clocks of container and PostgreSQL are not perfectly in sync
        accessible_from = get_current_time() - timedelta(milliseconds=50)

    access_id = get_access_type_id(access_type)
    assert access_id is not None
    block = block if isinstance(block, Block) else block.block
    for b in group.accesses:  # type: BlockAccess
        if b.type == access_id and b.block_id == block.id:
            b.accessible_from = accessible_from
            b.accessible_to = accessible_to
            b.duration = duration
            b.duration_from = duration_from
            b.duration_to = duration_to
            if commit:
                db.session.commit()
            return b
    ba = BlockAccess(block_id=block.id,
                     type=access_id,
                     accessible_from=accessible_from,
                     accessible_to=accessible_to,
                     duration_from=duration_from,
                     duration_to=duration_to,
                     duration=duration)
    group.accesses.append(ba)
    if commit:
        db.session.commit()
    return ba


def get_usergroup_by_name(name: str) -> Optional[int]:
    from timApp.user.usergroup import UserGroup
    ug = UserGroup.get_by_name(name)
    if ug:
        return ug.id
    return None


def get_user_id_by_name(name: str) -> Optional[int]:
    """Gets the id of the specified username.

    :param name: The name of the user.
    :returns: The id of the user or None if the user does not exist.

    """
    from timApp.user.user import User
    u = User.get_by_name(name)
    if u:
        return u.id
    return None


def get_default_right_document(folder_id, object_type: BlockType, create_if_not_exist=False):
    folder = Folder.get_by_id(folder_id)
    if folder is None:
        raise TimDbException('Non-existent folder')
    right_doc_path = default_right_paths.get(object_type)
    if right_doc_path is None:
        raise TimDbException(f'Unsupported object type: {object_type}')

    # we don't want to have an owner in the default rights by default
    doc = folder.get_document(right_doc_path,
                              create_if_not_exist=create_if_not_exist,
                              creator_group=None)
    return doc


def grant_default_access(groups,
                         folder_id: int,
                         access_type: str,
                         object_type: BlockType,
                         accessible_from: Optional[datetime] = None,
                         accessible_to: Optional[datetime] = None,
                         duration_from: Optional[datetime] = None,
                         duration_to: Optional[datetime] = None,
                         duration: Optional[timedelta] = None) -> List[BlockAccess]:
    doc = get_default_right_document(folder_id, object_type, create_if_not_exist=True)
    accesses = []
    for group in groups:
        accesses.append(grant_access(group,
                                     doc,
                                     access_type,
                                     commit=False,
                                     accessible_from=accessible_from,
                                     accessible_to=accessible_to,
                                     duration_from=duration_from,
                                     duration_to=duration_to,
                                     duration=duration))
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
