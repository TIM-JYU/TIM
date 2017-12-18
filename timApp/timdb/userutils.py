import hashlib
from datetime import datetime, timedelta, timezone
from typing import Optional, Dict, List

import bcrypt
from sqlalchemy import func

from timApp.documentmodel.specialnames import TEMPLATE_FOLDER_NAME
from timApp.timdb.blocktypes import BlockType, blocktypes
from timApp.timdb.exceptions import TimDbException
from timApp.timdb.models.folder import Folder
from timApp.timdb.special_group_names import ADMIN_GROUPNAME, LOGGED_IN_GROUPNAME, ANONYMOUS_GROUPNAME, \
    LOGGED_IN_USERNAME, ANONYMOUS_USERNAME
from timApp.timdb.tim_models import db, AccessType, BlockAccess, UserGroupMember

ANON_USER_ID = None
LOGGED_USER_ID = None
ANON_GROUP_ID = None
LOGGED_GROUP_ID = None
ADMIN_GROUP_ID = None
KORPPI_GROUP_ID = None
DOC_DEFAULT_RIGHT_NAME = 'DefaultDocumentRights'
FOLDER_DEFAULT_RIGHT_NAME = 'DefaultFolderRights'

access_type_map = {}

default_right_paths = {blocktypes.DOCUMENT: f'{TEMPLATE_FOLDER_NAME}/{DOC_DEFAULT_RIGHT_NAME}',
                       blocktypes.FOLDER: f'{TEMPLATE_FOLDER_NAME}/{FOLDER_DEFAULT_RIGHT_NAME}'}


class NoSuchUserException(TimDbException):
    def __init__(self, user_id):
        super().__init__(f'No such user: {user_id}')
        self.user_id = user_id


def get_admin_group_id() -> int:
    global ADMIN_GROUP_ID
    if ADMIN_GROUP_ID is not None:
        return ADMIN_GROUP_ID
    ADMIN_GROUP_ID = get_usergroup_by_name(ADMIN_GROUPNAME)
    return ADMIN_GROUP_ID


def get_logged_group_id() -> int:
    global LOGGED_GROUP_ID
    if LOGGED_GROUP_ID is not None:
        return LOGGED_GROUP_ID
    LOGGED_GROUP_ID = get_usergroup_by_name(LOGGED_IN_GROUPNAME)
    return LOGGED_GROUP_ID


def get_anon_group_id() -> int:
    global ANON_GROUP_ID
    if ANON_GROUP_ID is not None:
        return ANON_GROUP_ID
    ANON_GROUP_ID = get_usergroup_by_name(ANONYMOUS_GROUPNAME)
    return ANON_GROUP_ID


def get_logged_user_id() -> int:
    global LOGGED_USER_ID
    if LOGGED_USER_ID is not None:
        return LOGGED_USER_ID
    LOGGED_USER_ID = get_user_id_by_name(LOGGED_IN_USERNAME)
    return LOGGED_USER_ID


def get_anon_user_id() -> int:
    global ANON_USER_ID
    if ANON_USER_ID is not None:
        return ANON_USER_ID
    ANON_USER_ID = get_user_id_by_name(ANONYMOUS_USERNAME)
    return ANON_USER_ID


def get_access_type_id(access_type):
    if not access_type_map:
        result = AccessType.query.all()
        for row in result:
            access_type_map[row.name] = row.id
    return access_type_map[access_type]


def user_is_owner(user_id: int, block_id: int) -> bool:
    """Returns whether the user belongs to the owners of the specified block.

    :param user_id:
    :param block_id:
    :returns: True if the user with 'user_id' belongs to the owner group of the block 'block_id'.

    """
    return has_access(user_id, block_id, get_owner_access_id())


def has_seeanswers_access(uid: int, block_id: int) -> bool:
    return has_access(uid, block_id,
                      get_seeanswers_access_id(),
                      get_manage_access_id(),
                      get_teacher_access_id(),
                      get_owner_access_id())


def has_edit_access(user_id: int, block_id: int) -> bool:
    """Returns whether the user has edit access to the specified block.

    :param user_id:
    :param block_id:
    :returns: True if the user with id 'user_id' has edit access to the block 'block_id', false otherwise.

    """

    return has_access(user_id, block_id, get_edit_access_id(), get_manage_access_id(),
                      get_owner_access_id())


def get_viewable_blocks(user_id: int) -> Dict[int, BlockAccess]:
    return get_accessible_blocks(user_id, [get_view_access_id(),
                                           get_edit_access_id(),
                                           get_manage_access_id(),
                                           get_teacher_access_id(),
                                           get_seeanswers_access_id(),
                                           get_owner_access_id()])


def get_editable_blocks(user_id: int) -> Dict[int, BlockAccess]:
    return get_accessible_blocks(user_id, [get_edit_access_id(),
                                           get_manage_access_id(),
                                           get_owner_access_id()])


def get_see_answers_blocks(user_id: int) -> Dict[int, BlockAccess]:
    return get_accessible_blocks(user_id, [get_seeanswers_access_id(),
                                           get_teacher_access_id(),
                                           get_manage_access_id(),
                                           get_owner_access_id()])


def get_teachable_blocks(user_id: int) -> Dict[int, BlockAccess]:
    return get_accessible_blocks(user_id, [get_teacher_access_id(),
                                           get_manage_access_id(),
                                           get_owner_access_id()])


def get_manageable_blocks(user_id: int) -> Dict[int, BlockAccess]:
    return get_accessible_blocks(user_id, [get_manage_access_id(),
                                           get_owner_access_id()])


def get_owned_blocks(user_id: int) -> Dict[int, BlockAccess]:
    return get_accessible_blocks(user_id, [get_owner_access_id()])


def prepare_access_query(access_types, user_ids):
    user_query = db.session.query(UserGroupMember.usergroup_id).filter(UserGroupMember.user_id.in_(user_ids))
    q = BlockAccess.query.filter(
        BlockAccess.usergroup_id.in_(user_query)
        & BlockAccess.type.in_(access_types)
        & (func.current_timestamp().between(BlockAccess.accessible_from, func.coalesce(BlockAccess.accessible_to,
                                                                                       'infinity'))))
    return q


def get_accessible_blocks(user_id: int, access_types: List[int]) -> Dict[int, BlockAccess]:
    if has_admin_access(user_id):
        return {row.block_id: row for row in BlockAccess.query.all()}
    user_ids = [user_id, get_anon_user_id()]
    if user_id > 0:
        user_ids.append(get_logged_user_id())
    q = prepare_access_query(access_types, user_ids)
    return {row.block_id: row for row in q.all()}


def has_access(user_id: int, block_id: int, *access_ids) -> bool:
    """Returns whether the user has any of the specific kind of access types to the specified block.

    :param user_id: The user id to check.
    :param block_id: The block id to check.
    :param access_ids: List of access type ids to be checked.
    :returns: True if the user with id 'user_id' has a specific kind of access to the block 'block_id',
              false otherwise.

    """

    if has_admin_access(user_id):
        return True

    user_ids = [user_id, get_anon_user_id()]
    if user_id > 0:
        user_ids.append(get_logged_user_id())
    q = prepare_access_query(list(access_ids), user_ids).filter_by(block_id=block_id)
    return db.session.query(q.exists()).scalar()


def has_manage_access(user_id: int, block_id: int) -> bool:
    """Returns whether the user has manage access to the specified block.

    :param user_id: The user id to check.
    :param block_id: The block id to check.
    :returns: True if the user with id 'user_id' has manage access to the block 'block_id', false otherwise.

    """
    return has_access(user_id, block_id, get_manage_access_id(),
                      get_owner_access_id())


def has_teacher_access(user_id: int, block_id: int) -> bool:
    """Returns whether the user has teacher access to the specified block.

    :param user_id: The user id to check.
    :param block_id: The block id to check.
    :returns: True if the user with id 'user_id' has teacher access to the block 'block_id', false otherwise.

    """
    return has_access(user_id, block_id, get_manage_access_id(), get_teacher_access_id(),
                      get_owner_access_id())


def has_view_access(user_id: int, block_id: int) -> bool:
    """Returns whether the user has view access to the specified block.

    :param user_id: The user id to check.
    :param block_id: The block id to check.
    :returns: True if the user with id 'user_id' has view access to the block 'block_id', false otherwise.

    """
    return has_access(user_id,
                      block_id,
                      get_view_access_id(),
                      get_edit_access_id(),
                      get_manage_access_id(),
                      get_teacher_access_id(),
                      get_seeanswers_access_id(),
                      get_owner_access_id())


def has_admin_access(user_id: int) -> bool:
    return is_user_id_in_group_id(user_id, get_admin_group_id())


def get_owner_access_id() -> int:
    return get_access_type_id('owner')


def get_seeanswers_access_id() -> int:
    return get_access_type_id('see answers')


def get_manage_access_id() -> int:
    return get_access_type_id('manage')


def get_teacher_access_id() -> int:
    return get_access_type_id('teacher')


def get_edit_access_id() -> int:
    return get_access_type_id('edit')


def get_view_access_id() -> int:
    return get_access_type_id('view')


def grant_edit_access(group_id: int, block_id: int):
    """Grants edit access to a group for a block.

    :param group_id: The group id to which to grant view access.
    :param block_id: The id of the block for which to grant view access.

    """

    grant_access(group_id, block_id, 'edit')


def grant_view_access(group_id: int, block_id: int):
    """Grants view access to a group for a block.

    :param group_id: The group id to which to grant view access.
    :param block_id: The id of the block for which to grant view access.

    """

    grant_access(group_id, block_id, 'view')


def grant_access(group_id: int,
                 block_id: int,
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
    :param group_id: The group id to which to grant view access.
    :param block_id: The id of the block for which to grant view access.
    :param access_type: The kind of access. Possible values are listed in accesstype table.
    :return: The BlockAccess object.

    """

    if accessible_from is None and duration is None:
        # the delta is to ease testing; the clocks of container and PostgreSQL are not perfectly in sync
        accessible_from = datetime.now(tz=timezone.utc) - timedelta(milliseconds=50)

    access_id = get_access_type_id(access_type)
    assert access_id is not None
    ba = BlockAccess(block_id=block_id,
                     usergroup_id=group_id,
                     type=access_id,
                     accessible_from=accessible_from,
                     accessible_to=accessible_to,
                     duration_from=duration_from,
                     duration_to=duration_to,
                     duration=duration)
    db.session.merge(ba)
    if commit:
        db.session.commit()
    return ba


def is_user_id_in_group_id(user_id: int, usergroup_id: int) -> bool:
    q = UserGroupMember.query.filter_by(user_id=user_id, usergroup_id=usergroup_id)
    return db.session.query(q.exists()).scalar()


def get_usergroup_by_name(name: str) -> Optional[int]:
    from timApp.timdb.models.usergroup import UserGroup
    ug = UserGroup.get_by_name(name)
    if ug:
        return ug.id
    return None


def get_user_id_by_name(name: str) -> Optional[int]:
    """Gets the id of the specified username.

    :param name: The name of the user.
    :returns: The id of the user or None if the user does not exist.

    """
    from timApp.timdb.models.user import User
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
                              creator_group_id=None)
    return doc


def grant_default_access(group_ids: List[int],
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
    for group_id in group_ids:
        accesses.append(grant_access(group_id,
                                     doc.id,
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


def check_password_hash(password: str, password_hash: str, allow_old=False) -> bool:
    try:
        is_bcrypt_ok = bcrypt.checkpw(password.encode(), password_hash.encode())
    except ValueError:
        is_bcrypt_ok = False
    if is_bcrypt_ok:
        return True
    if not allow_old:
        return False
    return password_hash == hash_password_old(password)
