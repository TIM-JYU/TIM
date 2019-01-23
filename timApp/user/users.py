from sqlalchemy import func

from timApp.auth.auth_models import BlockAccess
from timApp.item.block import Block, BlockType
from timApp.timdb.sqa import db
from timApp.timdb.timdbbase import TimDbBase
from timApp.timdb.timdbbase import result_as_dict_list
from timApp.user.special_group_names import ANONYMOUS_USERNAME, ANONYMOUS_GROUPNAME, KORPPI_GROUPNAME, \
    LOGGED_IN_GROUPNAME, \
    LOGGED_IN_USERNAME, ADMIN_GROUPNAME, TEACHERS_GROUPNAME, GROUPADMIN_GROUPNAME
from timApp.user.user import User
from timApp.user.usergroup import UserGroup
from timApp.user.userutils import get_access_type_id, get_default_right_document


class Users(TimDbBase):
    """Handles saving and retrieving user-related information to/from the database."""

    def create_special_usergroups(self):
        """Creates all special usergroups."""

        anon = User(id=0, name=ANONYMOUS_USERNAME, real_name='Anonymous user')
        logged = User(name=LOGGED_IN_USERNAME)
        anon_group = UserGroup(name=ANONYMOUS_GROUPNAME)
        logged_group = UserGroup(id=0, name=LOGGED_IN_GROUPNAME)
        korppi_group = UserGroup(name=KORPPI_GROUPNAME)
        admin_group = UserGroup(name=ADMIN_GROUPNAME)
        teachers_group = UserGroup(name=TEACHERS_GROUPNAME)
        groupadmin_group = UserGroup(name=GROUPADMIN_GROUPNAME)
        anon.groups.append(anon_group)
        logged.groups.append(logged_group)
        self.session.add(anon)
        self.session.add(logged)
        self.session.add(korppi_group)
        self.session.add(admin_group)
        self.session.add(teachers_group)
        self.session.add(groupadmin_group)

    def create_anonymous_user(self, name: str, real_name: str, commit: bool = True) -> User:
        """Creates a new anonymous user.

        :param name: The name of the user to be created.
        :param real_name: The real name of the user.
        :returns: The id of the newly created user.

        """

        next_id = User.query.with_entities(func.min(User.id)).scalar() - 1
        u, _ = User.create_with_group(uid=next_id, name=name + str(abs(next_id)), real_name=real_name)
        self.session.add(u)
        if commit:
            self.session.commit()
        return u

    def get_rights_holders(self, block_id: int):
        cursor = self.db.cursor()
        cursor.execute("""SELECT b.UserGroup_id as gid,
                                 u.name as name,
                                 a.id as access_type,
                                 a.name as access_name,
                                 accessible_from,
                                 accessible_to,
                                 duration,
                                 duration_from,
                                 duration_to,
                                 fullname
                          FROM BlockAccess b
                          JOIN UserGroup u ON b.UserGroup_id = u.id
                          JOIN AccessType a ON b.type = a.id
                          LEFT JOIN (SELECT ug.id as gid, ua.real_name as fullname
                                     FROM useraccount ua
                                     JOIN usergroup ug on ug.name = ua.name
                                     ) tmp ON tmp.gid = b.UserGroup_id
                          WHERE Block_id = %s""", [block_id])
        return result_as_dict_list(cursor)

    def get_default_rights_holders(self, folder_id: int, object_type: BlockType):
        doc = get_default_right_document(folder_id, object_type)
        if doc is None:
            return []
        return self.get_rights_holders(doc.id)

    def remove_default_access(self, group_id: int, folder_id: int, access_type: str, object_type: BlockType):
        doc = get_default_right_document(folder_id, object_type, create_if_not_exist=True)
        self.remove_access(group_id, doc.id, access_type)

    def remove_access(self, group_id: int, block_id: int, access_type: str):
        access_type_id = get_access_type_id(access_type)
        i = Block.query.get(block_id)
        for a in i.accesses:  # type: BlockAccess
            if a.usergroup_id == group_id and a.type == access_type_id:
                db.session.delete(a)
                break
