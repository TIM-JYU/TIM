from typing import Optional, List

from timApp.timdb.blocktypes import BlockType
from timApp.timdb.exceptions import TimDbException
from timApp.timdb.models.block import Block
from timApp.timdb.models.user import User
from timApp.timdb.models.usergroup import UserGroup
from timApp.timdb.special_group_names import ANONYMOUS_USERNAME, ANONYMOUS_GROUPNAME, KORPPI_GROUPNAME, \
    LOGGED_IN_GROUPNAME, \
    LOGGED_IN_USERNAME, ADMIN_GROUPNAME
from timApp.timdb.tim_models import BlockAccess, db
from timApp.timdb.timdbbase import TimDbBase, result_as_dict_list
from timApp.timdb.userutils import NoSuchUserException, get_anon_group_id, \
    get_anon_user_id, get_access_type_id, get_default_right_document


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
        anon.groups.append(anon_group)
        logged.groups.append(logged_group)
        self.session.add(anon)
        self.session.add(logged)
        self.session.add(korppi_group)
        self.session.add(admin_group)

    def create_anonymous_user(self, name: str, real_name: str, commit: bool = True) -> User:
        """Creates a new anonymous user.

        :param name: The name of the user to be created.
        :param real_name: The real name of the user.
        :returns: The id of the newly created user.

        """

        next_id = self.get_next_anonymous_user_id()
        u = User(id=next_id, name=name + str(abs(next_id)), real_name=real_name)
        self.session.add(u)
        u.groups.append(UserGroup.get_anonymous_group())
        if commit:
            self.session.commit()
        return u

    def get_next_anonymous_user_id(self) -> int:
        """
        :returns: The next unused negative id.
        """
        cursor = self.db.cursor()
        cursor.execute('SELECT MIN(id) AS next_id FROM UserAccount')
        user_id = min(result_as_dict_list(cursor)[0]['next_id'], 0)
        return user_id - 1

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

    def get_owner_group(self, block_id: int) -> UserGroup:
        """Returns the owner group of the specified block.

        :param block_id: The id of the block.

        """
        return self.session.query(Block).filter_by(id=block_id).one().owner

    def get_user(self, user_id: int, include_authdata=False) -> Optional[dict]:
        """Gets the user with the specified id.

        :param include_authdata: Whether to include authentication-related fields in the result. Default False.
        :param user_id: The user id.
        :returns: A dict object representing the user. Columns: id, name.

        """
        authtemplate = ', yubikey, pass' if include_authdata else ''
        cursor = self.db.cursor()
        cursor.execute(f'SELECT id, name, real_name, email {authtemplate} FROM UserAccount WHERE id = %s', [user_id])
        result = result_as_dict_list(cursor)
        return result[0] if len(result) > 0 else None

    def get_user_group_name(self, group_id: int) -> Optional[str]:
        """Gets the user group name.

        :param group_id: The id of the group.
        :returns: The name of the group.

        """
        cursor = self.db.cursor()
        cursor.execute('SELECT name FROM UserGroup WHERE id = %s', [group_id])
        result = cursor.fetchone()
        return result[0] if result is not None else None

    def get_usergroups_by_name(self, name: str):
        """Gets the usergroups that have the specified name.

        :param name: The name of the usergroup to be retrieved.

        """

        cursor = self.db.cursor()
        cursor.execute('SELECT id FROM UserGroup WHERE name = %s', [name])
        return result_as_dict_list(cursor)

    def get_personal_usergroup(self, user: dict) -> int:
        """Gets the personal user group for the user.

        :param user: The user object.

        """
        if user is None:
            raise TimDbException("No such user")

        # For anonymous users, we return the group of anonymous users
        if user['id'] < 0 or user['id'] == get_anon_user_id():
            return get_anon_group_id()

        userName = user['name']
        groups = self.get_usergroups_by_name(userName)
        if len(groups) > 0:
            return groups[0]['id']

        groups = self.get_usergroups_by_name('group of user ' + userName)
        if len(groups) > 0:
            return groups[0]['id']

        raise TimDbException(f'Personal usergroup for user {userName} was not found!')

    def get_users_in_group(self, group_id: int, limit: int = 1000) -> List[dict]:
        cursor = self.db.cursor()
        cursor.execute("""SELECT UserGroupMember.User_id as id, UserAccount.real_name as name, UserAccount.email as email
                          FROM UserGroupMember
                          INNER JOIN UserAccount ON UserGroupMember.User_id=UserAccount.id
                          WHERE UserGroupMember.UserGroup_id=%s
                          LIMIT %s
                       """, [group_id, limit])
        return result_as_dict_list(cursor)

    def is_user_id_in_group(self, user_id: int, usergroup_name: str) -> bool:
        cursor = self.db.cursor()
        cursor.execute("""SELECT User_id FROM UserGroupMember WHERE
                          User_id      = %s AND
                          UserGroup_id = (SELECT id FROM UserGroup WHERE name = %s)
                       """, [user_id, usergroup_name])
        return len(cursor.fetchall()) > 0

    def remove_access(self, group_id: int, block_id: int, access_type: str):
        access_type_id = get_access_type_id(access_type)
        i = Block.query.get(block_id)
        for a in i.accesses:  # type: BlockAccess
            if a.usergroup_id == group_id and a.type == access_type_id:
                db.session.delete(a)
                break

    def get_preferences(self, user_id: int) -> str:
        """Gets the preferences of a user.

        :param user_id: The id of the user.
        :returns: The user preferences as a string.

        """
        cursor = self.db.cursor()
        cursor.execute("""SELECT prefs FROM UserAccount WHERE id = %s""", [user_id])
        result = result_as_dict_list(cursor)
        if not result:
            raise NoSuchUserException(user_id)
        return result[0]['prefs']

    def set_preferences(self, user_id: int, prefs: str):
        """Sets the preferences for a user.

        :param user_id: The id of the user.
        :param prefs: The preferences to set.

        """
        cursor = self.db.cursor()
        cursor.execute("""UPDATE UserAccount SET prefs = %s WHERE id = %s""", [prefs, user_id])
        self.db.commit()

    def get_users_for_group(self, usergroup_name, order=False):
        c = self.db.cursor()
        order_sql = ' ORDER BY UserAccount.name' if order else ''
        c.execute(f"""SELECT UserAccount.id, UserAccount.name, real_name, email
            FROM UserAccount
            JOIN UserGroupMember ON UserAccount.id = UserGroupMember.User_id
            JOIN UserGroup ON UserGroup.id = UserGroupMember.UserGroup_id
            WHERE UserGroup.name = %s{order_sql}""", [usergroup_name])
        return result_as_dict_list(c)

    def check_if_in_group(self, username, usergroup_name):
        c = self.db.cursor()
        c.execute("""SELECT ua.id, ua.name, ug.user_id, ug.usergroup_id, gr.id, gr.name
            FROM UserAccount AS ua, usergroupmember As ug, usergroup AS gr
            WHERE ua.id=ug.user_id  and ug.usergroup_id=gr.id and ua.name=%s and gr.name=%s
            """, [username, usergroup_name])
        return c.rowcount > 0

    def check_if_in_group_by_id(self, usernid, usergroup_name):
        c = self.db.cursor()
        c.execute("""SELECT ug.user_id, ug.usergroup_id, gr.id, gr.name
            FROM usergroupmember As ug, usergroup AS gr
            WHERE ug.user_id=%s  and ug.usergroup_id=gr.id  and gr.name=%s
            """, [usernid, usergroup_name])
        return c.rowcount > 0

    def get_personal_usergroup_by_id(self, user_id: int) -> Optional[int]:
        return self.get_personal_usergroup(self.get_user(user_id))
