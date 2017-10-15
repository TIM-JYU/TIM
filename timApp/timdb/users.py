from typing import Optional, List

from timApp.timdb.blocktypes import BlockType
from timApp.timdb.models.block import Block
from timApp.timdb.models.user import User
from timApp.timdb.models.usergroup import UserGroup
from timApp.timdb.special_group_names import ANONYMOUS_USERNAME, ANONYMOUS_GROUPNAME, KORPPI_GROUPNAME, LOGGED_IN_GROUPNAME, \
    LOGGED_IN_USERNAME, ADMIN_GROUPNAME
from timApp.timdb.timdbbase import TimDbBase
from timApp.timdb.timdbexception import TimDbException
from timApp.timdb.userutils import NoSuchUserException, get_anon_group_id, \
    get_anon_user_id, get_access_type_id, get_default_right_document, hash_password


class Users(TimDbBase):
    """Handles saving and retrieving user-related information to/from the database."""

    def create_special_usergroups(self):
        """Creates an anonymous user and a usergroup for it.

        The user id and its associated usergroup id is 0.

        """

        # Please keep these local and refer to the groups with their names instead.
        # They may differ depending on the script version they were created with.
        ANONYMOUS_USERID = 0
        LOGGED_IN_USERID = 1
        LOGGED_IN_GROUPID = 0
        ANONYMOUS_GROUPID = 2
        KORPPI_GROUPID = 3
        ADMIN_GROUPID = 4

        cursor = self.db.cursor()
        cursor.execute('INSERT INTO UserAccount (id, name, real_name) VALUES (%s, %s, %s)',
                       [ANONYMOUS_USERID, ANONYMOUS_USERNAME, 'Anonymous user'])
        cursor.execute('INSERT INTO UserAccount (id, name) VALUES (%s, %s)', [LOGGED_IN_USERID, LOGGED_IN_USERNAME])
        cursor.execute('INSERT INTO UserGroup (id, name) VALUES (%s, %s)', [ANONYMOUS_GROUPID, ANONYMOUS_GROUPNAME])
        cursor.execute('INSERT INTO UserGroup (id, name) VALUES (%s, %s)', [LOGGED_IN_GROUPID, LOGGED_IN_GROUPNAME])
        cursor.execute('INSERT INTO UserGroup (id, name) VALUES (%s, %s)', [KORPPI_GROUPID, KORPPI_GROUPNAME])
        cursor.execute('INSERT INTO UserGroup (id, name) VALUES (%s, %s)', [ADMIN_GROUPID, ADMIN_GROUPNAME])
        cursor.execute('INSERT INTO UserGroupMember (User_id, UserGroup_id) VALUES (%s, %s)',
                       [ANONYMOUS_USERID, ANONYMOUS_GROUPID])
        cursor.execute('INSERT INTO UserGroupMember (User_id, UserGroup_id) VALUES (%s, %s)',
                       [LOGGED_IN_USERID, LOGGED_IN_GROUPID])
        cursor.execute('SELECT MAX(id) FROM UserAccount')
        max_ua_id = cursor.fetchone()[0]
        cursor.execute('SELECT MAX(id) FROM UserGroup')
        max_ug_id = cursor.fetchone()[0]
        cursor.execute("SELECT setval('useraccount_id_seq', %s)", (max_ua_id,))
        cursor.execute("SELECT setval('usergroup_id_seq', %s)", (max_ug_id,))

        self.db.commit()

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
        user_id = min(self.resultAsDictionary(cursor)[0]['next_id'], 0)
        return user_id - 1

    def create_potential_user(self, email: str, password: str, commit: bool = True):
        """Creates a potential user with the specified email and password.

        :param email: The email address of the user.
        :param password: The password of the user.

        """
        cursor = self.db.cursor()
        hash = hash_password(password)
        cursor.execute(
            'INSERT INTO NewUser (email, pass, created) VALUES (%s, %s, CURRENT_TIMESTAMP) ON CONFLICT (email) DO UPDATE SET pass = EXCLUDED.pass',
            [email, hash])
        if commit:
            self.db.commit()

    def delete_potential_user(self, email: str, commit: bool = True):
        """Deletes a potential user.

        :param email: The email address of the user.

        """
        cursor = self.db.cursor()
        cursor.execute('DELETE FROM NewUser WHERE email=%s', [email])
        if commit:
            self.db.commit()

    def test_potential_user(self, email: str, password: str) -> bool:
        """Tests if a potential user matches to email and password.

        :param email: Email address.
        :param password: Password.
        :returns: Boolean.

        """

        cursor = self.db.cursor()
        hash = hash_password(password)
        cursor.execute('SELECT email FROM NewUser WHERE email=%s AND pass=%s', [email, hash])
        return cursor.fetchone() is not None

    def test_user(self, email: str, password: str) -> bool:
        """Tests if a potential user matches to email and password.

        :param email: Email address.
        :param password: Password.
        :returns: Boolean.

        """

        cursor = self.db.cursor()
        hash = hash_password(password)
        cursor.execute('SELECT email FROM UserAccount WHERE email=%s AND pass=%s', [email, hash])
        return cursor.fetchone() is not None

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
        return self.resultAsDictionary(cursor)

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
        result = self.resultAsDictionary(cursor)
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
        return self.resultAsDictionary(cursor)

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
        return self.resultAsDictionary(cursor)

    def is_user_id_in_group(self, user_id: int, usergroup_name: str) -> bool:
        cursor = self.db.cursor()
        cursor.execute("""SELECT User_id FROM UserGroupMember WHERE
                          User_id      = %s AND
                          UserGroup_id = (SELECT id FROM UserGroup WHERE name = %s)
                       """, [user_id, usergroup_name])
        return len(cursor.fetchall()) > 0

    def remove_access(self, group_id: int, block_id: int, access_type: str):
        cursor = self.db.cursor()
        cursor.execute("DELETE FROM BlockAccess WHERE UserGroup_id = %s AND Block_id = %s AND type = %s",
                       [group_id, block_id, get_access_type_id(access_type)])
        self.db.commit()

    def get_preferences(self, user_id: int) -> str:
        """Gets the preferences of a user.

        :param user_id: The id of the user.
        :returns: The user preferences as a string.

        """
        cursor = self.db.cursor()
        cursor.execute("""SELECT prefs FROM UserAccount WHERE id = %s""", [user_id])
        result = self.resultAsDictionary(cursor)
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
        return self.resultAsDictionary(c)

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

    def get_users_groups(self, username, order=False):
        c = self.db.cursor()
        order_sql = ' ORDER BY gr.name' if order else ''
        c.execute(f"""SELECT gr.name
            FROM UserAccount AS ua, usergroupmember As ug, usergroup AS gr
            WHERE ua.id=ug.user_id  and ug.usergroup_id=gr.id and ua.name=%s{order_sql}
            """, [username])
        return self.resultAsList(c)

    def get_personal_usergroup_by_id(self, user_id: int) -> Optional[int]:
        return self.get_personal_usergroup(self.get_user(user_id))
