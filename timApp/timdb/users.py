from typing import Optional, List, Set

from timdb.tim_models import User, UserGroup
from timdb.timdbbase import TimDbBase, TimDbException

import hashlib
import re

ANONYMOUS_USERNAME = "Anonymous"
ANONYMOUS_GROUPNAME = "Anonymous users"
KORPPI_GROUPNAME = "Korppi users"
LOGGED_IN_GROUPNAME = "Logged-in users"
LOGGED_IN_USERNAME = "Logged-in user"
ADMIN_GROUPNAME = "Administrators"

SPECIAL_GROUPS = {ANONYMOUS_GROUPNAME, KORPPI_GROUPNAME, LOGGED_IN_GROUPNAME, ADMIN_GROUPNAME}

# These will be cached in memory to reduce amount of db load
ANON_USER_ID = None
LOGGED_USER_ID = None
ANON_GROUP_ID = None
LOGGED_GROUP_ID = None
ADMIN_GROUP_ID = None


class NoSuchUserException(TimDbException):
    def __init__(self, user_id):
        super().__init__('No such user: {}'.format(user_id))
        self.user_id = user_id


class Users(TimDbBase):
    """Handles saving and retrieving user-related information to/from the database."""

    access_type_map = {}

    def create_special_usergroups(self) -> int:
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
        cursor.execute('INSERT INTO UserAccount (id, name) VALUES (%s, %s)', [ANONYMOUS_USERID, ANONYMOUS_USERNAME])
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
        return 0

    def create_user(self, name: str, real_name: str, email: str, password: str = '',
                    commit: bool = True) -> int:
        """Creates a new user with the specified name.
        
        :param email: The email address of the user.
        :param name: The name of the user to be created.
        :param real_name: The real name of the user.
        :param password: The password for the user (not used on Korppi login).
        :returns: The id of the newly created user.
        """

        hash = self.hash_password(password) if password != '' else ''
        user = User(name=name, real_name=real_name, email=email, pass_=hash)
        self.session.add(user)
        self.session.flush()
        if commit:
            self.session.commit()
        user_id = user.id
        assert user_id != 0
        return user_id

    def create_anonymous_user(self, name: str, real_name: str, commit: bool = True) -> int:
        """Creates a new user anonymous user.
        :param name: The name of the user to be created.
        :param real_name: The real name of the user.
        :returns: The id of the newly created user.
        """

        next_id = self.get_next_anonymous_user_id()
        u = User(id=next_id, name=name, real_name=real_name)
        self.session.add(u)
        self.session.flush()
        if commit:
            self.session.commit()
        user_id = u.id
        self.add_user_to_group(self.get_anon_group_id(), user_id)
        return user_id

    def get_next_anonymous_user_id(self) -> int:
        """
        :returns: The next unused negative id.
        """
        cursor = self.db.cursor()
        cursor.execute('SELECT MIN(id) AS next_id FROM UserAccount')
        user_id = min(self.resultAsDictionary(cursor)[0]['next_id'], 0)
        return user_id - 1

    def update_user(self, user_id: int, name: str, real_name: str, email: str, password: str = '',
                    commit: bool = True):
        """Updates user information.

        :param user_id: The id of the user to be updated.
        :param name: The username of the user.
        :param real_name: The real name of the user.
        :param email: The email of the user.
        :param password: The password of the user.
        """

        cursor = self.db.cursor()
        pass_hash = self.hash_password(password) if password != '' else ''
        cursor.execute('UPDATE UserAccount SET name = %s, real_name = %s, email = %s, pass = %s WHERE id = %s',
                       [name, real_name, email, pass_hash, user_id])
        if commit:
            self.db.commit()

    def update_user_field(self, user_id: int, field_name: str, field_value: str, commit: bool = True):
        cursor = self.db.cursor()
        if field_name == 'pass':
            field_value = self.hash_password(field_value)

        # No sql injection or other funny business
        if re.match('^[a-zA-Z_-]+$', field_name) is None:
            raise TimDbException('update_user_field: passed field name ' + field_name)

        cursor.execute('UPDATE UserAccount SET {} = %s WHERE id = %s'.format(field_name), [field_value, user_id])
        if commit:
            self.db.commit()


    def create_usergroup(self, name: str, commit: bool = True) -> int:
        """Creates a new user group.
        
        :param name: The name of the user group.
        :returns: The id of the created user group.
        """

        ug = UserGroup(name=name)
        self.session.add(ug)
        self.session.flush()
        group_id = ug.id
        assert group_id is not None and group_id != 0, 'group_id was None'
        if commit:
            self.session.commit()
        return group_id

    def add_user_to_group(self, group_id: int, user_id: int, commit: bool = True):
        """Adds a user to a usergroup.
        
        :param group_id: The id of the group.
        :param user_id: The id of the user.
        """
        cursor = self.db.cursor()
        cursor.execute('INSERT INTO UserGroupMember (UserGroup_id, User_id) VALUES (%s, %s)', [group_id, user_id])
        if commit:
            self.db.commit()

    def add_user_to_named_group(self, group_name: str, user_id: int, commit: bool = True):
        group_id = self.get_usergroup_by_name(group_name)
        if group_id is not None:
            self.add_user_to_group(group_id, user_id, commit)
        else:
            print("Could not add user {} to group {}".format(user_id, group_name))

    def add_user_to_korppi_group(self, user_id: int, commit: bool = True):
        self.add_user_to_named_group(KORPPI_GROUPNAME, user_id, commit)

    def addUserToAdmins(self, user_id: int, commit: bool = True):
        self.add_user_to_named_group(ADMIN_GROUPNAME, user_id, commit)

    def create_potential_user(self, email: str, password: str, commit: bool = True):
        """Creates a potential user with the specified email and password.
        
        :param email: The email address of the user.
        :param password: The password of the user.
        """
        cursor = self.db.cursor()
        hash = self.hash_password(password)
        cursor.execute('INSERT INTO NewUser (email, pass, created) VALUES (%s, %s, CURRENT_TIMESTAMP) ON CONFLICT UPDATE', [email, hash])
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
        hash = self.hash_password(password)
        cursor.execute('SELECT email FROM NewUser WHERE email=%s AND pass=%s', [email, hash])
        return cursor.fetchone() is not None

    def test_user(self, email: str, password: str) -> bool:
        """Tests if a potential user matches to email and password.
        
        :param email: Email address.
        :param password: Password.
        :returns: Boolean.
        """

        cursor = self.db.cursor()
        hash = self.hash_password(password)
        cursor.execute('SELECT email FROM UserAccount WHERE email=%s AND pass=%s', [email, hash])
        return cursor.fetchone() is not None

    def hash_password(self, password: str) -> str:
        return hashlib.sha256(password.encode()).hexdigest()

    def get_rights_holders(self, block_id: int):
        cursor = self.db.cursor()
        cursor.execute("""SELECT b.UserGroup_id as gid, u.name as name, a.id as access_type, a.name as access_name FROM BlockAccess b
                          JOIN UserGroup u ON b.UserGroup_id = u.id
                          JOIN AccessType a ON b.type = a.id
                          WHERE Block_id = %s""", [block_id])
        return self.resultAsDictionary(cursor)

    def get_owner_group(self, block_id: int):
        """Returns the owner group of the specified block.
        
        :param block_id: The id of the block.
        """

        cursor = self.db.cursor()
        cursor.execute('SELECT id, name FROM UserGroup WHERE id IN (SELECT UserGroup_id FROM Block WHERE id = %s)',
                       [block_id])
        return self.resultAsDictionary(cursor)[0]

    def get_user(self, user_id: int) -> Optional[dict]:
        """Gets the user with the specified id.
        
        :param user_id:
        :returns: An sqlite3 row object representing the user. Columns: id, name.
        """

        cursor = self.db.cursor()
        cursor.execute('SELECT * FROM UserAccount WHERE id = %s', [user_id])
        result = self.resultAsDictionary(cursor)
        return result[0] if len(result) > 0 else None

    def get_user_id_by_name(self, name: str) -> Optional[int]:
        """Gets the id of the specified username.
        
        :param name: The name of the user.
        :returns: The id of the user or None if the user does not exist.
        """

        cursor = self.db.cursor()
        cursor.execute('SELECT id FROM UserAccount WHERE id >= 0 AND name = %s', [name])
        result = cursor.fetchone()
        return result[0] if result is not None else None

    def get_user_by_name(self, name: str) -> User:
        """Gets the user information of the specified username.

        :param name: The name of the user.
        :returns: The user information or None if the user does not exist.
        """
        return self.session.query(User).filter(User.name==name).one()

    def get_user_by_email(self, email: str) -> Optional[dict]:
        """Gets the data of the specified user email address.
        
        :param email: Email address.
        :returns: The user data.
        """

        cursor = self.db.cursor()
        cursor.execute('SELECT * FROM UserAccount WHERE email = %s', [email])
        result = self.resultAsDictionary(cursor)
        return result[0] if len(result) > 0 else None

    def group_exists(self, group_name: str) -> bool:
        """Checks if the group with the specified name exists

        :param group_name: The name of the group.
        :returns: Boolean.
        """

        cursor = self.db.cursor()
        cursor.execute('SELECT id FROM UserGroup WHERE name = %s', [group_name])
        return cursor.fetchone() is not None

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

    def get_usergroup_by_name(self, name: str) -> Optional[int]:
        cursor = self.db.cursor()
        cursor.execute('SELECT id FROM UserGroup WHERE name = %s', [name])
        groups = cursor.fetchall()

        if groups is None or len(groups) == 0:
            print("DEBUG: No such named group: " + name)
            return None
        elif len(groups) > 1:
            print("DEBUG: Too many named groups: {} ({})".format(name, groups))
            return None

        return groups[0][0]

    def get_personal_usergroup(self, user: dict) -> int:
        """Gets the personal user group for the user.

        :param user: The user object.
        """
        if user is None:
            raise TimDbException("No such user")

        # For anonymous users, we return the group of anonymous users
        if user['id'] < 0 or user['id'] == self.get_anon_user_id():
            return self.get_anon_group_id()

        userName = user['name']
        groups = self.get_usergroups_by_name(userName)
        if len(groups) > 0:
            return groups[0]['id']

        groups = self.get_usergroups_by_name('group of user ' + userName)
        if len(groups) > 0:
            return groups[0]['id']

        raise TimDbException('Personal usergroup for user {} was not found!'.format(userName))

    def get_user_groups(self, user_id: int) -> List[dict]:
        """Gets the user groups of a user.
        
        :param user_id: The id of the user.
        :returns: The user groups that the user belongs to.
        """

        cursor = self.db.cursor()
        if self.has_admin_access(user_id):
            # Admin is part of every user group
            cursor.execute("""SELECT id, name FROM UserGroup ORDER BY id ASC""")
        else:
            cursor.execute("""SELECT id, name FROM UserGroup WHERE id IN
                              (SELECT UserGroup_id FROM UserGroupMember WHERE User_id = %s)
                              ORDER BY id ASC""", [user_id])

        return self.resultAsDictionary(cursor)

    def get_usergroups_printable(self, user_id: int, max_group_len: int = 32) -> List[dict]:
        """Gets the user groups of a user, truncating the group names.

        :param user_id: The id of the user.
        :returns: The user groups that the user belongs to.
        """
        groups = self.get_user_groups(user_id)
        for group in groups:
            if len(group['name']) > max_group_len:
                group['name'] = group['name'][:max_group_len]
        return groups

    def get_users_in_group(self, group_id: int, limit:int=1000) -> List[dict]:
        cursor = self.db.cursor()
        cursor.execute("""SELECT UserGroupMember.User_id as id, UserAccount.real_name as name, UserAccount.email as email
                          FROM UserGroupMember
                          INNER JOIN UserAccount ON UserGroupMember.User_id=UserAccount.id
                          WHERE UserGroupMember.UserGroup_id=%s
                          LIMIT %s
                       """, [group_id, limit])
        return self.resultAsDictionary(cursor)

    def get_group_users(self, group_id: int) -> List[dict]:
        cursor = self.db.cursor()
        cursor.execute("""SELECT User_id as id, User_name FROM UserGroupMember WHERE
                          User_name = (SELECT name FROM UserAccount WHERE id = %s) AND
                          User_id   = (SELECT id FROM UserGroup WHERE UserGroup_id = %s)
                       """)

        return len(cursor.fetchall()) > 0

    def is_user_id_in_group(self, user_id: int, usergroup_name: str) -> bool:
        cursor = self.db.cursor()
        cursor.execute("""SELECT User_id FROM UserGroupMember WHERE
                          User_id      = %s AND
                          UserGroup_id = (SELECT id FROM UserGroup WHERE name = %s)
                       """, [user_id, usergroup_name])
        return len(cursor.fetchall()) > 0

    def is_user_id_in_group_id(self, user_id: int, usergroup_id: int) -> bool:
        c = self.db.cursor()
        c.execute("""SELECT User_id FROM UserGroupMember
                           WHERE User_id = %s AND UserGroup_id = %s""", (user_id, usergroup_id))
        return c.fetchone() is not None

    def grant_access(self, group_id: int, block_id: int, access_type: str):
        """Grants access to a group for a block.
        
        :param group_id: The group id to which to grant view access.
        :param block_id: The id of the block for which to grant view access.
        :param access_type: The kind of access. Possible values are 'edit' and 'view'.
        """

        # TODO: Check that the group_id and block_id exist.
        access_id = self.get_access_type_id(access_type)
        cursor = self.db.cursor()
        if access_id is not None:
            cursor.execute("""INSERT INTO BlockAccess (Block_id,UserGroup_id,accessible_from,type)
                              VALUES (%s,%s,CURRENT_TIMESTAMP, %s)""", [block_id, group_id, access_id])
        self.db.commit()

    def grant_view_access(self, group_id: int, block_id: int):
        """Grants view access to a group for a block.
        
        :param group_id: The group id to which to grant view access.
        :param block_id: The id of the block for which to grant view access.
        """

        self.grant_access(group_id, block_id, 'view')

    def grant_edit_access(self, group_id: int, block_id: int):
        """Grants edit access to a group for a block.
        
        :param group_id: The group id to which to grant view access.
        :param block_id: The id of the block for which to grant view access.
        """

        self.grant_access(group_id, block_id, 'edit')

    def get_view_access_id(self) -> int:
        return self.get_access_type_id('view')

    def remove_access(self, group_id: int, block_id: int, access_type: str):
        cursor = self.db.cursor()
        cursor.execute("DELETE FROM BlockAccess WHERE UserGroup_id = %s AND Block_id = %s AND type = %s",
                       [group_id, block_id, self.get_access_type_id(access_type)])
        self.db.commit()

    def get_edit_access_id(self) -> int:
        return self.get_access_type_id('edit')

    def get_teacher_access_id(self) -> int:
        return self.get_access_type_id('teacher')

    def get_manage_access_id(self) -> int:
        return self.get_access_type_id('manage')

    def get_seeanswers_access_id(self) -> int:
        return self.get_access_type_id('see answers')

    def has_admin_access(self, user_id: int) -> bool:
        return self.is_user_id_in_group_id(user_id, self.get_admin_group_id())

    def has_view_access(self, user_id: int, block_id: int) -> bool:
        """Returns whether the user has view access to the specified block.

        :param user_id: The user id to check.
        :param block_id: The block id to check.
        :returns: True if the user with id 'user_id' has view access to the block 'block_id', false otherwise.
        """
        return self.has_access(user_id,
                               block_id,
                               self.get_view_access_id(),
                               self.get_edit_access_id(),
                               self.get_manage_access_id(),
                               self.get_teacher_access_id(),
                               self.get_seeanswers_access_id())

    def has_teacher_access(self, user_id: int, block_id: int) -> bool:
        """Returns whether the user has teacher access to the specified block.

        :param user_id: The user id to check.
        :param block_id: The block id to check.
        :returns: True if the user with id 'user_id' has teacher access to the block 'block_id', false otherwise.
        """
        return self.has_access(user_id, block_id, self.get_manage_access_id(), self.get_teacher_access_id())

    def has_manage_access(self, user_id: int, block_id: int) -> bool:
        """Returns whether the user has manage access to the specified block.

        :param user_id: The user id to check.
        :param block_id: The block id to check.
        :returns: True if the user with id 'user_id' has manage access to the block 'block_id', false otherwise.
        """
        return self.has_access(user_id, block_id, self.get_manage_access_id())

    def has_access(self, user_id: int, block_id: int, *access_ids) -> bool:
        """Returns whether the user has any of the specific kind of access types to the specified block.
        
        :param user_id: The user id to check.
        :param block_id: The block id to check.
        :param access_ids: List of access type ids to be checked.
        :returns: True if the user with id 'user_id' has a specific kind of access to the block 'block_id',
                  false otherwise.
        """

        if self.has_admin_access(user_id):
            return True

        user_ids = [user_id, self.get_anon_user_id()]
        if user_id > 0:
            user_ids.append(self.get_logged_user_id())
        whole_sql = """
({}) INTERSECT
SELECT User_id
FROM UserGroupMember
WHERE UserGroup_id IN
      (SELECT UserGroup_id
       FROM BlockAccess
       WHERE Block_id = %s AND type IN ({})
       UNION SELECT UserGroup_id
             FROM Block
             WHERE id = %s
      )
""".format(' UNION '.join('SELECT ' + str(x) for x in user_ids), ','.join(['%s'] * len(access_ids)))
        c = self.db.cursor()
        # print(whole_sql)
        c.execute(whole_sql, [block_id] + list(access_ids) + [block_id])
        result = c.fetchone()
        return result is not None

    def get_accessible_blocks(self, user_id: int, access_types: List[int]) -> Set[int]:
        if self.has_admin_access(user_id):
            c = self.db.cursor()
            c.execute("""SELECT id FROM Block""")
            return set(row[0] for row in c.fetchall())
        user_ids = [user_id, self.get_anon_user_id()]
        if user_id > 0:
            user_ids.append(self.get_logged_user_id())
        c = self.db.cursor()
        c.execute("""
SELECT Block_id as id FROM BlockAccess
WHERE UserGroup_id IN (SELECT UserGroup_id
FROM UserGroupMember
WHERE User_id IN ({}))
  AND type IN ({})
UNION
SELECT id FROM Block
WHERE UserGroup_id IN (SELECT UserGroup_id
FROM UserGroupMember
WHERE User_id IN ({}))
        """.format(self.get_sql_template(user_ids),
                   self.get_sql_template(access_types),
                   self.get_sql_template(user_ids)), user_ids + access_types + user_ids)
        return set(row[0] for row in c.fetchall())

    def get_viewable_blocks(self, user_id: int) -> Set[int]:
        return self.get_accessible_blocks(user_id, [self.get_view_access_id(),
                                                    self.get_edit_access_id(),
                                                    self.get_manage_access_id(),
                                                    self.get_teacher_access_id(),
                                                    self.get_seeanswers_access_id()])

    def has_edit_access(self, user_id: int, block_id: int) -> bool:
        """Returns whether the user has edit access to the specified block.
        
        :param user_id:
        :param block_id:
        :returns: True if the user with id 'user_id' has edit access to the block 'block_id', false otherwise.
        """

        return self.has_access(user_id, block_id, self.get_edit_access_id(), self.get_manage_access_id())

    def has_seeanswers_access(self, uid: int, block_id: int) -> bool:
        return self.has_access(uid, block_id,
                               self.get_seeanswers_access_id(),
                               self.get_manage_access_id(),
                               self.get_teacher_access_id())

    def user_is_owner(self, user_id: int, block_id: int) -> bool:
        """Returns whether the user belongs to the owners of the specified block.
        
        :param user_id:
        :param block_id:
        :returns: True if the user with 'user_id' belongs to the owner group of the block 'block_id'.
        """
        if self.has_admin_access(user_id):
            return True

        cursor = self.db.cursor()
        cursor.execute("""SELECT id FROM UserAccount WHERE
                          id = %s
                          AND (id IN
                              (SELECT User_id FROM UserGroupMember WHERE UserGroup_id IN
                              (SELECT UserGroup_id FROM Block WHERE id = %s))
                              )""", [user_id, block_id])
        result = cursor.fetchall()
        assert len(result) <= 1, 'rowcount should be 1 at most'
        return len(result) == 1

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
        c.execute("""SELECT UserAccount.id, UserAccount.name, real_name, email
            FROM UserAccount
            JOIN UserGroupMember ON UserAccount.id = UserGroupMember.User_id
            JOIN UserGroup ON UserGroup.id = UserGroupMember.UserGroup_id
            WHERE UserGroup.name = %s{}""".format(order_sql), [usergroup_name])
        return self.resultAsDictionary(c)

    def get_access_type_id(self, access_type):
        if not self.access_type_map:
            c = self.db.cursor()
            c.execute("""SELECT id, name FROM AccessType""")
            result = c.fetchall()
            for row in result:
                self.access_type_map[row[1]] = row[0]
        return self.access_type_map[access_type]

    def get_access_types(self):
        c = self.db.cursor()
        c.execute("""SELECT id, name FROM AccessType""")
        return self.resultAsDictionary(c)

    def remove_membership(self, uid: int, gid: int, commit: bool=True) -> int:
        """Removes membership of a user from a group.
        :param uid: The user id.
        :param gid: The group id.
        :returns: The number of affected rows (0 or 1).
        """
        c = self.db.cursor()
        c.execute("""DELETE FROM UserGroupMember WHERE User_id = %s and UserGroup_id = %s""", [uid, gid])
        if commit:
            self.db.commit()
        return c.rowcount

    def create_user_with_group(self, name: str,
                               real_name: Optional[str]=None,
                               email: Optional[str]=None,
                               password: Optional[str]=None,
                               is_admin: bool=False):
        user_id = self.create_user(name, real_name or name, email or name + '@example.com', password=password or '')
        user_group = self.create_usergroup(name)
        self.add_user_to_group(user_group, user_id)
        if is_admin:
            self.addUserToAdmins(user_id)
        return user_id, user_group

    def get_personal_usergroup_by_id(self, user_id: int) -> Optional[int]:
        return self.get_personal_usergroup(self.get_user(user_id))

    def get_anon_user_id(self) -> int:
        global ANON_USER_ID
        if ANON_USER_ID is not None:
            return ANON_USER_ID
        ANON_USER_ID = self.get_user_id_by_name(ANONYMOUS_USERNAME)
        return ANON_USER_ID

    def get_logged_user_id(self) -> int:
        global LOGGED_USER_ID
        if LOGGED_USER_ID is not None:
            return LOGGED_USER_ID
        LOGGED_USER_ID = self.get_user_id_by_name(LOGGED_IN_USERNAME)
        return LOGGED_USER_ID

    def get_anon_group_id(self) -> int:
        global ANON_GROUP_ID
        if ANON_GROUP_ID is not None:
            return ANON_GROUP_ID
        ANON_GROUP_ID = self.get_usergroup_by_name(ANONYMOUS_GROUPNAME)
        return ANON_GROUP_ID

    def get_logged_group_id(self) -> int:
        global LOGGED_GROUP_ID
        if LOGGED_GROUP_ID is not None:
            return LOGGED_GROUP_ID
        LOGGED_GROUP_ID = self.get_usergroup_by_name(LOGGED_IN_GROUPNAME)
        return LOGGED_GROUP_ID

    def get_admin_group_id(self) -> int:
        global ADMIN_GROUP_ID
        if ADMIN_GROUP_ID is not None:
            return ADMIN_GROUP_ID
        ADMIN_GROUP_ID = self.get_usergroup_by_name(ADMIN_GROUPNAME)
        return ADMIN_GROUP_ID

    def set_usergroup_name(self, group_id: int, user_name: str):
        c = self.db.cursor()
        c.execute("""UPDATE UserGroup SET name = %s WHERE id = %s""", (user_name, group_id))
        self.db.commit()
