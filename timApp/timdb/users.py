import json
from contracts import contract, new_contract
from timdb.timdbbase import TimDbBase, TimDbException

import hashlib
import sqlite3

new_contract('row', sqlite3.Row)

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


class Users(TimDbBase):
    """Handles saving and retrieving user-related information to/from the database."""

    access_type_map = {}

    @contract
    def create_special_usergroups(self) -> 'int':
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
        cursor.execute('INSERT INTO User (id, name) VALUES (?, ?)', [ANONYMOUS_USERID, ANONYMOUS_USERNAME])
        cursor.execute('INSERT INTO User (id, name) VALUES (?, ?)', [LOGGED_IN_USERID, LOGGED_IN_USERNAME])
        cursor.execute('INSERT INTO UserGroup (id, name) VALUES (?, ?)', [ANONYMOUS_GROUPID, ANONYMOUS_GROUPNAME])
        cursor.execute('INSERT INTO UserGroup (id, name) VALUES (?, ?)', [LOGGED_IN_GROUPID, LOGGED_IN_GROUPNAME])
        cursor.execute('INSERT INTO UserGroup (id, name) VALUES (?, ?)', [KORPPI_GROUPID, KORPPI_GROUPNAME])
        cursor.execute('INSERT INTO UserGroup (id, name) VALUES (?, ?)', [ADMIN_GROUPID, ADMIN_GROUPNAME])
        cursor.execute('INSERT INTO UserGroupMember (User_id, UserGroup_id) VALUES (?, ?)',
                       [ANONYMOUS_USERID, ANONYMOUS_GROUPID])
        cursor.execute('INSERT INTO UserGroupMember (User_id, UserGroup_id) VALUES (?, ?)',
                       [LOGGED_IN_USERID, LOGGED_IN_GROUPID])
        self.db.commit()
        return 0

    @contract
    def create_user(self, name: 'str', real_name: 'str', email: 'str', password: 'str' = '',
                    commit: 'bool' = True) -> 'int':
        """Creates a new user with the specified name.
        
        :param email: The email address of the user.
        :param name: The name of the user to be created.
        :param real_name: The real name of the user.
        :param password: The password for the user (not used on Korppi login).
        :returns: The id of the newly created user.
        """
        cursor = self.db.cursor()
        hash = self.hash_password(password) if password != '' else ''
        cursor.execute('INSERT INTO User (name, real_name, email, pass) VALUES (?, ?, ?, ?)',
                       [name, real_name, email, hash])
        if commit:
            self.db.commit()
        user_id = cursor.lastrowid
        return user_id

    def create_anonymous_user(self, name: 'str', real_name: 'str', commit: 'bool' = True) -> 'int':
        """Creates a new user anonymous user.
        :param name: The name of the user to be created.
        :param real_name: The real name of the user.
        :returns: The id of the newly created user.
        """

        next_id = self.get_next_anonymous_user_id()

        cursor = self.db.cursor()
        cursor.execute('INSERT INTO User (id, name, real_name) VALUES (?, ?, ?)', [next_id, name, real_name])
        if commit:
            self.db.commit()
        user_id = cursor.lastrowid
        self.add_user_to_group(2, user_id)
        return user_id

    @contract
    def get_next_anonymous_user_id(self) -> 'int':
        """
        :returns: Tnext unused negative id.
        """
        cursor = self.db.cursor()
        cursor.execute('SELECT MIN(id) AS next_id FROM User')
        user_id = min(self.resultAsDictionary(cursor)[0]['next_id'], 0)
        return user_id - 1

    def update_user(self, user_id: 'int', name: 'str', real_name: 'str', email: 'str', password: 'str' = '',
                    commit: 'bool' = True):
        """Updates user information.

        :param user_id: The id of the user to be updated.
        :param name: The username of the user.
        :param real_name: The real name of the user.
        :param email: The email of the user.
        :param password: The password of the user.
        """

        cursor = self.db.cursor()
        pass_hash = self.hash_password(password) if password != '' else ''
        cursor.execute('UPDATE User SET name = ?, real_name = ?, email = ?, pass = ? WHERE id = ?',
                       [name, real_name, email, pass_hash, user_id])
        if commit:
            self.db.commit()

    @contract
    def create_usergroup(self, name: 'str', commit: 'bool' = True) -> 'int':
        """Creates a new user group.
        
        :param name: The name of the user group.
        :returns: The id of the created user group.
        """
        cursor = self.db.cursor()
        cursor.execute('INSERT INTO UserGroup (name) VALUES (?)', [name])
        group_id = cursor.lastrowid
        assert group_id is not None, 'group_id was None'
        if commit:
            self.db.commit()
        return group_id

    @contract
    def add_user_to_group(self, group_id: 'int', user_id: 'int', commit: 'bool' = True):
        """Adds a user to a usergroup.
        
        :param group_id: The id of the group.
        :param user_id: The id of the user.
        """
        cursor = self.db.cursor()
        cursor.execute('INSERT INTO UserGroupMember (UserGroup_id, User_id) VALUES (?, ?)', [group_id, user_id])
        if commit:
            self.db.commit()

    @contract
    def add_user_to_named_group(self, group_name: 'str', user_id: 'int', commit: 'bool' = True):
        group_id = self.get_usergroup_by_name(group_name)
        if group_id is not None:
            self.add_user_to_group(group_id, user_id, commit)
        else:
            print("Could not add user {} to group {}".format(user_id, group_name))

    @contract
    def add_user_to_korppi_group(self, user_id: 'int', commit: 'bool' = True):
        self.add_user_to_named_group(KORPPI_GROUPNAME, user_id, commit)

    @contract
    def addUserToAdmins(self, user_id: 'int', commit: 'bool' = True):
        self.add_user_to_named_group(ADMIN_GROUPNAME, user_id, commit)

    @contract
    def create_potential_user(self, email: 'str', password: 'str', commit: 'bool' = True):
        """Creates a potential user with the specified email and password.
        
        :param email: The email address of the user.
        :param password: The password of the user.
        """
        cursor = self.db.cursor()
        hash = self.hash_password(password)
        cursor.execute('REPLACE INTO NewUser (email, pass) VALUES (?, ?)', [email, hash])
        if commit:
            self.db.commit()

    @contract
    def delete_potential_user(self, email: 'str', commit: 'bool' = True):
        """Deletes a potential user.
        
        :param email: The email address of the user.
        """
        cursor = self.db.cursor()
        cursor.execute('DELETE FROM NewUser WHERE email=?', [email])
        if commit:
            self.db.commit()

    @contract
    def test_potential_user(self, email: 'str', password: 'str') -> 'bool':
        """Tests if a potential user matches to email and password.
        
        :param email: Email address.
        :param password: Password.
        :returns: Boolean.
        """

        cursor = self.db.cursor()
        hash = self.hash_password(password)
        cursor.execute('SELECT email FROM NewUser WHERE email=? AND pass=?', [email, hash])
        return cursor.fetchone() is not None

    @contract
    def test_user(self, email: 'str', password: 'str') -> 'bool':
        """Tests if a potential user matches to email and password.
        
        :param email: Email address.
        :param password: Password.
        :returns: Boolean.
        """

        cursor = self.db.cursor()
        hash = self.hash_password(password)
        cursor.execute('SELECT email FROM User WHERE email=? AND pass=?', [email, hash])
        return cursor.fetchone() is not None

    @contract
    def hash_password(self, password: 'str') -> 'str':
        return hashlib.sha256(password.encode()).hexdigest()

    @contract
    def get_rights_holders(self, block_id: 'int'):
        cursor = self.db.cursor()
        cursor.execute("""SELECT b.UserGroup_id as gid, u.name as name, a.id as access_type, a.name as access_name FROM BlockAccess b
                          JOIN UserGroup u ON b.UserGroup_id = u.id
                          JOIN AccessType a ON b.type = a.id
                          WHERE Block_id = ?""", [block_id])
        return self.resultAsDictionary(cursor)

    @contract
    def get_owner_group(self, block_id: 'int'):
        """Returns the owner group of the specified block.
        
        :param block_id: The id of the block.
        """

        cursor = self.db.cursor()
        cursor.execute('SELECT id, name FROM UserGroup WHERE id IN (SELECT UserGroup_id FROM Block WHERE id = ?)',
                       [block_id])
        return self.resultAsDictionary(cursor)[0]

    @contract
    def get_user(self, user_id: 'int') -> 'dict|None':
        """Gets the user with the specified id.
        
        :param user_id:
        :returns: An sqlite3 row object representing the user. Columns: id, name.
        """

        cursor = self.db.cursor()
        cursor.execute('SELECT id, name, real_name, email FROM User WHERE id = ?', [user_id])
        result = self.resultAsDictionary(cursor)
        return result[0] if len(result) > 0 else None

    @contract
    def get_user_by_name(self, name: 'str') -> 'int|None':
        """Gets the id of the specified username.
        
        :param name: The name of the user.
        :returns: The id of the user or None if the user does not exist.
        """

        cursor = self.db.cursor()
        cursor.execute('SELECT id FROM User WHERE name = ?', [name])
        result = cursor.fetchone()
        return result[0] if result is not None else None

    @contract
    def get_user_by_email(self, email: 'str') -> 'dict|None':
        """Gets the data of the specified user email address.
        
        :param name: Email address.
        :returns: The user data.
        """

        cursor = self.db.cursor()
        cursor.execute('SELECT * FROM User WHERE email = ?', [email])
        result = self.resultAsDictionary(cursor)
        return result[0] if len(result) > 0 else None

    @contract
    def group_exists(self, group_name: 'str') -> 'bool':
        """Checks if the group with the specified name exists

        :param group_name: The name of the group.
        :returns: Boolean.
        """

        cursor = self.db.cursor()
        cursor.execute('SELECT id FROM UserGroup WHERE name = ?', [group_name])
        return cursor.fetchone() is not None

    @contract
    def get_user_group_name(self, group_id: 'int') -> 'str|None':
        """Gets the user group name.
        :param group_id: The id of the group.
        :returns: The name of the group.
        """
        cursor = self.db.cursor()
        cursor.execute('SELECT name FROM UserGroup WHERE id = ?', [group_id])
        result = cursor.fetchone()
        return result[0] if result is not None else None

    @contract
    def get_usergroups_by_name(self, name: 'str'):
        """Gets the usergroups that have the specified name.
        
        :param name: The name of the usergroup to be retrieved.
        """

        cursor = self.db.cursor()
        cursor.execute('SELECT id FROM UserGroup WHERE name = ?', [name])
        return self.resultAsDictionary(cursor)

    @contract
    def get_usergroup_by_name(self, name: 'str') -> 'int|None':
        cursor = self.db.cursor()
        cursor.execute('SELECT id FROM UserGroup WHERE name = ?', [name])
        groups = cursor.fetchall()

        if groups is None or len(groups) == 0:
            print("DEBUG: No such named group: " + name)
            return None
        elif len(groups) > 1:
            print("DEBUG: Too many named groups: {} ({})".format(name, groups))
            return None

        return groups[0][0]

    @contract
    def get_personal_usergroup(self, user: 'dict') -> 'int':
        """Gets the personal user group for the user.
        """
        if user is None:
            raise TimDbException("No such user")

        userName = user['name']
        groups = self.get_usergroups_by_name(userName)
        if len(groups) > 0:
            return groups[0]['id']

        groups = self.get_usergroups_by_name('group of user ' + userName)
        if len(groups) > 0:
            return groups[0]['id']

        raise TimDbException('Personal usergroup for user {} was not found!'.format(userName))

    @contract
    def get_user_groups(self, user_id: 'int') -> 'list(dict)':
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
                              (SELECT UserGroup_id FROM UserGroupMember WHERE User_id = ?)
                              ORDER BY id ASC""", [user_id])

        return self.resultAsDictionary(cursor)

    @contract
    def get_usergroups_printable(self, user_id: 'int', max_group_len: 'int' = 32) -> 'list(dict)':
        """Gets the user groups of a user, truncating the group names.

        :param user_id: The id of the user.
        :returns: The user groups that the user belongs to.
        """
        groups = self.get_user_groups(user_id)
        for group in groups:
            if len(group['name']) > max_group_len:
                group['name'] = group['name'][:max_group_len]
        return groups

    @contract
    def get_users_in_group(self, group_id: 'int', limit:'int'=1000) -> 'list(dict)':
        cursor = self.db.cursor()
        cursor.execute("""SELECT UserGroupMember.User_id as id, User.real_name as name from UserGroupMember
                          INNER JOIN User ON UserGroupMember.User_id=User.id
                          WHERE UserGroupMember.UserGroup_id=?
                          LIMIT ?
                       """, [group_id, limit])
        return self.resultAsDictionary(cursor)

    @contract
    def get_group_users(self, group_id: 'int') -> 'list(dict)':
        cursor = self.db.cursor()
        cursor.execute("""SELECT User_id as id, User_name FROM UserGroupMember WHERE
                          User_name = (SELECT name FROM User WHERE id = ?) AND
                          User_id   = (SELECT id FROM UserGroup WHERE UserGroup_id = ?)
                       """)

        return len(cursor.fetchall()) > 0

    @contract
    def is_user_id_in_group(self, user_id: 'int', usergroup_name: 'str') -> 'bool':
        cursor = self.db.cursor()
        cursor.execute("""SELECT User_id FROM UserGroupMember WHERE
                          User_id      = ? AND
                          UserGroup_id = (SELECT id FROM UserGroup WHERE name = ?)
                       """, [user_id, usergroup_name])
        return len(cursor.fetchall()) > 0

    def grant_access(self, group_id: 'int', block_id: 'int', access_type: 'str'):
        """Grants access to a group for a block.
        
        :param group_id: The group id to which to grant view access.
        :param block_id: The id of the block for which to grant view access.
        :param access_type: The kind of access. Possible values are 'edit' and 'view'.
        """

        # TODO: Check that the group_id and block_id exist.
        access_id = self.get_access_type_id(access_type)
        cursor = self.db.cursor()
        if access_id is not None:
            cursor.execute("""INSERT OR IGNORE INTO BlockAccess (Block_id,UserGroup_id,accessible_from,type)
                              VALUES (?,?,CURRENT_TIMESTAMP, ?)""", [block_id, group_id, access_id])
        self.db.commit()

    @contract
    def grant_view_access(self, group_id: 'int', block_id: 'int'):
        """Grants view access to a group for a block.
        
        :param group_id: The group id to which to grant view access.
        :param block_id: The id of the block for which to grant view access.
        """

        self.grant_access(group_id, block_id, 'view')

    @contract
    def grant_edit_access(self, group_id: 'int', block_id: 'int'):
        """Grants edit access to a group for a block.
        
        :param group_id: The group id to which to grant view access.
        :param block_id: The id of the block for which to grant view access.
        """

        self.grant_access(group_id, block_id, 'edit')

    @contract
    def get_view_access_id(self) -> 'int':
        return self.get_access_type_id('view')

    @contract
    def remove_access(self, group_id: 'int', block_id: 'int', access_type: 'str'):
        cursor = self.db.cursor()
        cursor.execute("DELETE FROM BlockAccess WHERE UserGroup_id = ? AND Block_id = ? AND type = ?",
                       [group_id, block_id, self.get_access_type_id(access_type)])
        self.db.commit()

    @contract
    def get_edit_access_id(self) -> 'int':
        return self.get_access_type_id('edit')

    @contract
    def get_teacher_access_id(self) -> 'int':
        return self.get_access_type_id('teacher')

    @contract
    def get_manage_access_id(self) -> 'int':
        return self.get_access_type_id('manage')

    @contract
    def get_seeanswers_access_id(self) -> 'int':
        return self.get_access_type_id('see answers')

    @contract
    def has_admin_access(self, user_id: 'int') -> 'bool':
        return self.is_user_id_in_group(user_id, 'Administrators')

    @contract
    def has_view_access(self, user_id: 'int', block_id: 'int') -> 'bool':
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

    @contract
    def has_teacher_access(self, user_id: 'int', block_id: 'int') -> 'bool':
        """Returns whether the user has teacher access to the specified block.

        :param user_id: The user id to check.
        :param block_id: The block id to check.
        :returns: True if the user with id 'user_id' has teacher access to the block 'block_id', false otherwise.
        """
        return self.has_access(user_id, block_id, self.get_manage_access_id(), self.get_teacher_access_id())

    @contract
    def has_manage_access(self, user_id: 'int', block_id: 'int') -> 'bool':
        """Returns whether the user has manage access to the specified block.

        :param user_id: The user id to check.
        :param block_id: The block id to check.
        :returns: True if the user with id 'user_id' has manage access to the block 'block_id', false otherwise.
        """
        return self.has_access(user_id, block_id, self.get_manage_access_id())

    @contract
    def has_access(self, user_id: 'int', block_id: 'int', *access_ids) -> 'bool':
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
{} INTERSECT
SELECT User_id
FROM UserGroupMember
WHERE UserGroup_id IN
      (SELECT UserGroup_id
       FROM BlockAccess
       WHERE Block_id = ? AND type IN ({})
       UNION SELECT UserGroup_id
             FROM Block
             WHERE id = ?
      )
""".format(' UNION '.join('SELECT ' + str(x) for x in user_ids), ','.join('?' * len(access_ids)))
        result = self.db.execute(whole_sql, [block_id] + list(access_ids) + [block_id]).fetchone()
        return result is not None

    @contract
    def get_accessible_blocks(self, user_id: int, access_types: 'list(int)') -> 'set(int)':
        user_ids = [user_id, self.get_anon_user_id()]
        if user_id > 0:
            user_ids.append(self.get_logged_user_id())
        r = self.db.execute("""
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
        return set(row[0] for row in r.fetchall())

    def get_viewable_blocks(self, user_id: int) -> 'set(int)':
        return self.get_accessible_blocks(user_id, [self.get_view_access_id(),
                                                    self.get_edit_access_id(),
                                                    self.get_manage_access_id(),
                                                    self.get_teacher_access_id(),
                                                    self.get_seeanswers_access_id()])

    @contract
    def has_edit_access(self, user_id: 'int', block_id: 'int') -> 'bool':
        """Returns whether the user has edit access to the specified block.
        
        :param user_id:
        :param block_id:
        :returns: True if the user with id 'user_id' has edit access to the block 'block_id', false otherwise.
        """

        return self.has_access(user_id, block_id, self.get_edit_access_id(), self.get_manage_access_id())

    @contract
    def has_seeanswers_access(self, uid: 'int', block_id: 'int') -> 'bool':
        return self.has_access(uid, block_id,
                               self.get_seeanswers_access_id(),
                               self.get_manage_access_id(),
                               self.get_teacher_access_id())

    @contract
    def user_is_owner(self, user_id: 'int', block_id: 'int') -> 'bool':
        """Returns whether the user belongs to the owners of the specified block.
        
        :param user_id:
        :param block_id:
        :returns: True if the user with 'user_id' belongs to the owner group of the block 'block_id'.
        """
        if self.has_admin_access(user_id):
            return True

        cursor = self.db.cursor()
        cursor.execute("""SELECT id FROM User WHERE
                          id = ?
                          AND (id IN
                              (SELECT User_id FROM UserGroupMember WHERE UserGroup_id IN
                              (SELECT UserGroup_id FROM Block WHERE id = ?))
                              )""", [user_id, block_id])
        result = cursor.fetchall()
        assert len(result) <= 1, 'rowcount should be 1 at most'
        return len(result) == 1

    def get_preferences(self, user_id: 'int') -> 'str':
        """Gets the preferences of a user.

        :param user_id: The id of the user.
        :returns: The user preferences as a string.
        """
        cursor = self.db.cursor()
        cursor.execute("""SELECT prefs FROM User WHERE id = ?""", [user_id])
        result = self.resultAsDictionary(cursor)
        return result[0]['prefs']

    def set_preferences(self, user_id: 'int', prefs: 'str'):
        """Sets the preferences for a user.

        :param user_id: The id of the user.
        :param prefs: The preferences to set.
        """
        cursor = self.db.cursor()
        cursor.execute("""UPDATE User SET prefs = ? WHERE id = ?""", [prefs, user_id])
        self.db.commit()

    def get_users_for_group(self, usergroup_name, order=False):
        order_sql = ' ORDER BY User.name' if order else ''
        return self.resultAsDictionary(
            self.db.execute("""SELECT User.id, User.name, real_name, email
                           FROM User
                           JOIN UserGroupMember ON User.id = UserGroupMember.User_id
                           JOIN UserGroup ON UserGroup.id = UserGroupMember.UserGroup_id
                           WHERE UserGroup.name = ?{}""".format(order_sql), [usergroup_name]))

    def get_access_type_id(self, access_type):
        if not self.access_type_map:
            result = self.db.execute("""SELECT id, name FROM AccessType""").fetchall()
            for row in result:
                self.access_type_map[row[1]] = row[0]
        return self.access_type_map[access_type]

    def get_access_types(self):
        return self.resultAsDictionary(self.db.execute("""SELECT id, name FROM AccessType"""))

    @contract
    def remove_membership(self, uid: 'int', gid: 'int', commit: 'bool'=True) -> 'int':
        """Removes membership of a user from a group.
        :param uid: The user id.
        :param gid: The group id.
        :returns: The number of affected rows (0 or 1).
        """
        c = self.db.cursor()
        c.execute("""DELETE FROM UserGroupMember WHERE User_id = ? and UserGroup_id = ?""", [uid, gid])
        if commit:
            self.db.commit()
        return c.rowcount

    @contract
    def create_user_with_group(self, name: 'str',
                               real_name: 'str|None'=None,
                               email: 'str|None'=None,
                               password: 'str|None'=None,
                               is_admin: 'bool'=False):
        user_id = self.create_user(name, real_name or name, email or name + '@example.com', password=password or '')
        user_group = self.create_usergroup(name)
        self.add_user_to_group(user_group, user_id)
        if is_admin:
            self.addUserToAdmins(user_id)
        return user_id, user_group

    @contract
    def get_personal_usergroup_by_id(self, user_id: int) -> 'int|None':
        return self.get_personal_usergroup(self.get_user(user_id))

    @contract
    def get_anon_user_id(self) -> int:
        global ANON_USER_ID
        if ANON_USER_ID is not None:
            return ANON_USER_ID
        ANON_USER_ID = self.get_user_by_name(ANONYMOUS_USERNAME)
        return ANON_USER_ID

    @contract
    def get_logged_user_id(self) -> int:
        global LOGGED_USER_ID
        if LOGGED_USER_ID is not None:
            return LOGGED_USER_ID
        LOGGED_USER_ID = self.get_user_by_name(LOGGED_IN_USERNAME)
        return LOGGED_USER_ID

    @contract
    def get_anon_group_id(self) -> int:
        global ANON_GROUP_ID
        if ANON_GROUP_ID is not None:
            return ANON_GROUP_ID
        ANON_GROUP_ID = self.get_usergroup_by_name(ANONYMOUS_GROUPNAME)
        return ANON_GROUP_ID

    @contract
    def get_logged_group_id(self) -> int:
        global LOGGED_GROUP_ID
        if LOGGED_GROUP_ID is not None:
            return LOGGED_GROUP_ID
        LOGGED_GROUP_ID = self.get_usergroup_by_name(LOGGED_IN_GROUPNAME)
        return LOGGED_GROUP_ID

    def set_usergroup_name(self, group_id: int, user_name: str):
        self.db.execute("""UPDATE UserGroup SET name = ? WHERE id = ?""", (user_name, group_id))
        self.db.commit()
