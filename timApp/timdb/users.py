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
ADMIN_GROUPNAME = "Administrators"


class Users(TimDbBase):
    """Handles saving and retrieving user-related information to/from the database."""

    access_type_map = {}

    @contract
    def createAnonymousAndLoggedInUserGroups(self) -> 'int':
        """Creates an anonymous user and a usergroup for it.
        The user id and its associated usergroup id is 0.
        """

        # Please keep these local and refer to the groups with their names instead.
        # They may differ depending on the script version they were created with.
        ANONYMOUS_USERID = 0
        ANONYMOUS_GROUPID = 2
        LOGGED_IN_GROUPID = 0
        KORPPI_GROUPID = 3
        ADMIN_GROUPID = 4

        cursor = self.db.cursor()
        cursor.execute('INSERT INTO User (id, name) VALUES (?, ?)', [0, ANONYMOUS_USERNAME])
        cursor.execute('INSERT INTO UserGroup (id, name) VALUES (?, ?)', [ANONYMOUS_GROUPID, ANONYMOUS_GROUPNAME])
        cursor.execute('INSERT INTO UserGroupMember (User_id, UserGroup_id) VALUES (?, ?)',
                       [ANONYMOUS_USERID, ANONYMOUS_GROUPID])
        cursor.execute('INSERT INTO UserGroup (id, name) VALUES (?, ?)', [LOGGED_IN_GROUPID, LOGGED_IN_GROUPNAME])
        cursor.execute('INSERT INTO UserGroup (id, name) VALUES (?, ?)', [KORPPI_GROUPID, KORPPI_GROUPNAME])
        cursor.execute('INSERT INTO UserGroup (id, name) VALUES (?, ?)', [ADMIN_GROUPID, ADMIN_GROUPNAME])
        self.db.commit()
        return 0

    @contract
    def createUser(self, name: 'str', real_name: 'str', email: 'str', password: 'str' = '',
                   commit: 'bool' = True) -> 'int':
        """Creates a new user with the specified name.
        
        :param email: The email address of the user.
        :param name: The name of the user to be created.
        :param real_name: The real name of the user.
        :param password: The password for the user (not used on Korppi login).
        :returns: The id of the newly created user.
        """
        cursor = self.db.cursor()
        hash = self.hashPassword(password) if password != '' else ''
        cursor.execute('INSERT INTO User (name, real_name, email, pass) VALUES (?, ?, ?, ?)',
                       [name, real_name, email, hash])
        if commit:
            self.db.commit()
        user_id = cursor.lastrowid
        return user_id

    def createAnonymousUser(self, name: 'str', real_name: 'str', commit: 'bool' = True) -> 'int':
        """Creates a new user anonymous user.
        :param id: id of anonymous user
        :param name: The name of the user to be created.
        :param real_name: The real name of the user.
        :returns: The id of the newly created user.
        """

        next_id = self.getNextAnonymousUserId()

        cursor = self.db.cursor()
        cursor.execute('INSERT INTO User (id, name, real_name) VALUES (?, ?, ?)', [next_id, name, real_name])
        if commit:
            self.db.commit()
        user_id = cursor.lastrowid
        self.addUserToGroup(2, user_id)
        return user_id

    @contract
    def getNextAnonymousUserId(self) -> 'int':
        """
        :returns: Tnext unused negative id.
        """
        cursor = self.db.cursor()
        cursor.execute('SELECT MIN(id) AS next_id FROM User')
        user_id = min(self.resultAsDictionary(cursor)[0]['next_id'], 0)
        return user_id - 1

    def updateUser(self, user_id: 'int', name: 'str', real_name: 'str', email: 'str', password: 'str' = '',
                   commit: 'bool' = True):
        """Updates user information.

        :param user_id: The id of the user to be updated.
        :param name: The username of the user.
        :param real_name: The real name of the user.
        :param email: The email of the user.
        :param password: The password of the user.
        """

        cursor = self.db.cursor()
        pass_hash = self.hashPassword(password) if password != '' else ''
        cursor.execute('UPDATE User SET name = ?, real_name = ?, email = ?, pass = ? WHERE id = ?',
                       [name, real_name, email, pass_hash, user_id])
        if commit:
            self.db.commit()

    @contract
    def createUserGroup(self, name: 'str', commit: 'bool' = True) -> 'int':
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
    def addUserToGroup(self, group_id: 'int', user_id: 'int', commit: 'bool' = True):
        """Adds a user to a usergroup.
        
        :param group_id: The id of the group.
        :param user_id: The id of the user.
        """
        cursor = self.db.cursor()
        cursor.execute('INSERT INTO UserGroupMember (UserGroup_id, User_id) VALUES (?, ?)', [group_id, user_id])
        if commit:
            self.db.commit()

    @contract
    def addUserToNamedGroup(self, group_name: 'str', user_id: 'int', commit: 'bool' = True):
        group_id = self.getUserGroupByName(group_name)
        if group_id is not None:
            self.addUserToGroup(group_id, user_id, commit)
        else:
            print("Could not add user {} to group {}".format(user_id, group_name))

    @contract
    def addUserToKorppiGroup(self, user_id: 'int', commit: 'bool' = True):
        self.addUserToNamedGroup(KORPPI_GROUPNAME, user_id, commit)

    @contract
    def addUserToAdmins(self, user_id: 'int', commit: 'bool' = True):
        self.addUserToNamedGroup(ADMIN_GROUPNAME, user_id, commit)

    @contract
    def createPotentialUser(self, email: 'str', password: 'str', commit: 'bool' = True):
        """Creates a potential user with the specified email and password.
        
        :param email: The email address of the user.
        :param password: The password of the user.
        """
        cursor = self.db.cursor()
        hash = self.hashPassword(password)
        cursor.execute('REPLACE INTO NewUser (email, pass) VALUES (?, ?)', [email, hash])
        if commit:
            self.db.commit()

    @contract
    def deletePotentialUser(self, email: 'str', commit: 'bool' = True):
        """Deletes a potential user.
        
        :param email: The email address of the user.
        """
        cursor = self.db.cursor()
        cursor.execute('DELETE FROM NewUser WHERE email=?', [email])
        if commit:
            self.db.commit()

    @contract
    def testPotentialUser(self, email: 'str', password: 'str') -> 'bool':
        """Tests if a potential user matches to email and password.
        
        :param email: Email address.
        :param password: Password.
        :returns: Boolean.
        """

        cursor = self.db.cursor()
        hash = self.hashPassword(password)
        cursor.execute('SELECT email FROM NewUser WHERE email=? AND pass=?', [email, hash])
        return cursor.fetchone() is not None

    @contract
    def testUser(self, email: 'str', password: 'str') -> 'bool':
        """Tests if a potential user matches to email and password.
        
        :param email: Email address.
        :param password: Password.
        :returns: Boolean.
        """

        cursor = self.db.cursor()
        hash = self.hashPassword(password)
        cursor.execute('SELECT email FROM User WHERE email=? AND pass=?', [email, hash])
        return cursor.fetchone() is not None

    @contract
    def hashPassword(self, password: 'str') -> 'str':
        return hashlib.sha256(password.encode()).hexdigest()

    @contract
    def testPassword(self, hash: 'str', password: 'str'):
        return self.hashPassword(password) == hash

    @contract
    def get_rights_holders(self, block_id: 'int'):
        cursor = self.db.cursor()
        cursor.execute("""SELECT b.UserGroup_id as gid, u.name as name, a.id as access_type, a.name as access_name FROM BlockAccess b
                          JOIN UserGroup u ON b.UserGroup_id = u.id
                          JOIN AccessType a ON b.type = a.id
                          WHERE Block_id = ?""", [block_id])
        return self.resultAsDictionary(cursor)

    @contract
    def getEditors(self, block_id: 'int'):
        """Gets the users that are allowed to edit the speficied block.
        
        :param block_id: The id of the block.
        :returns: The list of users who can edit the block.
        """

        cursor = self.db.cursor()
        cursor.execute("""SELECT b.UserGroup_id, b.accessible_from, b.accessible_to, u.name FROM BlockAccess b
                          JOIN UserGroup u ON b.UserGroup_id = u.id
                               WHERE Block_id = ? AND type = ?""", [block_id, self.get_edit_access_id()])
        return self.resultAsDictionary(cursor)

    @contract
    def getViewers(self, block_id: 'int'):
        """Gets the users that are allowed to view the speficied block.
        
        :param block_id: The id of the block.
        :returns: The list of users who can view the block.
        """

        cursor = self.db.cursor()
        cursor.execute("""SELECT b.UserGroup_id, b.accessible_from, b.accessible_to, u.name FROM BlockAccess b
                          JOIN UserGroup u ON b.UserGroup_id = u.id
                               WHERE Block_id = ? AND type = ?""", [block_id, self.get_view_access_id()])
        return self.resultAsDictionary(cursor)

    @contract
    def getOwnerGroup(self, block_id: 'int'):
        """Returns the owner group of the specified block.
        
        :param block_id: The id of the block.
        """

        cursor = self.db.cursor()
        cursor.execute('SELECT id, name FROM UserGroup WHERE id IN (SELECT UserGroup_id FROM Block WHERE id = ?)',
                       [block_id])
        return self.resultAsDictionary(cursor)[0]

    @contract
    def userExists(self, user_id: 'int') -> 'bool':
        """Checks if the user with the specified id. exists

        :returns: Boolean.
        """

        cursor = self.db.cursor()
        cursor.execute('SELECT id FROM User WHERE id = ?', [user_id])
        return cursor.fetchone() is not None

    @contract
    def getUser(self, user_id: 'int') -> 'dict|None':
        """Gets the user with the specified id.
        
        :param user_id:
        :returns: An sqlite3 row object representing the user. Columns: id, name.
        """

        cursor = self.db.cursor()
        cursor.execute('SELECT id, name, real_name, email FROM User WHERE id = ?', [user_id])
        result = self.resultAsDictionary(cursor)
        return result[0] if len(result) > 0 else None

    @contract
    def getUserByName(self, name: 'str') -> 'int|None':
        """Gets the id of the specified username.
        
        :param name: The name of the user.
        :returns: The id of the user or None if the user does not exist.
        """

        cursor = self.db.cursor()
        cursor.execute('SELECT id FROM User WHERE name = ?', [name])
        result = cursor.fetchone()
        return result[0] if result is not None else None

    @contract
    def getUserByEmail(self, email: 'str') -> 'dict|None':
        """Gets the data of the specified user email address.
        
        :param name: Email address.
        :returns: The user data.
        """

        cursor = self.db.cursor()
        cursor.execute('SELECT * FROM User WHERE email = ?', [email])
        result = self.resultAsDictionary(cursor)
        return result[0] if len(result) > 0 else None

    @contract
    def groupExists(self, group_id: 'int') -> 'bool':
        """Checks if the group with the specified id. exists

        :param group_id: The id of the group.
        :returns: Boolean.
        """

        cursor = self.db.cursor()
        cursor.execute('SELECT id FROM UserGroup WHERE id = ?', [group_id])
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
    def getUserGroupsByName(self, name: 'str'):
        """Gets the usergroups that have the specified name.
        
        :param name: The name of the usergroup to be retrieved.
        """

        cursor = self.db.cursor()
        cursor.execute('SELECT id FROM UserGroup WHERE name = ?', [name])
        return self.resultAsDictionary(cursor)

    @contract
    def getUserGroupByName(self, name: 'str') -> 'int|None':
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
    def getPersonalUserGroup(self, user_id: 'int') -> 'int':
        """Gets the personal user group for the user.
        """
        user = self.getUser(user_id)
        if user is None:
            raise TimDbException("No such user")

        userName = self.getUser(user_id)['name']
        groups = self.getUserGroupsByName(userName)
        if len(groups) > 0:
            return groups[0]['id']

        groups = self.getUserGroupsByName('group of user ' + userName)
        if len(groups) > 0:
            return groups[0]['id']

        return self.getUserGroups(user_id)[0]['id']

    @contract
    def getUserGroups(self, user_id: 'int') -> 'list(dict)':
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
    def getUserGroupsPrintable(self, user_id: 'int', max_group_len: 'int' = 32) -> 'list(dict)':
        """Gets the user groups of a user, truncating the group names.

        :param user_id: The id of the user.
        :returns: The user groups that the user belongs to.
        """
        groups = self.getUserGroups(user_id)
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
    def getGroupUsers(self, group_id: 'int') -> 'list(dict)':
        cursor = self.db.cursor()
        cursor.execute("""SELECT User_id as id, User_name FROM UserGroupMember WHERE
                          User_name = (SELECT name FROM User WHERE id = ?) AND
                          User_id   = (SELECT id FROM UserGroup WHERE UserGroup_id = ?)
                       """)

        return len(cursor.fetchall()) > 0

    @contract
    def isUserIdInGroup(self, user_id: 'int', usergroup_name: 'str') -> 'bool':
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
    def grantViewAccess(self, group_id: 'int', block_id: 'int'):
        """Grants view access to a group for a block.
        
        :param group_id: The group id to which to grant view access.
        :param block_id: The id of the block for which to grant view access.
        """

        self.grant_access(group_id, block_id, 'view')

    @contract
    def grantEditAccess(self, group_id: 'int', block_id: 'int'):
        """Grants edit access to a group for a block.
        
        :param group_id: The group id to which to grant view access.
        :param block_id: The id of the block for which to grant view access.
        """

        self.grant_access(group_id, block_id, 'edit')

    @contract
    def removeViewAccess(self, group_id: 'int', block_id: 'int'):
        """Removes view access from a user for a block.
        
        :param group_id: The id of the group from which to remove view access.
        :param block_id: The block id.
        """

        cursor = self.db.cursor()
        cursor.execute("DELETE FROM BlockAccess WHERE UserGroup_id = ? AND Block_id = ? AND type = ?",
                       [group_id, block_id, self.get_view_access_id()])
        self.db.commit()

    def get_view_access_id(self):
        return self.get_access_type_id('view')

    @contract
    def remove_access(self, group_id: 'int', block_id: 'int', access_type: 'str'):
        cursor = self.db.cursor()
        cursor.execute("DELETE FROM BlockAccess WHERE UserGroup_id = ? AND Block_id = ? AND type = ?",
                       [group_id, block_id, self.get_access_type_id(access_type)])
        self.db.commit()

    @contract
    def removeEditAccess(self, group_id: 'int', block_id: 'int'):
        """Removes edit access from a user for a block.
        
        :param group_id: The id of the group from which to remove edit access.
        :param block_id: The block id.
        """

        cursor = self.db.cursor()
        cursor.execute("DELETE FROM BlockAccess WHERE UserGroup_id = ? AND Block_id = ? AND type = ?",
                       [group_id, block_id, self.get_edit_access_id()])
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
    def has_admin_access(self, user_id: 'int') -> 'bool':
        return self.isUserIdInGroup(user_id, 'Administrators')

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
                               self.get_teacher_access_id())

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

        if self.userIsOwner(user_id, block_id):
            return True
        for access_id in access_ids:
            if self.checkUserGroupAccess(block_id, self.getUserGroupByName(ANONYMOUS_GROUPNAME), access_id):
                return True
            if user_id > 0 and self.checkUserGroupAccess(block_id, self.getUserGroupByName(LOGGED_IN_GROUPNAME), access_id):
                return True
            result = self.db.execute("""SELECT id FROM User WHERE
                              id = ?
                              AND User.id IN
                                  (SELECT User_id FROM UserGroupMember WHERE UserGroup_id IN
                                      (SELECT UserGroup_id FROM BlockAccess WHERE Block_id = ? AND type = ?))
                              """, [user_id, block_id, access_id]).fetchone()
            if result is not None:
                return True
        return False

    @contract
    def has_edit_access(self, user_id: 'int', block_id: 'int') -> 'bool':
        """Returns whether the user has edit access to the specified block.
        
        :param user_id:
        :param block_id:
        :returns: True if the user with id 'user_id' has edit access to the block 'block_id', false otherwise.
        """

        return self.has_access(user_id, block_id, self.get_edit_access_id(), self.get_manage_access_id())

    def checkUserGroupAccess(self, block_id: 'int', usergroup_id: 'int|None', access_id: 'int') -> 'bool':
        if usergroup_id is None:
            return False

        result = self.db.execute("""SELECT UserGroup_id FROM BlockAccess
                                    WHERE Block_id = ?
                                     AND UserGroup_id = ?
                                     AND type = ?""", [block_id, usergroup_id, access_id]).fetchall()
        return len(result) > 0

    @contract
    def userIsOwner(self, user_id: 'int', block_id: 'int') -> 'bool':
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

    @contract
    def userGroupHasViewAccess(self, user_group_id: 'int', block_id: 'int') -> 'bool':
        """

        :param user_group_id:
        :param block_id:
        :return:
        """
        cursor = self.db.cursor()
        cursor.execute("""SELECT id FROM UserGroup WHERE id == ?
                          AND id IN
                             (SELECT UserGroup_id
                              FROM BlockAccess
                              WHERE Block_id = ?)""", [user_group_id, block_id])
        result = cursor.fetchall()
        assert len(result) <= 1, 'rowcount should be 1 at most'
        return len(result) == 1

    def getPrefs(self, user_id: 'int') -> 'str':
        """Gets the preferences of a user.

        :param user_id: The id of the user.
        :returns: The user preferences as a string.
        """
        cursor = self.db.cursor()
        cursor.execute("""SELECT prefs FROM User WHERE id = ?""", [user_id])
        result = self.resultAsDictionary(cursor)
        return result[0]['prefs']

    def setPrefs(self, user_id: 'int', prefs: 'str'):
        """Sets the preferences for a user.

        :param user_id: The id of the user.
        :param prefs: The preferences to set.
        """
        cursor = self.db.cursor()
        cursor.execute("""UPDATE User SET prefs = ? WHERE id = ?""", [prefs, user_id])
        self.db.commit()

    def get_users_for_group(self, usergroup_name):
        return self.resultAsDictionary(
            self.db.execute("""SELECT User.id, User.name, real_name, email
                           FROM User
                           JOIN UserGroupMember ON User.id = UserGroupMember.User_id
                           JOIN UserGroup ON UserGroup.id = UserGroupMember.UserGroup_id
                           WHERE UserGroup.name = ?""", [usergroup_name]))

    def get_access_type_id(self, access_type):
        if not self.access_type_map:
            result = self.db.execute("""SELECT id, name FROM AccessType""").fetchall()
            for row in result:
                self.access_type_map[row[1]] = row[0]
        return self.access_type_map[access_type]

    def get_access_types(self):
        return self.resultAsDictionary(self.db.execute("""SELECT id, name FROM AccessType"""))
