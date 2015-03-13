import json
from contracts import contract, new_contract
from timdb.timdbbase import TimDbBase

import hashlib
import sqlite3

new_contract('row', sqlite3.Row)

ANONYMOUS_GROUP = 2
LOGGED_IN_GROUP = 0


class Users(TimDbBase):
    """Handles saving and retrieving user-related information to/from the database."""

    @contract
    def createAnonymousAndLoggedInUserGroups(self) -> 'int':
        """Creates an anonymous user and a usergroup for it.
        The user id and its associated usergroup id is 0.
        """

        cursor = self.db.cursor()
        cursor.execute('INSERT INTO User (id, name) VALUES (?, ?)', [0, 'Anonymous'])
        cursor.execute('INSERT INTO UserGroup (id, name) VALUES (?, ?)', [ANONYMOUS_GROUP, 'Anonymous users'])
        cursor.execute('INSERT INTO UserGroupMember (User_id, UserGroup_id) VALUES (?, ?)', [0, ANONYMOUS_GROUP])
        cursor.execute('INSERT INTO UserGroup (id, name) VALUES (?, ?)', [LOGGED_IN_GROUP, 'Logged-in users'])
        self.db.commit()
        return 0

    @contract
    def createUser(self, name: 'str', real_name: 'str', email: 'str', password: 'str' = '', commit : 'bool' = True) -> 'int':
        """Creates a new user with the specified name.
        
        :param email: The email address of the user.
        :param name: The name of the user to be created.
        :param real_name: The real name of the user.
        :param password: The password for the user (not used on Korppi login).
        :returns: The id of the newly created user.
        """
        cursor = self.db.cursor()
        hash = self.hashPassword(password) if password != '' else ''
        cursor.execute('INSERT INTO User (name, real_name, email, pass) VALUES (?, ?, ?, ?)', [name, real_name, email, hash])
        if commit:
            self.db.commit()
        user_id = cursor.lastrowid
        return user_id

    def updateUser(self, user_id: 'int', name: 'str', real_name: 'str', email: 'str', password: 'str' = '', commit : 'bool' = True):
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
    def createUserGroup(self, name: 'str', commit : 'bool' = True) -> 'int':
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
    def addUserToGroup(self, group_id: 'int', user_id: 'int', commit : 'bool' = True):
        """Adds a user to a usergroup.
        
        :param group_id: The id of the group.
        :param user_id: The id of the user.
        """
        cursor = self.db.cursor()
        cursor.execute('INSERT INTO UserGroupMember (UserGroup_id, User_id) VALUES (?, ?)', [group_id, user_id])
        if commit:
            self.db.commit()

    @contract
    def createPotentialUser(self, email: 'str', password: 'str', commit : 'bool' = True):
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
    def deletePotentialUser(self, email: 'str', commit : 'bool' = True):
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
        cursor.execute('SELECT email FROM NewUser WHERE email=? and pass=?', [email, hash])
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
        cursor.execute('SELECT email FROM User WHERE email=? and pass=?', [email, hash])
        return cursor.fetchone() is not None

    @contract
    def hashPassword(self, password: 'str') -> 'str':
        return hashlib.sha256(password.encode()).hexdigest()

    @contract
    def testPassword(self, hash: 'str', password: 'str'):
        return self.hashPassword(password) == hash

    @contract
    def getEditors(self, block_id: 'int'):
        """Gets the users that are allowed to edit the speficied block.
        
        :param block_id: The id of the block.
        :returns: The list of users who can edit the block.
        """

        cursor = self.db.cursor()
        cursor.execute("""SELECT b.UserGroup_id, b.editable_from, b.editable_to, u.name FROM BlockEditAccess b
                          JOIN UserGroup u ON b.UserGroup_id = u.id
                               WHERE Block_id = ?""", [block_id])
        return self.resultAsDictionary(cursor)

    @contract
    def getViewers(self, block_id: 'int'):
        """Gets the users that are allowed to view the speficied block.
        
        :param block_id: The id of the block.
        :returns: The list of users who can view the block.
        """

        cursor = self.db.cursor()
        cursor.execute("""SELECT b.UserGroup_id, b.visible_from, b.visible_to, u.name FROM BlockViewAccess b
                          JOIN UserGroup u ON b.UserGroup_id = u.id
                               WHERE Block_id = ?""", [block_id])
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
    def userExists(self, user_id : 'int') -> 'bool':
        """Checks if the user with the specified id. exists

        :returns: Boolean.
        """

        cursor = self.db.cursor()
        cursor.execute('select id from User where id = ?', [user_id])
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
    def groupExists(self, user_id : 'int') -> 'bool':
        """Checks if the group with the specified id. exists

        :returns: Boolean.
        """

        cursor = self.db.cursor()
        cursor.execute('select id from UserGroup where id = ?', [user_id])
        return cursor.fetchone() is not None

    @contract
    def getUserGroupsByName(self, name: 'str'):
        """Gets the usergroup that has the specified name.
        
        :param name: The name of the usergroup to be retrieved.
        """

        cursor = self.db.cursor()
        cursor.execute('SELECT id FROM UserGroup WHERE name = ?', [name])
        return self.resultAsDictionary(cursor)

    @contract
    def getUserGroups(self, user_id: 'int') -> 'list(dict)':
        """Gets the user groups of a user.
        
        :param user_id: The id of the user.
        :returns: The user groups that the user belongs to.
        """

        cursor = self.db.cursor()
        cursor.execute("""SELECT id, name FROM UserGroup WHERE id IN
                          (SELECT UserGroup_id FROM UserGroupMember WHERE User_id = ?)
                          ORDER BY id ASC""", [user_id])
        return self.resultAsDictionary(cursor)

    def __grantAccess(self, group_id: 'int', block_id: 'int', access_type: 'str'):
        """Grants access to a group for a block.
        
        :param group_id: The group id to which to grant view access.
        :param block_id: The id of the block for which to grant view access.
        :param access_type: The kind of access. Possible values are 'edit' and 'view'.
        """

        # TODO: Check that the group_id and block_id exist.
        assert access_type in ['view', 'edit'], 'invalid value for access_type'
        cursor = self.db.cursor()
        if access_type == 'view':
            cursor.execute(
                'INSERT OR IGNORE INTO BlockViewAccess (Block_id,UserGroup_id,visible_from) VALUES (?,?,CURRENT_TIMESTAMP)'
                , [block_id, group_id])
        else:
            cursor.execute(
                'INSERT OR IGNORE INTO BlockEditAccess (Block_id,UserGroup_id,editable_from) VALUES (?,?,CURRENT_TIMESTAMP)'
                , [block_id, group_id])
        self.db.commit()

    @contract
    def grantViewAccess(self, group_id: 'int', block_id: 'int'):
        """Grants view access to a group for a block.
        
        :param group_id: The group id to which to grant view access.
        :param block_id: The id of the block for which to grant view access.
        """

        self.__grantAccess(group_id, block_id, 'view')

    @contract
    def grantEditAccess(self, group_id: 'int', block_id: 'int'):
        """Grants edit access to a group for a block.
        
        :param group_id: The group id to which to grant view access.
        :param block_id: The id of the block for which to grant view access.
        """

        self.__grantAccess(group_id, block_id, 'edit')

    @contract
    def removeViewAccess(self, group_id: 'int', block_id: 'int'):
        """Removes view access from a user for a block.
        
        :param group_id: The id of the group from which to remove view access.
        :param block_id: The block id.
        """

        cursor = self.db.cursor()
        cursor.execute("DELETE FROM BlockViewAccess WHERE UserGroup_id = ? AND Block_id = ?", [group_id, block_id])
        self.db.commit()

    @contract
    def removeEditAccess(self, group_id: 'int', block_id: 'int'):
        """Removes edit access from a user for a block.
        
        :param group_id: The id of the group from which to remove edit access.
        :param block_id: The block id.
        """

        cursor = self.db.cursor()
        cursor.execute("DELETE FROM BlockEditAccess WHERE UserGroup_id = ? AND Block_id = ?", [group_id, block_id])
        self.db.commit()

    @contract
    def userHasViewAccess(self, user_id: 'int', block_id: 'int') -> 'bool':
        """Returns whether the user has view access to the specified block.
        
        :param user_id:
        :param block_id:
        :returns: True if the user with id 'user_id' has view access to the block 'block_id', false otherwise.
        """

        if self.userIsOwner(user_id, block_id):
            return True
        if self.checkAnonViewAccess(block_id):
            return True
        if user_id > 0 and self.checkLoggedInViewAccess(block_id):
            return True
        result = self.db.execute("""SELECT id FROM User WHERE
                          id = ?
                          AND User.id IN
                              (SELECT User_id FROM UserGroupMember WHERE UserGroup_id IN
                                  (SELECT UserGroup_id FROM BlockViewAccess WHERE Block_id = ?))
                          """, [user_id, block_id]).fetchall()
        return len(result) > 0

    @contract
    def userHasEditAccess(self, user_id: 'int', block_id: 'int') -> 'bool':
        """Returns whether the user has edit access to the specified block.
        
        :param user_id:
        :param block_id:
        :returns: True if the user with id 'user_id' has view access to the block 'block_id', false otherwise.
        """

        if self.userIsOwner(user_id, block_id):
            return True
        if self.checkAnonEditAccess(block_id):
            return True
        if user_id > 0 and self.checkLoggedInEditAccess(block_id):
            return True
        # TODO: This method is pretty much copy-paste from userHasViewAccess. Should make some common method.
        result = self.db.execute("""SELECT id FROM User
                          WHERE id = ?
                          AND User.id IN
                              (SELECT User_id FROM UserGroupMember WHERE UserGroup_id IN
                                  (SELECT UserGroup_id FROM BlockEditAccess WHERE Block_id = ?))
                          """, [user_id, block_id]).fetchall()
        return len(result) > 0

    def checkUserGroupEditAccess(self, block_id: 'int', usergroup_id: 'int') -> 'bool':
        result = self.db.execute("""SELECT UserGroup_id FROM BlockEditAccess
                           WHERE Block_id = ? AND UserGroup_id = ?""", [block_id, usergroup_id]).fetchall()
        return len(result) > 0

    def checkAnonEditAccess(self, block_id: 'int') -> 'bool':
        return self.checkUserGroupEditAccess(block_id, ANONYMOUS_GROUP)

    def checkLoggedInEditAccess(self, block_id: 'int') -> 'bool':
        return self.checkUserGroupEditAccess(block_id, LOGGED_IN_GROUP)

    def checkUserGroupViewAccess(self, block_id: 'int', usergroup_id: 'int') -> 'bool':
        result = self.db.execute("""SELECT UserGroup_id FROM BlockViewAccess
                           WHERE Block_id = ? AND UserGroup_id = ?""", [block_id, usergroup_id]).fetchall()
        return len(result) > 0

    def checkAnonViewAccess(self, block_id: 'int') -> 'bool':
        return self.checkUserGroupViewAccess(block_id, ANONYMOUS_GROUP)

    def checkLoggedInViewAccess(self, block_id: 'int') -> 'bool':
        return self.checkUserGroupViewAccess(block_id, LOGGED_IN_GROUP)

    @contract
    def userIsOwner(self, user_id: 'int', block_id: 'int') -> 'bool':
        """Returns whether the user belongs to the owners of the specified block.
        
        :param user_id:
        :param block_id:
        :returns: True if the user with 'user_id' belongs to the owner group of the block 'block_id'.
        """
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
        cursor.execute("""SELECT id from UserGroup where id == ?
                          AND id IN
                             (SELECT UserGroup_id
                              FROM BlockViewAccess
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
        cursor.execute("""SELECT prefs from User WHERE id = ?""", [user_id])
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
