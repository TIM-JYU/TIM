from contracts import contract, new_contract
from timdb.timdbbase import TimDbBase
import sqlite3

new_contract('row', sqlite3.Row)


class Users(TimDbBase):
    """Handles saving and retrieving user-related information to/from the database."""

    @contract
    def createAnonymousUser(self) -> 'int':
        """Creates an anonymous user and a usergroup for it.
        The user id and its associated usergroup id is 0.
        """

        cursor = self.db.cursor()
        cursor.execute('INSERT INTO User (id, name) VALUES (?, ?)', [0, 'Anonymous'])
        cursor.execute('INSERT INTO UserGroup (id, name) VALUES (?, ?)', [0, 'all'])
        cursor.execute('INSERT INTO UserGroupMember (User_id, UserGroup_id) VALUES (?, ?)', [0, 0])
        self.db.commit()
        return 0

    @contract
    def createUser(self, name: 'str', real_name: 'str', email: 'str') -> 'int':
        """Creates a new user with the specified name.
        
        :param email: The email address of the user.
        :param real_name: The real name of the user.
        :param name: The name of the user to be created.
        :returns: The id of the newly created user.
        """
        cursor = self.db.cursor()
        cursor.execute('INSERT INTO User (name, real_name, email) VALUES (?, ?, ?)', [name, real_name, email])
        self.db.commit()
        user_id = cursor.lastrowid
        return user_id

    def updateUser(self, user_id: 'int', name: 'str', real_name: 'str', email: 'str'):
        """Updates user information.

        :param user_id: The id of the user to be updated.
        :param name: The username of the user.
        :param real_name: The real name of the user.
        :param email: The email of the user.
        """

        cursor = self.db.cursor()
        cursor.execute('UPDATE User SET name = ?, real_name = ?, email = ? WHERE id = ?',
                       [name, real_name, email, user_id])
        self.db.commit()

    @contract
    def createUserGroup(self, name: 'str') -> 'int':
        """Creates a new user group.
        
        :param name: The name of the user group.
        :returns: The id of the created user group.
        """
        cursor = self.db.cursor()
        cursor.execute('INSERT INTO UserGroup (name) VALUES (?)', [name])
        group_id = cursor.lastrowid
        assert group_id is not None, 'group_id was None'
        self.db.commit()
        return group_id

    @contract
    def addUserToGroup(self, group_id: 'int', user_id: 'int'):
        """Adds a user to a usergroup.
        
        :param group_id: The id of the group.
        :param user_id: The id of the user.
        """
        cursor = self.db.cursor()
        cursor.execute('INSERT INTO UserGroupMember (UserGroup_id, User_id) VALUES (?, ?)', [group_id, user_id])
        self.db.commit()

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
    def getUser(self, user_id: 'int') -> 'row':
        """Gets the user with the specified id.
        
        :param user_id:
        :returns: An sqlite3 row object representing the user. Columns: id, name.
        """

        cursor = self.db.cursor()
        cursor.execute('SELECT id, name FROM User WHERE id = ?', [user_id])
        return cursor.fetchone()

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
        cursor = self.db.cursor()
        cursor.execute("""SELECT id FROM User WHERE
                          (id = ? OR id = 0)
                          AND (User.id IN
                              (SELECT User_id FROM UserGroupMember WHERE UserGroup_id IN
                                  (SELECT UserGroup_id FROM BlockViewAccess WHERE Block_id = ?))
                              
                          OR  (User.id IN
                              (SELECT User_id FROM UserGroupMember WHERE UserGroup_id IN
                              (SELECT UserGroup_id FROM Block WHERE Block.id = ?))
                              ))""", [user_id, block_id, block_id])
        result = cursor.fetchall()
        assert len(result) <= 1, 'rowcount should be 1 at most'
        return len(result) == 1

    @contract
    def userHasEditAccess(self, user_id: 'int', block_id: 'int') -> 'bool':
        """Returns whether the user has edit access to the specified block.
        
        :param user_id:
        :param block_id:
        :returns: True if the user with id 'user_id' has view access to the block 'block_id', false otherwise.
        """

        if self.userIsOwner(user_id, block_id):
            return True
        # TODO: This method is pretty much copy-paste from userHasViewAccess. Should make some common method.
        cursor = self.db.cursor()
        cursor.execute("""SELECT id FROM User WHERE
                          (id = ? OR id = 0)
                          AND (User.id IN
                              (SELECT User_id FROM UserGroupMember WHERE UserGroup_id IN
                                  (SELECT UserGroup_id FROM BlockEditAccess WHERE Block_id = ?))
                              
                          OR  (User.id IN
                              (SELECT User_id FROM UserGroupMember WHERE UserGroup_id IN
                              (SELECT UserGroup_id FROM Block WHERE Block.id = ?))
                              ))""", [user_id, block_id, block_id])
        result = cursor.fetchall()
        return len(result) > 0

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
