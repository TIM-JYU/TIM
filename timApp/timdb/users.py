
from contracts import contract, new_contract
from timdb.timdbbase import TimDbBase
import sqlite3

new_contract('row', sqlite3.Row)

class Users(TimDbBase):
    """Handles saving and retrieving user-related information to/from the database."""
    
    @contract
    def __init__(self, db_path : 'Connection', files_root_path : 'str'):
        """Initializes TimDB with the specified database and root path.
        
        :param db_path: The path of the database file.
        :param files_root_path: The root path where all the files will be stored.
        """
        TimDbBase.__init__(self, db_path, files_root_path)
    
    @contract
    def createAnonymousUser(self) -> 'None':
        """Creates an anonymous user and a usergroup for it."""
        
        cursor = self.db.cursor()
        cursor.execute('insert into User (id, name) values (?, ?)', [0, 'Anonymous'])
        cursor.execute('insert into UserGroup (id, name) values (?, ?)', [0, 'Anonymous group'])
        cursor.execute('insert into UserGroupMember (User_id, UserGroup_id) values (?, ?)', [0, 0])
        self.db.commit()
    
    @contract
    def createUser(self, name : 'str') -> 'int':
        """Creates a new user with the specified name.
        
        :param name: The name of the user to be created.
        :returns: The id of the newly created user.
        """
        cursor = self.db.cursor()
        cursor.execute('insert into User (name) values (?)', [name])
        self.db.commit()
        user_id = cursor.lastrowid
        return user_id

    @contract
    def createUserGroup(self, name : 'str') -> 'int':
        """Creates a new user group.
        
        :returns: The id of the created user group.
        """
        cursor = self.db.cursor()
        cursor.execute('insert into UserGroup (name) values (?)', [name])
        group_id = cursor.lastrowid
        assert group_id is not None, 'group_id was None'
        self.db.commit()
        return group_id
    
    @contract
    def addUserToGroup(self, group_id : 'int', user_id : 'int'):
        """Adds a user to a usergroup.
        
        :param group_id: The id of the group.
        :param user_id: The id of the user.
        """
        cursor = self.db.cursor()
        cursor.execute('insert into UserGroupMember (UserGroup_id, User_id) values (?, ?)', [group_id, user_id])
        self.db.commit()
        
    @contract
    def getUser(self, user_id : 'int') -> 'row':
        """Gets the user with the specified id.
        
        :returns: An sqlite3 row object representing the user. Columns: id, name.
        """
        
        cursor = self.db.cursor()
        cursor.execute('select id, name from User where id = ?', [user_id])
        return cursor.fetchone()
    
    @contract
    def getUserByName(self, name : 'str') -> 'int|None':
        """Gets the id of the specified username.
        
        :param name: The name of the user.
        :returns: The id of the user or None if the user does not exist.
        """
        
        cursor = self.db.cursor()
        cursor.execute('select id from User where name = ?', [name])
        result = cursor.fetchone()
        return result[0] if result is not None else None
    
    @contract
    def getUserGroups(self, user_id : 'int') -> 'list(dict)':
        """Gets the user groups of a user.
        
        :param user_id: The id of the user.
        :returns: The user groups that the user belongs to.
        """
        
        cursor = self.db.cursor()
        cursor.execute("""select id, name from UserGroup where id in (select UserGroup_id from UserGroupMember where User_id = ?)""", [user_id])
        return self.resultAsDictionary(cursor)
    
    def __grantAccess(self, group_id : 'int', block_id : 'int', access_type : 'str'):
        """Grants access to a group for a block.
        
        :param group_id: The group id to which to grant view access.
        :param block_id: The id of the block for which to grant view access.
        :param access_type: The kind of access. Possible values are 'edit' and 'view'.
        """

        #TODO: Check that the group_id and block_id exist.
        assert access_type in ['view', 'edit'], 'invalid value for access_type'
        cursor = self.db.cursor()
        cursor.execute('insert into %s (Block_id,UserGroup_id,visible_from) values (?,?,date(\'now\'))'
                       % 'BlockViewAccess' if access_type == 'view' else 'BlockEditAccess', [block_id, group_id])
        self.db.commit()
        
    @contract
    def grantViewAccess(self, group_id : 'int', block_id : 'int'):
        """Grants view access to a group for a block.
        
        :param group_id: The group id to which to grant view access.
        :param block_id: The id of the block for which to grant view access.
        """

        self.__grantAccess(group_id, block_id, 'view')

    @contract
    def grantEditAccess(self, group_id : 'int', block_id : 'int'):
        """Grants edit access to a group for a block.
        
        :param group_id: The group id to which to grant view access.
        :param block_id: The id of the block for which to grant view access.
        """

        self.__grantAccess(group_id, block_id, 'edit')

    @contract
    def userHasViewAccess(self, user_id : 'int', block_id : 'int') -> 'bool':
        """Returns whether the user has view access to the specified block.
        
        :returns: True if the user with id 'user_id' has view access to the block 'block_id', false otherwise.
        """
        
        cursor = self.db.cursor()
        cursor.execute("""select id from User where
                          id = ?
                          and (User.id in 
                              (select User_id from UserGroupMember where UserGroup_id in
                                  (select UserGroup_id from BlockViewAccess where Block_id = ?))
                              
                          or  (User.id in 
                              (select User_id from UserGroupMember where UserGroup_id in
                              (select UserGroup_id from Block where Block.id = ?))
                              ))""", [user_id, block_id, block_id])
        result = cursor.fetchall()
        assert len(result) <= 1, 'rowcount should be 1 at most'
        return len(result) == 1
    
    @contract
    def userHasEditAccess(self, user_id : 'int', block_id : 'int') -> 'bool':
        """Returns whether the user has edit access to the specified block.
        
        :returns: True if the user with id 'user_id' has view access to the block 'block_id', false otherwise.
        """
        
        #TODO: This method is pretty much copy-paste from userHasViewAccess. Should make some common method.
        cursor = self.db.cursor()
        cursor.execute("""select id from User where
                          id = ?
                          and (User.id in 
                              (select User_id from UserGroupMember where UserGroup_id in
                                  (select UserGroup_id from BlockEditAccess where Block_id = ?))
                              
                          or  (User.id in 
                              (select User_id from UserGroupMember where UserGroup_id in
                              (select UserGroup_id from Block where Block.id = ?))
                              ))""", [user_id, block_id, block_id])
        assert cursor.rowcount <= 1, 'rowcount should be 1 at most'
        return cursor.rowcount == 1
    
    @contract
    def userIsOwner(self, user_id : 'int', block_id : 'int') -> 'bool':
        """Returns whether the user belongs to the owners of the specified block.
        
        :returns: True if the user with 'user_id' belongs to the owner group of the block 'block_id'.
        """
        cursor = self.db.cursor()
        cursor.execute("""select id from User where
                          id = ?
                          and (id in
                              (select User_id from UserGroup where id in
                              (select UserGroup_id from Block where Block_id = ?))
                              )""", [user_id, block_id])
        assert cursor.rowcount <= 1, 'rowcount should be 1 at most'
        return cursor.rowcount == 1
