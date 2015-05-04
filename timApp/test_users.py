import unittest
from timdbtest import TimDbTest


class UserTest(TimDbTest):
    def test_create_user(self):
        test_block_id = 4
        anonymous_usergroup_id = 2
        name, real_name, email, password_hash = ['test', 'John Doe', 'john@example.com', '0123456789abcdef']
        user_id = self.db.users.createUser(name, real_name, email, password_hash)
        saved_user = self.db.users.getUser(user_id)
        self.assertEqual(saved_user['name'], name)
        self.assertEqual(saved_user['real_name'], real_name)
        self.assertEqual(saved_user['email'], email)
        group_id = self.db.users.createUserGroup('test group')
        self.assertNotEqual(group_id, anonymous_usergroup_id)  # Should not be equal to anonymous usergroup id

        # Testing view access
        self.assertFalse(self.db.users.userHasViewAccess(user_id, test_block_id))
        self.db.users.addUserToGroup(group_id, user_id)
        self.db.users.grantViewAccess(group_id, test_block_id)
        self.assertTrue(self.db.users.userHasViewAccess(user_id, test_block_id))
        self.db.users.removeViewAccess(group_id, test_block_id)
        self.assertFalse(self.db.users.userHasViewAccess(user_id, test_block_id))
        self.db.users.grantViewAccess(anonymous_usergroup_id, test_block_id)  # Anonymous usergroup
        self.assertTrue(self.db.users.userHasViewAccess(user_id, test_block_id))

        # Testing edit access
        self.assertFalse(self.db.users.userHasEditAccess(user_id, test_block_id))
        self.db.users.grantEditAccess(group_id, test_block_id)
        self.assertTrue(self.db.users.userHasEditAccess(user_id, test_block_id))
        self.db.users.removeEditAccess(group_id, test_block_id)
        self.assertFalse(self.db.users.userHasEditAccess(user_id, test_block_id))
        self.db.users.grantEditAccess(anonymous_usergroup_id, test_block_id)  # Anonymous usergroup
        self.assertTrue(self.db.users.userHasEditAccess(user_id, test_block_id))

if __name__ == '__main__':
    unittest.main()
