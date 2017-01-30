import unittest
from datetime import datetime, timezone, timedelta

from tests.db.timdbtest import TimDbTest, TEST_USER_1_ID
from timdb.dbutils import insert_block
from timdb.tim_models import BlockAccess


class UserTest(TimDbTest):

    def test_create_user(self):
        db = self.get_db()
        anonymous_usergroup_id = db.users.get_anon_group_id()
        name, real_name, email, password_hash = ['test', 'John Doe', 'john@example.com', '0123456789abcdef']
        user_id = db.users.create_user(name, real_name, email, password_hash).id
        gid = db.users.create_usergroup(name).id
        gid2 = db.users.create_usergroup('dummy').id
        db.users.add_user_to_group(gid, user_id)
        test_block = insert_block(description='test', owner_group_id=gid2, block_type=0)
        test_block_id = test_block.id
        test_block_2 = insert_block(description='test', owner_group_id=gid, block_type=0)
        test_block_id2 = test_block_2.id

        saved_user = db.users.get_user(user_id)
        self.assertEqual(saved_user['name'], name)
        self.assertEqual(saved_user['real_name'], real_name)
        self.assertEqual(saved_user['email'], email)
        group_id = db.users.create_usergroup('dummy2').id
        self.assertNotEqual(group_id, anonymous_usergroup_id)  # Should not be equal to anonymous usergroup id

        # Testing view access
        self.assertFalse(db.users.has_view_access(user_id, test_block_id))
        db.users.add_user_to_group(group_id, user_id)
        db.users.grant_access(group_id, test_block_id, 'view')
        self.assertTrue(db.users.has_view_access(user_id, test_block_id))
        self.assertFalse(db.users.has_edit_access(user_id, test_block_id))
        self.assertFalse(db.users.has_teacher_access(user_id, test_block_id))
        self.assertFalse(db.users.has_manage_access(user_id, test_block_id))
        self.assertFalse(db.users.has_seeanswers_access(user_id, test_block_id))
        db.users.remove_access(group_id, test_block_id, 'view')
        self.assertFalse(db.users.has_view_access(user_id, test_block_id))
        db.users.grant_access(anonymous_usergroup_id, test_block_id, 'view')
        self.assertTrue(db.users.has_view_access(user_id, test_block_id))
        self.assertFalse(db.users.has_edit_access(user_id, test_block_id))
        self.assertFalse(db.users.has_teacher_access(user_id, test_block_id))
        self.assertFalse(db.users.has_manage_access(user_id, test_block_id))
        self.assertFalse(db.users.has_seeanswers_access(user_id, test_block_id))
        db.users.remove_access(anonymous_usergroup_id, test_block_id, 'view')
        self.assertFalse(db.users.has_view_access(user_id, test_block_id))

        # Testing edit access
        self.assertFalse(db.users.has_edit_access(user_id, test_block_id))
        db.users.grant_access(group_id, test_block_id, 'edit')
        self.assertTrue(db.users.has_edit_access(user_id, test_block_id))
        self.assertTrue(db.users.has_view_access(user_id, test_block_id))
        self.assertFalse(db.users.has_teacher_access(user_id, test_block_id))
        self.assertFalse(db.users.has_manage_access(user_id, test_block_id))
        self.assertFalse(db.users.has_seeanswers_access(user_id, test_block_id))
        db.users.remove_access(group_id, test_block_id, 'edit')
        self.assertFalse(db.users.has_edit_access(user_id, test_block_id))
        self.assertFalse(db.users.has_view_access(user_id, test_block_id))
        db.users.grant_access(anonymous_usergroup_id, test_block_id, 'edit')
        self.assertTrue(db.users.has_edit_access(user_id, test_block_id))
        self.assertTrue(db.users.has_view_access(user_id, test_block_id))
        self.assertFalse(db.users.has_teacher_access(user_id, test_block_id))
        self.assertFalse(db.users.has_manage_access(user_id, test_block_id))
        self.assertFalse(db.users.has_seeanswers_access(user_id, test_block_id))
        db.users.remove_access(anonymous_usergroup_id, test_block_id, 'edit')
        self.assertFalse(db.users.has_edit_access(user_id, test_block_id))
        self.assertFalse(db.users.has_view_access(user_id, test_block_id))

        # Testing manage access
        self.assertFalse(db.users.has_manage_access(user_id, test_block_id))
        db.users.grant_access(group_id, test_block_id, 'manage')
        self.assertTrue(db.users.has_manage_access(user_id, test_block_id))
        self.assertTrue(db.users.has_edit_access(user_id, test_block_id))
        self.assertTrue(db.users.has_view_access(user_id, test_block_id))
        self.assertTrue(db.users.has_teacher_access(user_id, test_block_id))
        self.assertTrue(db.users.has_seeanswers_access(user_id, test_block_id))
        db.users.remove_access(group_id, test_block_id, 'manage')
        self.assertFalse(db.users.has_manage_access(user_id, test_block_id))
        self.assertFalse(db.users.has_edit_access(user_id, test_block_id))
        self.assertFalse(db.users.has_view_access(user_id, test_block_id))
        self.assertFalse(db.users.has_teacher_access(user_id, test_block_id))
        self.assertFalse(db.users.has_seeanswers_access(user_id, test_block_id))
        db.users.grant_access(anonymous_usergroup_id, test_block_id, 'manage')
        self.assertTrue(db.users.has_manage_access(user_id, test_block_id))
        self.assertTrue(db.users.has_edit_access(user_id, test_block_id))
        self.assertTrue(db.users.has_view_access(user_id, test_block_id))
        self.assertTrue(db.users.has_teacher_access(user_id, test_block_id))
        self.assertTrue(db.users.has_seeanswers_access(user_id, test_block_id))
        db.users.remove_access(anonymous_usergroup_id, test_block_id, 'manage')
        self.assertFalse(db.users.has_manage_access(user_id, test_block_id))
        self.assertFalse(db.users.has_edit_access(user_id, test_block_id))
        self.assertFalse(db.users.has_view_access(user_id, test_block_id))
        self.assertFalse(db.users.has_teacher_access(user_id, test_block_id))
        self.assertFalse(db.users.has_seeanswers_access(user_id, test_block_id))

        # Testing teacher access
        self.assertFalse(db.users.has_manage_access(user_id, test_block_id))
        db.users.grant_access(group_id, test_block_id, 'teacher')
        self.assertFalse(db.users.has_manage_access(user_id, test_block_id))
        self.assertFalse(db.users.has_edit_access(user_id, test_block_id))
        self.assertTrue(db.users.has_view_access(user_id, test_block_id))
        self.assertTrue(db.users.has_teacher_access(user_id, test_block_id))
        self.assertTrue(db.users.has_seeanswers_access(user_id, test_block_id))
        db.users.remove_access(group_id, test_block_id, 'teacher')
        self.assertFalse(db.users.has_manage_access(user_id, test_block_id))
        self.assertFalse(db.users.has_edit_access(user_id, test_block_id))
        self.assertFalse(db.users.has_view_access(user_id, test_block_id))
        self.assertFalse(db.users.has_teacher_access(user_id, test_block_id))
        self.assertFalse(db.users.has_seeanswers_access(user_id, test_block_id))
        db.users.grant_access(anonymous_usergroup_id, test_block_id, 'teacher')
        self.assertFalse(db.users.has_manage_access(user_id, test_block_id))
        self.assertFalse(db.users.has_edit_access(user_id, test_block_id))
        self.assertTrue(db.users.has_view_access(user_id, test_block_id))
        self.assertTrue(db.users.has_teacher_access(user_id, test_block_id))
        self.assertTrue(db.users.has_seeanswers_access(user_id, test_block_id))
        db.users.remove_access(anonymous_usergroup_id, test_block_id, 'teacher')
        self.assertFalse(db.users.has_manage_access(user_id, test_block_id))
        self.assertFalse(db.users.has_edit_access(user_id, test_block_id))
        self.assertFalse(db.users.has_view_access(user_id, test_block_id))
        self.assertFalse(db.users.has_teacher_access(user_id, test_block_id))
        self.assertFalse(db.users.has_seeanswers_access(user_id, test_block_id))

        # Testing see answers access
        db.users.grant_access(group_id, test_block_id, 'see answers')
        self.assertFalse(db.users.has_manage_access(user_id, test_block_id))
        self.assertFalse(db.users.has_edit_access(user_id, test_block_id))
        self.assertTrue(db.users.has_view_access(user_id, test_block_id))
        self.assertFalse(db.users.has_teacher_access(user_id, test_block_id))
        self.assertTrue(db.users.has_seeanswers_access(user_id, test_block_id))
        db.users.remove_access(group_id, test_block_id, 'see answers')
        self.assertFalse(db.users.has_manage_access(user_id, test_block_id))
        self.assertFalse(db.users.has_edit_access(user_id, test_block_id))
        self.assertFalse(db.users.has_view_access(user_id, test_block_id))
        self.assertFalse(db.users.has_teacher_access(user_id, test_block_id))
        self.assertFalse(db.users.has_seeanswers_access(user_id, test_block_id))
        db.users.grant_access(anonymous_usergroup_id, test_block_id, 'see answers')
        self.assertFalse(db.users.has_manage_access(user_id, test_block_id))
        self.assertFalse(db.users.has_edit_access(user_id, test_block_id))
        self.assertTrue(db.users.has_view_access(user_id, test_block_id))
        self.assertFalse(db.users.has_teacher_access(user_id, test_block_id))
        self.assertTrue(db.users.has_seeanswers_access(user_id, test_block_id))
        db.users.remove_access(anonymous_usergroup_id, test_block_id, 'see answers')
        self.assertFalse(db.users.has_manage_access(user_id, test_block_id))
        self.assertFalse(db.users.has_edit_access(user_id, test_block_id))
        self.assertFalse(db.users.has_view_access(user_id, test_block_id))
        self.assertFalse(db.users.has_teacher_access(user_id, test_block_id))
        self.assertFalse(db.users.has_seeanswers_access(user_id, test_block_id))

        self.assertDictEqual({2: test_block_2.owner_access}, db.users.get_viewable_blocks(user_id))
        db.users.addUserToAdmins(user_id)
        self.assertDictEqual({1: test_block.owner_access,
                              2: test_block_2.owner_access}, db.users.get_viewable_blocks(user_id))
        for bid in (test_block_id, test_block_id2):
            self.assertTrue(db.users.has_manage_access(user_id, bid))
            self.assertTrue(db.users.has_edit_access(user_id, bid))
            self.assertTrue(db.users.has_view_access(user_id, bid))
            self.assertTrue(db.users.has_teacher_access(user_id, bid))
            self.assertTrue(db.users.has_seeanswers_access(user_id, bid))

    def test_timed_permissions(self):
        db = self.get_db()
        block = insert_block('testing', self.get_test_user_2_group_id(), 0)
        b = block.id
        self.assertFalse(db.users.has_view_access(TEST_USER_1_ID, b))
        self.assertDictEqual({}, db.users.get_accessible_blocks(TEST_USER_1_ID, [db.users.get_view_access_id()]))
        v = 'view'

        db.users.grant_access(self.get_test_user_1_group_id(), b, v,
                              accessible_from=datetime.now(tz=timezone.utc) + timedelta(days=1))
        self.assertFalse(db.users.has_view_access(TEST_USER_1_ID, b))
        self.assertDictEqual({}, db.users.get_accessible_blocks(TEST_USER_1_ID, [db.users.get_view_access_id()]))
        db.users.remove_access(self.get_test_user_1_group_id(), b, v)

        ba = db.users.grant_access(self.get_test_user_1_group_id(), b, v,
                                   accessible_from=datetime.now(tz=timezone.utc) - timedelta(days=1))
        self.assertTrue(db.users.has_view_access(TEST_USER_1_ID, b))
        self.assertDictEqual({3: ba},
                             db.users.get_accessible_blocks(TEST_USER_1_ID, [db.users.get_view_access_id()]))
        db.users.remove_access(self.get_test_user_1_group_id(), b, v)

        db.users.grant_access(self.get_test_user_1_group_id(), b, v,
                              accessible_from=datetime.now(tz=timezone.utc) - timedelta(days=1),
                              accessible_to=datetime.now(tz=timezone.utc) - timedelta(seconds=1))
        self.assertFalse(db.users.has_view_access(TEST_USER_1_ID, b))
        self.assertDictEqual({}, db.users.get_accessible_blocks(TEST_USER_1_ID, [db.users.get_view_access_id()]))
        db.users.remove_access(self.get_test_user_1_group_id(), b, v)

        db.users.grant_access(self.get_test_user_1_group_id(), b, v,
                              duration=timedelta(days=1))
        self.assertFalse(db.users.has_view_access(TEST_USER_1_ID, b))
        self.assertDictEqual({}, db.users.get_accessible_blocks(TEST_USER_1_ID, [db.users.get_view_access_id()]))
        db.users.remove_access(self.get_test_user_1_group_id(), b, v)


if __name__ == '__main__':
    unittest.main()
