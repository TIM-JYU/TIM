import unittest
from datetime import timedelta

from timApp.item.block import insert_block, BlockType
from timApp.tests.db.timdbtest import TimDbTest, TEST_USER_1_ID
from timApp.timdb.sqa import db
from timApp.user.user import User
from timApp.user.usergroup import UserGroup
from timApp.user.userutils import get_anon_group_id
from timApp.user.userutils import grant_access
from timApp.util.utils import get_current_time


class UserTest(TimDbTest):
    def test_create_user(self):
        timdb = self.get_db()
        anonymous_usergroup_id = get_anon_group_id()
        name, real_name, email, password = ['test', 'John Doe', 'john@example.com', '0123456789abcdef']
        user = User.create(name, real_name, email, password)
        g = UserGroup.create(name)
        gid = g.id
        g2 = UserGroup.create('dummy')
        gid2 = g2.id
        user.groups.append(g)

        test_block = insert_block(block_type=BlockType.Document, description='test', owner_group_id=gid2)
        test_block_id = test_block.id
        test_block_2 = insert_block(block_type=BlockType.Document, description='test', owner_group_id=gid)
        db.session.commit()

        self.assertEqual(user.name, name)
        self.assertEqual(user.real_name, real_name)
        self.assertEqual(user.email, email)
        g3 = UserGroup.create('dummy2')
        gid3 = g3.id
        self.assertNotEqual(gid3, anonymous_usergroup_id)  # Should not be equal to anonymous usergroup id

        # Testing view access
        self.assertFalse(user.has_view_access(test_block))
        user.groups.append(g3)
        db.session.commit()
        va = grant_access(gid3, test_block_id, 'view')
        self.assertTrue(user.has_view_access(test_block))
        self.assertFalse(user.has_edit_access(test_block))
        self.assertFalse(user.has_teacher_access(test_block))
        self.assertFalse(user.has_manage_access(test_block))
        self.assertFalse(user.has_seeanswers_access(test_block))
        timdb.users.remove_access(gid3, test_block_id, 'view')
        db.session.commit()
        self.assertFalse(user.has_view_access(test_block))
        grant_access(anonymous_usergroup_id, test_block_id, 'view')
        self.assertTrue(user.has_view_access(test_block))
        self.assertFalse(user.has_edit_access(test_block))
        self.assertFalse(user.has_teacher_access(test_block))
        self.assertFalse(user.has_manage_access(test_block))
        self.assertFalse(user.has_seeanswers_access(test_block))
        timdb.users.remove_access(anonymous_usergroup_id, test_block_id, 'view')
        db.session.commit()
        self.assertFalse(user.has_view_access(test_block))

        # Testing edit access
        self.assertFalse(user.has_edit_access(test_block))
        grant_access(gid3, test_block_id, 'edit')
        self.assertTrue(user.has_edit_access(test_block))
        self.assertTrue(user.has_view_access(test_block))
        self.assertFalse(user.has_teacher_access(test_block))
        self.assertFalse(user.has_manage_access(test_block))
        self.assertFalse(user.has_seeanswers_access(test_block))
        timdb.users.remove_access(gid3, test_block_id, 'edit')
        db.session.commit()
        self.assertFalse(user.has_edit_access(test_block))
        self.assertFalse(user.has_view_access(test_block))
        grant_access(anonymous_usergroup_id, test_block_id, 'edit')
        self.assertTrue(user.has_edit_access(test_block))
        self.assertTrue(user.has_view_access(test_block))
        self.assertFalse(user.has_teacher_access(test_block))
        self.assertFalse(user.has_manage_access(test_block))
        self.assertFalse(user.has_seeanswers_access(test_block))
        timdb.users.remove_access(anonymous_usergroup_id, test_block_id, 'edit')
        db.session.commit()
        self.assertFalse(user.has_edit_access(test_block))
        self.assertFalse(user.has_view_access(test_block))

        # Testing manage access
        self.assertFalse(user.has_manage_access(test_block))
        grant_access(gid3, test_block_id, 'manage')
        self.assertTrue(user.has_manage_access(test_block))
        self.assertTrue(user.has_edit_access(test_block))
        self.assertTrue(user.has_view_access(test_block))
        self.assertTrue(user.has_teacher_access(test_block))
        self.assertTrue(user.has_seeanswers_access(test_block))
        timdb.users.remove_access(gid3, test_block_id, 'manage')
        db.session.commit()
        self.assertFalse(user.has_manage_access(test_block))
        self.assertFalse(user.has_edit_access(test_block))
        self.assertFalse(user.has_view_access(test_block))
        self.assertFalse(user.has_teacher_access(test_block))
        self.assertFalse(user.has_seeanswers_access(test_block))
        grant_access(anonymous_usergroup_id, test_block_id, 'manage')
        self.assertTrue(user.has_manage_access(test_block))
        self.assertTrue(user.has_edit_access(test_block))
        self.assertTrue(user.has_view_access(test_block))
        self.assertTrue(user.has_teacher_access(test_block))
        self.assertTrue(user.has_seeanswers_access(test_block))
        timdb.users.remove_access(anonymous_usergroup_id, test_block_id, 'manage')
        db.session.commit()
        self.assertFalse(user.has_manage_access(test_block))
        self.assertFalse(user.has_edit_access(test_block))
        self.assertFalse(user.has_view_access(test_block))
        self.assertFalse(user.has_teacher_access(test_block))
        self.assertFalse(user.has_seeanswers_access(test_block))

        # Testing teacher access
        self.assertFalse(user.has_manage_access(test_block))
        grant_access(gid3, test_block_id, 'teacher')
        self.assertFalse(user.has_manage_access(test_block))
        self.assertFalse(user.has_edit_access(test_block))
        self.assertTrue(user.has_view_access(test_block))
        self.assertTrue(user.has_teacher_access(test_block))
        self.assertTrue(user.has_seeanswers_access(test_block))
        timdb.users.remove_access(gid3, test_block_id, 'teacher')
        db.session.commit()
        self.assertFalse(user.has_manage_access(test_block))
        self.assertFalse(user.has_edit_access(test_block))
        self.assertFalse(user.has_view_access(test_block))
        self.assertFalse(user.has_teacher_access(test_block))
        self.assertFalse(user.has_seeanswers_access(test_block))
        grant_access(anonymous_usergroup_id, test_block_id, 'teacher')
        self.assertFalse(user.has_manage_access(test_block))
        self.assertFalse(user.has_edit_access(test_block))
        self.assertTrue(user.has_view_access(test_block))
        self.assertTrue(user.has_teacher_access(test_block))
        self.assertTrue(user.has_seeanswers_access(test_block))
        timdb.users.remove_access(anonymous_usergroup_id, test_block_id, 'teacher')
        db.session.commit()
        self.assertFalse(user.has_manage_access(test_block))
        self.assertFalse(user.has_edit_access(test_block))
        self.assertFalse(user.has_view_access(test_block))
        self.assertFalse(user.has_teacher_access(test_block))
        self.assertFalse(user.has_seeanswers_access(test_block))

        # Testing see answers access
        grant_access(gid3, test_block_id, 'see answers')
        self.assertFalse(user.has_manage_access(test_block))
        self.assertFalse(user.has_edit_access(test_block))
        self.assertTrue(user.has_view_access(test_block))
        self.assertFalse(user.has_teacher_access(test_block))
        self.assertTrue(user.has_seeanswers_access(test_block))
        timdb.users.remove_access(gid3, test_block_id, 'see answers')
        db.session.commit()
        self.assertFalse(user.has_manage_access(test_block))
        self.assertFalse(user.has_edit_access(test_block))
        self.assertFalse(user.has_view_access(test_block))
        self.assertFalse(user.has_teacher_access(test_block))
        self.assertFalse(user.has_seeanswers_access(test_block))
        grant_access(anonymous_usergroup_id, test_block_id, 'see answers')
        self.assertFalse(user.has_manage_access(test_block))
        self.assertFalse(user.has_edit_access(test_block))
        self.assertTrue(user.has_view_access(test_block))
        self.assertFalse(user.has_teacher_access(test_block))
        self.assertTrue(user.has_seeanswers_access(test_block))
        timdb.users.remove_access(anonymous_usergroup_id, test_block_id, 'see answers')
        db.session.commit()
        self.assertFalse(user.has_manage_access(test_block))
        self.assertFalse(user.has_edit_access(test_block))
        self.assertFalse(user.has_view_access(test_block))
        self.assertFalse(user.has_teacher_access(test_block))
        self.assertFalse(user.has_seeanswers_access(test_block))

        user.groups.append(UserGroup.get_admin_group())
        timdb.session.commit()
        del user.__dict__['is_admin']
        for b in (test_block, test_block_2):
            self.assertTrue(user.has_manage_access(b))
            self.assertTrue(user.has_edit_access(b))
            self.assertTrue(user.has_view_access(b))
            self.assertTrue(user.has_teacher_access(b))
            self.assertTrue(user.has_seeanswers_access(b))

    def test_timed_permissions(self):
        db = self.get_db()
        block = insert_block(BlockType.Document, 'testing', self.get_test_user_2_group_id())
        b = block.id
        user = User.query.get(TEST_USER_1_ID)
        self.assertFalse(user.has_view_access(block))
        v = 'view'

        grant_access(self.get_test_user_1_group_id(), b, v,
                     accessible_from=get_current_time() + timedelta(days=1))
        self.assertFalse(user.has_view_access(block))
        db.users.remove_access(self.get_test_user_1_group_id(), b, v)

        ba = grant_access(self.get_test_user_1_group_id(), b, v,
                          accessible_from=get_current_time() - timedelta(days=1))
        self.assertTrue(user.has_view_access(block))
        db.users.remove_access(self.get_test_user_1_group_id(), b, v)

        grant_access(self.get_test_user_1_group_id(), b, v,
                     accessible_from=get_current_time() - timedelta(days=1),
                     accessible_to=get_current_time() - timedelta(seconds=1))
        self.assertFalse(user.has_view_access(block))
        db.users.remove_access(self.get_test_user_1_group_id(), b, v)

        grant_access(self.get_test_user_1_group_id(), b, v,
                     duration=timedelta(days=1))
        self.assertFalse(user.has_view_access(block))
        db.users.remove_access(self.get_test_user_1_group_id(), b, v)


if __name__ == '__main__':
    unittest.main()
