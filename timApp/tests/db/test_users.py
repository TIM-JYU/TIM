import unittest
from datetime import timedelta

from timApp.item.block import insert_block, BlockType, Block
from timApp.tests.db.timdbtest import TimDbTest, TEST_USER_1_ID
from timApp.timdb.sqa import db
from timApp.user.user import User
from timApp.user.usergroup import UserGroup
from timApp.user.users import remove_access
from timApp.user.userutils import get_anon_group_id, grant_access
from timApp.util.utils import get_current_time


class UserTest(TimDbTest):
    def grant(self, g, b: Block, t, **kwargs):
        r = grant_access(g, b.id, t, **kwargs)
        db.session.refresh(b)
        return r

    def remove(self, g, b: Block, t):
        r = remove_access(g, b.id, t)
        db.session.commit()
        db.session.refresh(b)
        return r

    def test_create_user(self):
        anonymous_usergroup_id = get_anon_group_id()
        name, real_name, email, password = ['test', 'John Doe', 'john@example.com', '0123456789abcdef']
        user = User.create(name, real_name, email, password)
        g = UserGroup.create(name)
        gid = g.id
        g2 = UserGroup.create('dummy')
        gid2 = g2.id
        user.groups.append(g)

        test_block = insert_block(block_type=BlockType.Document, description='test', owner_group_id=gid2)
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
        self.grant(gid3, test_block, 'view')
        self.assertTrue(user.has_view_access(test_block))
        self.assertFalse(user.has_edit_access(test_block))
        self.assertFalse(user.has_teacher_access(test_block))
        self.assertFalse(user.has_manage_access(test_block))
        self.assertFalse(user.has_seeanswers_access(test_block))
        self.remove(gid3, test_block, 'view')
        self.assertFalse(user.has_view_access(test_block))
        self.grant(anonymous_usergroup_id, test_block, 'view')
        self.assertTrue(user.has_view_access(test_block))
        self.assertFalse(user.has_edit_access(test_block))
        self.assertFalse(user.has_teacher_access(test_block))
        self.assertFalse(user.has_manage_access(test_block))
        self.assertFalse(user.has_seeanswers_access(test_block))
        self.remove(anonymous_usergroup_id, test_block, 'view')
        self.assertFalse(user.has_view_access(test_block))

        # Testing edit access
        self.assertFalse(user.has_edit_access(test_block))
        self.grant(gid3, test_block, 'edit')
        self.assertTrue(user.has_edit_access(test_block))
        self.assertTrue(user.has_view_access(test_block))
        self.assertFalse(user.has_teacher_access(test_block))
        self.assertFalse(user.has_manage_access(test_block))
        self.assertFalse(user.has_seeanswers_access(test_block))
        self.remove(gid3, test_block, 'edit')
        self.assertFalse(user.has_edit_access(test_block))
        self.assertFalse(user.has_view_access(test_block))
        self.grant(anonymous_usergroup_id, test_block, 'edit')
        self.assertTrue(user.has_edit_access(test_block))
        self.assertTrue(user.has_view_access(test_block))
        self.assertFalse(user.has_teacher_access(test_block))
        self.assertFalse(user.has_manage_access(test_block))
        self.assertFalse(user.has_seeanswers_access(test_block))
        self.remove(anonymous_usergroup_id, test_block, 'edit')
        self.assertFalse(user.has_edit_access(test_block))
        self.assertFalse(user.has_view_access(test_block))

        # Testing manage access
        self.assertFalse(user.has_manage_access(test_block))
        self.grant(gid3, test_block, 'manage')
        self.assertTrue(user.has_manage_access(test_block))
        self.assertTrue(user.has_edit_access(test_block))
        self.assertTrue(user.has_view_access(test_block))
        self.assertTrue(user.has_teacher_access(test_block))
        self.assertTrue(user.has_seeanswers_access(test_block))
        self.remove(gid3, test_block, 'manage')
        self.assertFalse(user.has_manage_access(test_block))
        self.assertFalse(user.has_edit_access(test_block))
        self.assertFalse(user.has_view_access(test_block))
        self.assertFalse(user.has_teacher_access(test_block))
        self.assertFalse(user.has_seeanswers_access(test_block))
        self.grant(anonymous_usergroup_id, test_block, 'manage')
        self.assertTrue(user.has_manage_access(test_block))
        self.assertTrue(user.has_edit_access(test_block))
        self.assertTrue(user.has_view_access(test_block))
        self.assertTrue(user.has_teacher_access(test_block))
        self.assertTrue(user.has_seeanswers_access(test_block))
        self.remove(anonymous_usergroup_id, test_block, 'manage')
        self.assertFalse(user.has_manage_access(test_block))
        self.assertFalse(user.has_edit_access(test_block))
        self.assertFalse(user.has_view_access(test_block))
        self.assertFalse(user.has_teacher_access(test_block))
        self.assertFalse(user.has_seeanswers_access(test_block))

        # Testing teacher access
        self.assertFalse(user.has_manage_access(test_block))
        self.grant(gid3, test_block, 'teacher')
        self.assertFalse(user.has_manage_access(test_block))
        self.assertFalse(user.has_edit_access(test_block))
        self.assertTrue(user.has_view_access(test_block))
        self.assertTrue(user.has_teacher_access(test_block))
        self.assertTrue(user.has_seeanswers_access(test_block))
        self.remove(gid3, test_block, 'teacher')
        self.assertFalse(user.has_manage_access(test_block))
        self.assertFalse(user.has_edit_access(test_block))
        self.assertFalse(user.has_view_access(test_block))
        self.assertFalse(user.has_teacher_access(test_block))
        self.assertFalse(user.has_seeanswers_access(test_block))
        self.grant(anonymous_usergroup_id, test_block, 'teacher')
        self.assertFalse(user.has_manage_access(test_block))
        self.assertFalse(user.has_edit_access(test_block))
        self.assertTrue(user.has_view_access(test_block))
        self.assertTrue(user.has_teacher_access(test_block))
        self.assertTrue(user.has_seeanswers_access(test_block))
        self.remove(anonymous_usergroup_id, test_block, 'teacher')
        self.assertFalse(user.has_manage_access(test_block))
        self.assertFalse(user.has_edit_access(test_block))
        self.assertFalse(user.has_view_access(test_block))
        self.assertFalse(user.has_teacher_access(test_block))
        self.assertFalse(user.has_seeanswers_access(test_block))

        # Testing see answers access
        self.grant(gid3, test_block, 'see answers')
        self.assertFalse(user.has_manage_access(test_block))
        self.assertFalse(user.has_edit_access(test_block))
        self.assertTrue(user.has_view_access(test_block))
        self.assertFalse(user.has_teacher_access(test_block))
        self.assertTrue(user.has_seeanswers_access(test_block))
        self.remove(gid3, test_block, 'see answers')
        self.assertFalse(user.has_manage_access(test_block))
        self.assertFalse(user.has_edit_access(test_block))
        self.assertFalse(user.has_view_access(test_block))
        self.assertFalse(user.has_teacher_access(test_block))
        self.assertFalse(user.has_seeanswers_access(test_block))
        self.grant(anonymous_usergroup_id, test_block, 'see answers')
        self.assertFalse(user.has_manage_access(test_block))
        self.assertFalse(user.has_edit_access(test_block))
        self.assertTrue(user.has_view_access(test_block))
        self.assertFalse(user.has_teacher_access(test_block))
        self.assertTrue(user.has_seeanswers_access(test_block))
        self.remove(anonymous_usergroup_id, test_block, 'see answers')
        self.assertFalse(user.has_manage_access(test_block))
        self.assertFalse(user.has_edit_access(test_block))
        self.assertFalse(user.has_view_access(test_block))
        self.assertFalse(user.has_teacher_access(test_block))
        self.assertFalse(user.has_seeanswers_access(test_block))

        user.groups.append(UserGroup.get_admin_group())
        db.session.commit()
        del user.__dict__['is_admin']
        for b in (test_block, test_block_2):
            self.assertTrue(user.has_manage_access(b))
            self.assertTrue(user.has_edit_access(b))
            self.assertTrue(user.has_view_access(b))
            self.assertTrue(user.has_teacher_access(b))
            self.assertTrue(user.has_seeanswers_access(b))

    def test_timed_permissions(self):
        b = insert_block(BlockType.Document, 'testing', self.get_test_user_2_group_id())
        user = User.query.get(TEST_USER_1_ID)
        self.assertFalse(user.has_view_access(b))
        v = 'view'

        self.grant(self.get_test_user_1_group_id(), b, v,
                   accessible_from=get_current_time() + timedelta(days=1))
        self.assertFalse(user.has_view_access(b))
        self.remove(self.get_test_user_1_group_id(), b, v)

        self.grant(self.get_test_user_1_group_id(), b, v,
                   accessible_from=get_current_time() - timedelta(days=1))
        self.assertTrue(user.has_view_access(b))
        self.remove(self.get_test_user_1_group_id(), b, v)

        self.grant(self.get_test_user_1_group_id(), b, v,
                   accessible_from=get_current_time() - timedelta(days=1),
                   accessible_to=get_current_time() - timedelta(seconds=1))
        self.assertFalse(user.has_view_access(b))
        self.remove(self.get_test_user_1_group_id(), b, v)

        self.grant(self.get_test_user_1_group_id(), b, v,
                   duration=timedelta(days=1))
        self.assertFalse(user.has_view_access(b))
        self.remove(self.get_test_user_1_group_id(), b, v)


if __name__ == '__main__':
    unittest.main()
