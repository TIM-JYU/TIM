from datetime import timedelta

from timApp.auth.accesstype import AccessType
from timApp.item.block import insert_block, BlockType, Block
from timApp.tests.db.timdbtest import TimDbTest, TEST_USER_1_ID
from timApp.timdb.sqa import db
from timApp.user.user import User, last_name_to_first, last_name_to_last, UserInfo
from timApp.user.usergroup import UserGroup
from timApp.user.users import remove_access
from timApp.user.userutils import grant_access
from timApp.util.utils import get_current_time


class UserTest(TimDbTest):
    def grant(self, g: UserGroup, b: Block, t: AccessType, **kwargs):
        r = grant_access(g, b, t, **kwargs)
        db.session.commit()
        db.session.refresh(b)
        db.session.refresh(g)
        return r

    def remove(self, g, b: Block, t):
        r = remove_access(g, b, t)
        db.session.commit()
        db.session.refresh(b)
        db.session.refresh(g)
        return r

    def test_create_user(self):
        anon_group = UserGroup.get_anonymous_group()
        name, real_name, email, password = [
            "test",
            "John Doe",
            "john@example.com",
            "0123456789abcdef",
        ]
        user, g = User.create_with_group(
            UserInfo(username=name, full_name=real_name, email=email, password=password)
        )
        g2 = UserGroup.create("dummy")

        test_block = insert_block(
            block_type=BlockType.Document, description="test", owner_groups=[g2]
        )
        test_block_2 = insert_block(
            block_type=BlockType.Document, description="test", owner_groups=[g]
        )
        db.session.commit()

        self.assertEqual(user.name, name)
        self.assertEqual(user.real_name, real_name)
        self.assertEqual(user.email, email)
        g3 = UserGroup.create("dummy2")
        gid3 = g3
        self.assertNotEqual(
            gid3, anon_group
        )  # Should not be equal to anonymous usergroup id

        # Testing view access
        self.assertFalse(user.has_view_access(test_block))
        user.groups.append(g3)
        db.session.commit()
        self.grant(gid3, test_block, AccessType.view)
        self.assertTrue(user.has_view_access(test_block))
        self.assertFalse(user.has_edit_access(test_block))
        self.assertFalse(user.has_teacher_access(test_block))
        self.assertFalse(user.has_manage_access(test_block))
        self.assertFalse(user.has_seeanswers_access(test_block))
        self.remove(gid3, test_block, AccessType.view)
        self.assertFalse(user.has_view_access(test_block))
        self.grant(anon_group, test_block, AccessType.view)
        self.assertTrue(user.has_view_access(test_block))
        self.assertFalse(user.has_edit_access(test_block))
        self.assertFalse(user.has_teacher_access(test_block))
        self.assertFalse(user.has_manage_access(test_block))
        self.assertFalse(user.has_seeanswers_access(test_block))
        self.remove(anon_group, test_block, AccessType.view)
        self.assertFalse(user.has_view_access(test_block))

        # Testing edit access
        self.assertFalse(user.has_edit_access(test_block))
        self.grant(gid3, test_block, AccessType.edit)
        self.assertTrue(user.has_edit_access(test_block))
        self.assertTrue(user.has_view_access(test_block))
        self.assertFalse(user.has_teacher_access(test_block))
        self.assertFalse(user.has_manage_access(test_block))
        self.assertFalse(user.has_seeanswers_access(test_block))
        self.remove(gid3, test_block, AccessType.edit)
        self.assertFalse(user.has_edit_access(test_block))
        self.assertFalse(user.has_view_access(test_block))
        self.grant(anon_group, test_block, AccessType.edit)
        self.assertTrue(user.has_edit_access(test_block))
        self.assertTrue(user.has_view_access(test_block))
        self.assertFalse(user.has_teacher_access(test_block))
        self.assertFalse(user.has_manage_access(test_block))
        self.assertFalse(user.has_seeanswers_access(test_block))
        self.remove(anon_group, test_block, AccessType.edit)
        self.assertFalse(user.has_edit_access(test_block))
        self.assertFalse(user.has_view_access(test_block))

        # Testing manage access
        self.assertFalse(user.has_manage_access(test_block))
        self.grant(gid3, test_block, AccessType.manage)
        self.assertTrue(user.has_manage_access(test_block))
        self.assertTrue(user.has_edit_access(test_block))
        self.assertTrue(user.has_view_access(test_block))
        self.assertTrue(user.has_teacher_access(test_block))
        self.assertTrue(user.has_seeanswers_access(test_block))
        self.remove(gid3, test_block, AccessType.manage)
        self.assertFalse(user.has_manage_access(test_block))
        self.assertFalse(user.has_edit_access(test_block))
        self.assertFalse(user.has_view_access(test_block))
        self.assertFalse(user.has_teacher_access(test_block))
        self.assertFalse(user.has_seeanswers_access(test_block))
        self.grant(anon_group, test_block, AccessType.manage)
        self.assertTrue(user.has_manage_access(test_block))
        self.assertTrue(user.has_edit_access(test_block))
        self.assertTrue(user.has_view_access(test_block))
        self.assertTrue(user.has_teacher_access(test_block))
        self.assertTrue(user.has_seeanswers_access(test_block))
        self.remove(anon_group, test_block, AccessType.manage)
        self.assertFalse(user.has_manage_access(test_block))
        self.assertFalse(user.has_edit_access(test_block))
        self.assertFalse(user.has_view_access(test_block))
        self.assertFalse(user.has_teacher_access(test_block))
        self.assertFalse(user.has_seeanswers_access(test_block))

        # Testing teacher access
        self.assertFalse(user.has_manage_access(test_block))
        self.grant(gid3, test_block, AccessType.teacher)
        self.assertFalse(user.has_manage_access(test_block))
        self.assertFalse(user.has_edit_access(test_block))
        self.assertTrue(user.has_view_access(test_block))
        self.assertTrue(user.has_teacher_access(test_block))
        self.assertTrue(user.has_seeanswers_access(test_block))
        self.remove(gid3, test_block, AccessType.teacher)
        self.assertFalse(user.has_manage_access(test_block))
        self.assertFalse(user.has_edit_access(test_block))
        self.assertFalse(user.has_view_access(test_block))
        self.assertFalse(user.has_teacher_access(test_block))
        self.assertFalse(user.has_seeanswers_access(test_block))
        self.grant(anon_group, test_block, AccessType.teacher)
        self.assertFalse(user.has_manage_access(test_block))
        self.assertFalse(user.has_edit_access(test_block))
        self.assertTrue(user.has_view_access(test_block))
        self.assertTrue(user.has_teacher_access(test_block))
        self.assertTrue(user.has_seeanswers_access(test_block))
        self.remove(anon_group, test_block, AccessType.teacher)
        self.assertFalse(user.has_manage_access(test_block))
        self.assertFalse(user.has_edit_access(test_block))
        self.assertFalse(user.has_view_access(test_block))
        self.assertFalse(user.has_teacher_access(test_block))
        self.assertFalse(user.has_seeanswers_access(test_block))

        # Testing see answers access
        self.grant(gid3, test_block, AccessType.see_answers)
        self.assertFalse(user.has_manage_access(test_block))
        self.assertFalse(user.has_edit_access(test_block))
        self.assertTrue(user.has_view_access(test_block))
        self.assertFalse(user.has_teacher_access(test_block))
        self.assertTrue(user.has_seeanswers_access(test_block))
        self.remove(gid3, test_block, AccessType.see_answers)
        self.assertFalse(user.has_manage_access(test_block))
        self.assertFalse(user.has_edit_access(test_block))
        self.assertFalse(user.has_view_access(test_block))
        self.assertFalse(user.has_teacher_access(test_block))
        self.assertFalse(user.has_seeanswers_access(test_block))
        self.grant(anon_group, test_block, AccessType.see_answers)
        self.assertFalse(user.has_manage_access(test_block))
        self.assertFalse(user.has_edit_access(test_block))
        self.assertTrue(user.has_view_access(test_block))
        self.assertFalse(user.has_teacher_access(test_block))
        self.assertTrue(user.has_seeanswers_access(test_block))
        self.remove(anon_group, test_block, AccessType.see_answers)
        self.assertFalse(user.has_manage_access(test_block))
        self.assertFalse(user.has_edit_access(test_block))
        self.assertFalse(user.has_view_access(test_block))
        self.assertFalse(user.has_teacher_access(test_block))
        self.assertFalse(user.has_seeanswers_access(test_block))

        user.groups.append(UserGroup.get_admin_group())
        db.session.commit()
        user.__dict__.pop("is_admin", None)
        for b in (test_block, test_block_2):
            self.assertTrue(user.has_manage_access(b))
            self.assertTrue(user.has_edit_access(b))
            self.assertTrue(user.has_view_access(b))
            self.assertTrue(user.has_teacher_access(b))
            self.assertTrue(user.has_seeanswers_access(b))

    def test_timed_permissions(self):
        b = insert_block(
            BlockType.Document, "testing", [self.test_user_2.get_personal_group()]
        )
        user = db.session.get(User, TEST_USER_1_ID)
        self.assertFalse(user.has_view_access(b))
        v = AccessType.view

        pg1 = self.test_user_1.get_personal_group()
        self.grant(pg1, b, v, accessible_from=get_current_time() + timedelta(days=1))
        self.assertFalse(user.has_view_access(b))
        self.remove(pg1, b, v)

        self.grant(pg1, b, v, accessible_from=get_current_time() - timedelta(days=1))
        self.assertTrue(user.has_view_access(b))
        self.remove(pg1, b, v)

        self.grant(
            pg1,
            b,
            v,
            accessible_from=get_current_time() - timedelta(days=1),
            accessible_to=get_current_time() - timedelta(seconds=1),
        )
        self.assertFalse(user.has_view_access(b))
        self.remove(pg1, b, v)

        self.grant(pg1, b, v, duration=timedelta(days=1))
        self.assertFalse(user.has_view_access(b))
        self.remove(pg1, b, v)

    def test_last_name_switch(self):
        for fn1, fn2 in [
            (lambda x: x, last_name_to_first),
            (last_name_to_last, lambda x: x),
        ]:
            self.assertEqual(fn1("Doe John"), fn2("John Doe"))
            self.assertEqual(fn1("Doe John Matt"), fn2("John Matt Doe"))
            self.assertEqual(fn1("Doe John Matt Henry"), fn2("John Matt Henry Doe"))
            self.assertEqual(fn1("Someone"), fn2("Someone"))
            self.assertEqual(fn1(""), fn2(""))
            self.assertEqual(fn1(None), fn2(None))
