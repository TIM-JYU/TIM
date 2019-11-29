from datetime import timedelta

from timApp.auth.accesstype import AccessType
from timApp.document.docentry import DocEntry
from timApp.item.item import Item
from timApp.tests.server.test_default_rights import convert_to_old_format
from timApp.tests.server.timroutetest import TimRouteTest
from timApp.timdb.sqa import db
from timApp.user.usergroup import UserGroup
from timApp.user.userutils import grant_access
from timApp.util.utils import get_current_time


class PermissionTest(TimRouteTest):

    def test_cannot_remove_ownership(self):
        self.login_test1()
        d = self.create_doc()
        self.assertTrue(self.current_user.has_ownership(d))
        self.json_put(
            f'/permissions/add',
            {
                'time': {
                    'from': get_current_time() + timedelta(days=1),
                    'type': 'range',
                },
                'id': d.id,
                'type': 'owner',
                'groups': ['testuser1'],
                'confirm': False,
            },
            expect_status=403, expect_content={'error': 'You cannot remove ownership from yourself.'})
        db.session.remove()
        d = DocEntry.find_by_id(d.id)
        self.assertTrue(self.current_user.has_ownership(d))
        self.json_put(f'/permissions/remove',
                      {
                          'id': d.id,
                          'type': 'owner',
                          'group': self.get_test_user_1_group_id(),
                      },
                      expect_status=403)
        db.session.remove()
        d = DocEntry.find_by_id(d.id)
        self.assertTrue(self.current_user.has_ownership(d))

    def test_non_owner_cannot_change_owner(self):
        self.login_test1()
        d = self.create_doc()
        docid = d.id
        self.test_user_2.grant_access(d, AccessType.manage)
        self.login_test2()
        self.json_put(
            f'/permissions/add',
            {
                'time': {
                    'from': get_current_time(),
                    'type': 'always',
                },
                'id': docid,
                'type': 'owner',
                'groups': ['testuser2'],
                'confirm': False,
            },
            expect_status=403)

    def test_cannot_change_owner_of_personal_folder(self):
        self.login_test1()
        f = self.current_user.get_personal_folder()
        self.json_put(
            f'/permissions/add',
            {
                'id': f.id,
                'type': 'owner',
                'groups': ['testuser2'],
                'time': {
                    'type': 'always',
                },
                'confirm': False,
            }, expect_status=403,
            expect_content={'error': 'You cannot add owners to your personal folder.'},
        )

    def test_trim_whitespace(self):
        self.login_test1()
        f = self.current_user.get_personal_folder()
        self.json_put(
            f'/permissions/add',
            {
                'time': {
                    'from': get_current_time(),
                    'type': 'always',
                },
                'id': f.id,
                'type': 'view',
                'groups': ['testuser2', 'testuser3'],
                'confirm': False,
            })
        f = self.current_user.get_personal_folder()
        self.assertTrue(self.test_user_2.has_view_access(f))
        self.assertTrue(self.test_user_3.has_view_access(f))

    def test_nonexistent_user(self):
        self.login_test1()
        f = self.create_doc()
        self.json_put(
            f'/permissions/add',
            {
                'time': {
                    'from': get_current_time(),
                    'type': 'always',
                },
                'id': f.id,
                'type': 'view',
                'groups': ['testuser2', 'testuserx'],
                'confirm': False,
            }, expect_content={'not_exist': ['testuserx']})

    def test_permissions_get(self):
        self.login_test1()
        for i in (self.current_user.get_personal_folder(), self.create_doc()):
            self.json_put(
                f'/permissions/add',
                {
                    'time': {
                        'from': get_current_time(),
                        'type': 'always',
                    },
                    'id': i.id,
                    'type': 'view',
                    'groups': ['testuser2'],
                    'confirm': False,
                })
            rights = self.get(f'/permissions/get/{i.id}')
            i = Item.find_by_id(i.id)
            self.assertEqual(rights['accesstypes'],
                             [
                                 {'id': 1, 'name': 'view'},
                                 {'id': 2, 'name': 'edit'},
                                 {'id': 3, 'name': 'teacher'},
                                 {'id': 4, 'name': 'manage'},
                                 {'id': 5, 'name': 'see answers'},
                                 {'id': 6, 'name': 'owner'},
                                 {'id': 7, 'name': 'copy'},
                             ])
            self.assertEqual(convert_to_old_format(rights['grouprights']),
                             [{'access_name': 'owner',
                               'access_type': 6,
                               'accessible_from': i.block.accesses[
                                   0].accessible_from.isoformat(),
                               'accessible_to': None,
                               'duration': None,
                               'duration_from': None,
                               'duration_to': None,
                               'fullname': 'Test user 1',
                               'gid': self.get_test_user_1_group_id(),
                               'name': 'testuser1'},
                              {'access_name': 'view',
                               'access_type': 1,
                               'accessible_from': str(
                                   i.block.accesses[1].accessible_from.isoformat()),
                               'accessible_to': None,
                               'duration': None,
                               'duration_from': None,
                               'duration_to': None,
                               'fullname': 'Test user 2',
                               'gid': self.get_test_user_2_group_id(),
                               'name': 'testuser2'}, ])

    def test_logged_in_right(self):
        self.login_test1()
        d = self.create_doc()
        self.json_put(
            f'/permissions/add',
            {
                'time': {
                    'from': get_current_time(),
                    'type': 'always',
                },
                'id': d.id,
                'type': 'view',
                'groups': ['Logged-in users'],
                'confirm': False,
            })
        self.login_test2()
        self.get(d.url)
        self.login_test1()
        self.json_put(f'/permissions/remove', {
            'id': d.id,
            'type': 'view',
            'group': UserGroup.get_logged_in_group().id,
        })
        self.login_test2()
        self.get(d.url, expect_status=403)

    def test_recursive_permissions(self):
        self.login_test1()
        paths = ['a', 'b', 'c', 'x/a', 'x/b', 'x/c', 'x/x/a', 'x/x/b', 'x/x/c', ]
        ds = []
        for p in paths:
            d = self.create_doc(self.get_personal_item_path(p))
            ds.append(d)
            t1_f = self.test_user_1.get_personal_folder()
            self.assertFalse(self.test_user_2.has_view_access(d))
            self.assertFalse(self.test_user_2.has_edit_access(d))
            self.assertFalse(self.test_user_2.has_edit_access(t1_f))

        all_ids = [x.id for x in ds]
        self.json_put(
            f"/permissions/edit",
            {
                'groups': ['testuser1'],
                'type': 'owner',
                'action': 'remove',
                'ids': all_ids,
                'time': {
                    'type': 'always',
                },
                'confirm': False,
            },
            expect_status=403,
        )

        self.json_put(
            f"/permissions/edit",
            {
                'groups': ['testuser2', 'testuser3'],
                'type': 'view',
                'action': 'add',
                'ids': all_ids,
                'time': {
                    'type': 'always',
                },
                'confirm': False,
            },
        )
        t1_f = self.test_user_1.get_personal_folder()
        for p in paths:
            d = DocEntry.find_by_path(self.get_personal_item_path(p))
            self.assertTrue(self.test_user_2.has_view_access(d))
            self.assertTrue(self.test_user_3.has_view_access(d))
            self.assertFalse(self.test_user_2.has_edit_access(d))
            self.assertFalse(self.test_user_2.has_edit_access(t1_f))

        self.json_put(
            f"/permissions/edit",
            {
                'groups': ['testuser2'],
                'type': 'view',
                'action': 'remove',
                'ids': all_ids,
                'time': {
                    'type': 'always',
                },
                'confirm': False,
            },
        )
        t1_f = self.test_user_1.get_personal_folder()
        for p in paths:
            d = DocEntry.find_by_path(self.get_personal_item_path(p))
            self.assertTrue(self.test_user_1.has_ownership(d))
            self.assertFalse(self.test_user_2.has_view_access(d))
            self.assertTrue(self.test_user_3.has_view_access(d))
            self.assertFalse(self.test_user_2.has_edit_access(d))
            self.assertFalse(self.test_user_2.has_edit_access(t1_f))

        self.json_put(
            f"/permissions/edit",
            {
                'groups': ['testuser2', 'testuser3'],
                'type': 'view',
                'action': 'remove',
                'ids': all_ids,
                'time': {
                    'type': 'always',
                },
                'confirm': False,
            },
        )
        t1_f = self.test_user_1.get_personal_folder()
        for p in paths:
            d = DocEntry.find_by_path(self.get_personal_item_path(p))
            self.assertTrue(self.test_user_1.has_ownership(d))
            self.assertFalse(self.test_user_2.has_view_access(d))
            self.assertFalse(self.test_user_3.has_view_access(d))
            self.assertFalse(self.test_user_2.has_edit_access(d))
            self.assertFalse(self.test_user_2.has_edit_access(t1_f))

        self.login_test2()
        self.create_doc()
        self.login_test1()
        self.json_put(
            f"/permissions/edit",
            {
                'groups': ['testuser1'],
                'type': 'view',
                'action': 'add',
                'ids': [x.id for x in self.test_user_2.get_personal_folder().get_all_documents(include_subdirs=True)],
                'time': {
                    'type': 'always',
                },
                'confirm': False,
            },
            expect_status=403
        )
        self.json_put(
            f"/permissions/edit",
            {
                'groups': ['testuser1'],
                'type': 'asd',
                'action': 'add',
                'ids': all_ids,
                'time': {
                    'type': 'always',
                },
                'confirm': False,
            },
            expect_status=422,
        )
        self.json_put(
            f"/permissions/edit",
            {
                'groups': ['nonexistent'],
                'type': 'view',
                'action': 'add',
                'ids': all_ids,
                'time': {
                    'type': 'always',
                },
                'confirm': False,
            },
            expect_status=400,
        )

        # If the user doesn't have access to all documents, the operation should not complete.
        d = DocEntry.find_by_path(self.get_personal_item_path(paths[-1]))
        for a in d.block.accesses:
            db.session.delete(a)
        db.session.commit()
        self.json_put(
            f"/permissions/edit",
            {
                'groups': ['testuser2', 'testuser3'],
                'type': 'view',
                'action': 'add',
                'ids': all_ids,
                'time': {
                    'type': 'always',
                },
                'confirm': False,
            },
            expect_status=403
        )
        self.json_put(
            f"/permissions/edit",
            {
                'groups': ['testuser2', 'testuser3'],
                'type': 'view',
                'action': 'remove',
                'ids': all_ids,
                'time': {
                    'type': 'always',
                },
                'confirm': False,
            },
            expect_status=403
        )

    def test_chaining(self):
        self.login_test1()
        d = self.create_doc()
        # Add an expired right
        grant_access(
            group=self.test_user_2.get_personal_group(),
            access_type=AccessType.view,
            accessible_from=get_current_time(),
            accessible_to=get_current_time(),
            block=d,
        )
        d.document.set_settings({
            'auto_confirm': self.get_personal_item_path('nextdoc'),
            'expire_next_doc_message': 'My custom message',
        })
        self.login_test2()
        r = self.get(d.url, expect_status=403)
        self.assertIn('auto_confirm document does not exist', r)
        self.login_test1()
        d2 = self.create_doc(path=self.get_personal_item_path('nextdoc'))
        self.login_test2()
        r = self.get(d.url, expect_status=403)
        self.assertIn('Document is not authorized to auto-confirm rights', r)
        d2.document.set_settings({
            'allow_self_confirm_from': d.path,
        })
        r = self.get(d.url, expect_status=403)
        self.assertIn('Cannot get access:', r)
        self.assertNotIn('My custom message', r)
        grant_access(
            group=self.test_user_2.get_personal_group(),
            access_type=AccessType.view,
            block=d2,
            duration=timedelta(hours=2),
            require_confirm=True,
        )
        r = self.get(d.url, expect_status=403)
        self.assertIn('My custom message', r)
        self.assertIn('Go to the next document', r)

        # Make sure refreshing the page does not change anything.
        r = self.get(d.url, expect_status=403)
        self.assertIn('My custom message', r)
        self.assertIn('Go to the next document', r)
