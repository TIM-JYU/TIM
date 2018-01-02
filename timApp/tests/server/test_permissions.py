from datetime import datetime, timezone, timedelta

from timApp.tests.server.timroutetest import TimRouteTest
from timApp.timdb.item import Item
from timApp.timdb.models.docentry import DocEntry
from timApp.timdb.models.usergroup import UserGroup


class PermissionTest(TimRouteTest):

    def test_cannot_remove_ownership(self):
        self.login_test1()
        d = self.create_doc()
        self.assertTrue(self.current_user.has_ownership(d))
        self.json_put(f'/permissions/add/{d.id}/{"testuser1"}/{"owner"}',
                      {'from': datetime.now(tz=timezone.utc) + timedelta(days=1),
                       'type': 'range'},
                      expect_status=403)
        d = DocEntry.find_by_id(d.id)
        self.assertTrue(self.current_user.has_ownership(d))
        self.json_put(f'/permissions/remove/{d.id}/{self.get_test_user_1_group_id()}/{"owner"}',
                      expect_status=403)
        d = DocEntry.find_by_id(d.id)
        self.assertTrue(self.current_user.has_ownership(d))

    def test_cannot_change_owner_of_personal_folder(self):
        self.login_test1()
        f = self.current_user.get_personal_folder()
        self.json_put(f'/permissions/add/{f.id}/{"testuser2"}/{"owner"}', {},
                      expect_status=403,
                      expect_content={'error': 'You cannot add owners to your personal folder.'})

    def test_trim_whitespace(self):
        self.login_test1()
        f = self.current_user.get_personal_folder()
        self.json_put(f'/permissions/add/{f.id}/{"testuser2  ; testuser3 "}/{"view"}',
                      {'from': datetime.now(tz=timezone.utc),
                       'type': 'always'})
        f = self.current_user.get_personal_folder()
        self.assertTrue(self.test_user_2.has_view_access(f))
        self.assertTrue(self.test_user_3.has_view_access(f))

    def test_permissions_get(self):
        self.login_test1()
        for i in (self.current_user.get_personal_folder(), self.create_doc()):
            self.json_put(f'/permissions/add/{i.id}/testuser2/view',
                          {'from': datetime.now(tz=timezone.utc),
                           'type': 'always'})
            rights = self.get(f'/permissions/get/{i.id}')
            i = Item.find_by_id(i.id)
            self.assertEqual(rights['accesstypes'],
                             [{'id': 1, 'name': 'view'},
                              {'id': 2, 'name': 'edit'},
                              {'id': 3, 'name': 'teacher'},
                              {'id': 4, 'name': 'manage'},
                              {'id': 5, 'name': 'see answers'},
                              {'id': 6, 'name': 'owner'}])
            self.assertEqual(rights['grouprights'],
                             [{'access_name': 'owner',
                               'access_type': 6,
                               'accessible_from': i.block.accesses[
                                   0].accessible_from.isoformat(),
                               'accessible_to': None,
                               'duration': None,
                               'duration_from': None,
                               'duration_to': None,
                               'fullname': 'Test user 1',
                               'gid': 6,
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
                               'gid': 7,
                               'name': 'testuser2'}, ])

    def test_logged_in_right(self):
        self.login_test1()
        d = self.create_doc()
        self.json_put(f'/permissions/add/{d.id}/Logged-in users/view',
                      {'from': datetime.now(tz=timezone.utc),
                       'type': 'always'})
        self.login_test2()
        self.get(d.url)
        self.login_test1()
        self.json_put(f'/permissions/remove/{d.id}/{UserGroup.get_logged_in_group().id}/view')
        self.login_test2()
        self.get(d.url, expect_status=403)
