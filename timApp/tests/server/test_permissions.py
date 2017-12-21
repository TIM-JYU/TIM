from datetime import datetime, timezone, timedelta

from timApp.tests.server.timroutetest import TimRouteTest
from timApp.timdb.models.docentry import DocEntry


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

    def test_logged_in_right(self):
        self.login_test1()
        d = self.create_doc()
        self.json_put(f'/permissions/add/{d.id}/Logged-in users/view',
                      {'from': datetime.now(tz=timezone.utc),
                       'type': 'always'})
        self.login_test2()
        self.get(d.url)
