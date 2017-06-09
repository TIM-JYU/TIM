from datetime import datetime, timezone, timedelta

from timApp.tests.db.timdbtest import TEST_USER_1_ID
from timApp.tests.server.timroutetest import TimRouteTest
from timApp.timdb.userutils import user_is_owner


class PermissionTest(TimRouteTest):

    def test_cannot_remove_ownership(self):
        self.login_test1()
        d = self.create_doc()
        self.assertTrue(user_is_owner(TEST_USER_1_ID, d.id))
        self.json_put('/permissions/add/{}/{}/{}'.format(d.id, 'testuser1', 'owner'),
                      {'from': datetime.now(tz=timezone.utc) + timedelta(days=1),
                       'type': 'range'},
                      expect_status=403)
        self.assertTrue(user_is_owner(TEST_USER_1_ID, d.id))
        self.json_put('/permissions/remove/{}/{}/{}'.format(d.id, self.get_test_user_1_group_id(), 'owner'),
                      expect_status=403)
        self.assertTrue(user_is_owner(TEST_USER_1_ID, d.id))

    def test_cannot_change_owner_of_personal_folder(self):
        self.login_test1()
        f = self.current_user.get_personal_folder()
        self.json_put('/permissions/add/{}/{}/{}'.format(f.id, 'testuser2', 'owner'), {},
                      expect_status=403,
                      expect_content={'error': 'You cannot add owners to your personal folder.'})

    def test_trim_whitespace(self):
        self.login_test1()
        f = self.current_user.get_personal_folder()
        self.json_put('/permissions/add/{}/{}/{}'.format(f.id, 'testuser2  ; testuser3 ', 'view'),
                      {'from': datetime.now(tz=timezone.utc),
                       'type': 'always'})
        self.assertTrue(self.test_user_2.has_view_access(f.id))
        self.assertTrue(self.test_user_3.has_view_access(f.id))
