from timroutetest import TimRouteTest


class GroupLoginTest(TimRouteTest):
    def test_grouplogin(self):
        resp = self.login_test1(force=True)
        uid1 = resp['current_user']['id']
        one_user = {'current_user': {'email': 'test1@example.com', 'id': uid1, 'name': 'testuser1',
                                     'real_name': 'Test user 1'}, 'other_users': []}
        self.assertDictEqual(one_user, resp)
        self.maxDiff = None
        resp = self.login_test2(add=True)
        uid2 = resp['other_users'][0]['id']
        two_users = {'current_user': {'email': 'test1@example.com', 'id': uid1, 'name': 'testuser1',
                                      'real_name': 'Test user 1'},
                     'other_users': [
                         {'email': 'test2@example.com',
                          'id': uid2,
                          'name': 'testuser2',
                          'real_name': 'Test user 2'}]}
        self.assertDictEqual(two_users, resp)

        # Trying to log in again should not add a duplicate entry
        resp = self.login_test2(add=True)
        self.assertDictEqual(two_users, resp)

        resp = self.logout(user_id=uid2)
        self.assertDictEqual(one_user, resp)
        resp = self.login_test2(add=True)
        self.assertDictEqual(two_users, resp)
        resp = self.logout(user_id=uid1)
        self.assertDictEqual({'current_user': {'email': None,
                                               'id': 0,
                                               'name': 'Anonymous',
                                               'real_name': None},
                              'other_users': []}, resp)
