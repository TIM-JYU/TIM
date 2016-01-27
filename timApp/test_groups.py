from timroutetest import TimRouteTest


class GroupTest(TimRouteTest):
    error_resp = {'error': 'Usergroup must contain at least one digit and one letter and must '
                           'be alphanumeric.'}

    def test_groups(self):
        db = self.get_db()
        db.users.create_user_with_group('testuser3', 'Test user 3', 'test3@example.com', password='test3pass',
                                        is_admin=True)
        self.login_test3()

        names = ['t' + str(i) for i in range(1, 5)]
        t1 = names[0]
        t2 = names[1]
        t3 = names[2]
        t4 = names[3]
        t5 = 't5'
        uids = [db.users.create_user_with_group(name)[0] for name in names]
        self.assertDictResponse({'error': 'Usergroup does not exist.'}, self.json_req('/groups/show/testgroup1'),
                                expect_status=404)
        self.assertDictResponse(self.ok_resp, self.json_req('/groups/create/testgroup1'))
        self.assertDictResponse({'added': [t1, t3], 'already_belongs': [], 'not_exist': []},
                                self.json_req('/groups/addmember/testgroup1/{},{}'.format(t1, t3)))
        self.assertDictResponse({'already_belongs': [t1, t3], 'added': [], 'not_exist': []},
                                self.json_req('/groups/addmember/testgroup1/{},{}'.format(t1, t3)))
        self.assertDictResponse({'already_belongs': [t1, t3], 'added': [t2, t4], 'not_exist': [t5]},
                                self.json_req('/groups/addmember/testgroup1/{},{},{},{},{}'.format(t1, t2, t3, t4, t5)))

        self.assertListResponse([
            {'email': t1 + '@example.com', 'id': uids[0], 'name': names[0], 'real_name': names[0]},
            {'email': t2 + '@example.com', 'id': uids[1], 'name': names[1], 'real_name': names[1]},
            {'email': t3 + '@example.com', 'id': uids[2], 'name': names[2], 'real_name': names[2]},
            {'email': t4 + '@example.com', 'id': uids[3], 'name': names[3], 'real_name': names[3]}
        ],
                self.json_req('/groups/show/testgroup1'))

        self.assertDictResponse({'removed': [t1, t3], 'does_not_belong': [], 'not_exist': []},
                                self.json_req('/groups/removemember/testgroup1/{},{}'.format(t1, t3)))
        self.assertDictResponse({'removed': [], 'does_not_belong': [t1, t3], 'not_exist': []},
                                self.json_req('/groups/removemember/testgroup1/{},{}'.format(t1, t3)))
        self.assertDictResponse({'removed': [t2, t4], 'does_not_belong': [t1, t3], 'not_exist': [t5]},
                                self.json_req(
                                        '/groups/removemember/testgroup1/{},{},{},{},{}'.format(t1, t2, t3, t4, t5)))
        self.assertListResponse([], self.json_req('/groups/show/testgroup1'))
        db.close()

    def test_invalid_groups(self):
        self.assertDictResponse(self.error_resp, self.json_req('/groups/create/testgroup'), expect_status=400)
        self.assertDictResponse(self.error_resp, self.json_req('/groups/create/1'), expect_status=400)
        self.assertDictResponse(self.error_resp, self.json_req('/groups/create/a1@a'), expect_status=400)
        self.assertDictResponse(self.error_resp, self.json_req('/groups/create/ok ok'), expect_status=400)
        self.assertDictResponse(self.ok_resp, self.json_req('/groups/create/test x1'))
