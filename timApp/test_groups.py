from timroutetest import TimRouteTest


class GroupTest(TimRouteTest):
    ok_resp = {'status': 'ok'}
    error_resp = {'error': 'Usergroup must contain at least one digit and one letter and must '
                           'be alphanumeric.'}

    def test_groups(self):
        db = self.get_db()
        db.users.create_user_with_group('testuser3', 'Test user 3', 'test3@example.com', password='test3pass',
                                        is_admin=True)
        self.login_test3()

        db.users.create_user_with_group('t1')
        db.users.create_user_with_group('t2')
        db.users.create_user_with_group('t3')
        db.users.create_user_with_group('t4')
        self.assertDictResponse({'error': 'Usergroup does not exist.'}, self.json_req('/groups/show/testgroup1'),
                                expect_status=404)
        self.assertDictResponse(self.ok_resp, self.json_req('/groups/create/testgroup1'))
        self.assertDictResponse({'added': ['t1', 't3'], 'already_belongs': [], 'not_exist': []},
                                self.json_req('/groups/addmember/testgroup1/t1,t3'))
        self.assertDictResponse({'already_belongs': ['t1', 't3'], 'added': [], 'not_exist': []},
                                self.json_req('/groups/addmember/testgroup1/t1,t3'))
        self.assertDictResponse({'already_belongs': ['t1', 't3'], 'added': ['t2', 't4'], 'not_exist': ['t5']},
                                self.json_req('/groups/addmember/testgroup1/t1,t2,t3,t4,t5'))

        self.assertListResponse([{'email': 't1@example.com', 'id': 6, 'name': 't1', 'real_name': 't1'},
                                 {'email': 't2@example.com', 'id': 7, 'name': 't2', 'real_name': 't2'},
                                 {'email': 't3@example.com', 'id': 8, 'name': 't3', 'real_name': 't3'},
                                 {'email': 't4@example.com', 'id': 9, 'name': 't4', 'real_name': 't4'}],
                                self.json_req('/groups/show/testgroup1'))

        self.assertDictResponse({'removed': ['t1', 't3'], 'does_not_belong': [], 'not_exist': []},
                                self.json_req('/groups/removemember/testgroup1/t1,t3'))
        self.assertDictResponse({'removed': [], 'does_not_belong': ['t1', 't3'], 'not_exist': []},
                                self.json_req('/groups/removemember/testgroup1/t1,t3'))
        self.assertDictResponse({'removed': ['t2', 't4'], 'does_not_belong': ['t1', 't3'], 'not_exist': ['t5']},
                                self.json_req('/groups/removemember/testgroup1/t1,t2,t3,t4,t5'))
        self.assertListResponse([], self.json_req('/groups/show/testgroup1'))
        db.close()

    def test_invalid_groups(self):
        self.assertDictResponse(self.error_resp, self.json_req('/groups/create/testgroup'), expect_status=400)
        self.assertDictResponse(self.error_resp, self.json_req('/groups/create/1'), expect_status=400)
        self.assertDictResponse(self.error_resp, self.json_req('/groups/create/a1@a'), expect_status=400)
        self.assertDictResponse(self.error_resp, self.json_req('/groups/create/ok ok'), expect_status=400)
        self.assertDictResponse(self.ok_resp, self.json_req('/groups/create/test x1'))
