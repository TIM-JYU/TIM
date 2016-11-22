from tests.server.timroutetest import TimRouteTest


class GroupTest(TimRouteTest):
    error_resp = {'error': 'Usergroup must contain at least one digit and one letter and must '
                           'be alphanumeric.'}

    def test_groups(self):
        db = self.get_db()
        db.users.addUserToAdmins(db.users.get_user_by_name('testuser3').id)
        self.login_test3()

        names = ['t' + str(i) for i in range(1, 5)]
        t1 = names[0]
        t2 = names[1]
        t3 = names[2]
        t4 = names[3]
        t5 = 't5'
        uids = [db.users.create_user_with_group(name)[0] for name in names]
        self.json_req('/groups/show/testgroup1', expect_status=404,
                      expect_content={'error': 'Usergroup does not exist.'})
        self.json_req('/groups/create/testgroup1', expect_content=self.ok_resp)
        self.json_req('/groups/addmember/testgroup1/{},{}'.format(t1, t3),
                      expect_content={'added': [t1, t3], 'already_belongs': [], 'not_exist': []})
        self.json_req('/groups/addmember/testgroup1/{},{}'.format(t1, t3),
                      expect_content={'already_belongs': [t1, t3], 'added': [], 'not_exist': []})
        self.json_req('/groups/addmember/testgroup1/{},{},{},{},{}'.format(t1, t2, t3, t4, t5),
                      expect_content={'already_belongs': [t1, t3], 'added': [t2, t4], 'not_exist': [t5]})

        self.json_req('/groups/show/testgroup1', expect_content=[
            {'email': t1 + '@example.com', 'id': uids[0], 'name': names[0], 'real_name': names[0]},
            {'email': t2 + '@example.com', 'id': uids[1], 'name': names[1], 'real_name': names[1]},
            {'email': t3 + '@example.com', 'id': uids[2], 'name': names[2], 'real_name': names[2]},
            {'email': t4 + '@example.com', 'id': uids[3], 'name': names[3], 'real_name': names[3]}
        ])

        self.json_req('/groups/removemember/testgroup1/{},{}'.format(t1, t3),
                      expect_content={'removed': [t1, t3], 'does_not_belong': [], 'not_exist': []})
        self.json_req('/groups/removemember/testgroup1/{},{}'.format(t1, t3),
                      expect_content={'removed': [], 'does_not_belong': [t1, t3], 'not_exist': []})
        self.json_req('/groups/removemember/testgroup1/{},{},{},{},{}'.format(t1, t2, t3, t4, t5),
                      expect_content={'removed': [t2, t4], 'does_not_belong': [t1, t3], 'not_exist': [t5]})
        self.json_req('/groups/show/testgroup1', expect_content=[])
        db.close()

    def test_invalid_groups(self):
        self.json_req('/groups/create/testgroup', expect_status=400, expect_content=self.error_resp)
        self.json_req('/groups/create/1', expect_status=400, expect_content=self.error_resp)
        self.json_req('/groups/create/a1@a', expect_status=400, expect_content=self.error_resp)
        self.json_req('/groups/create/ok ok', expect_status=400, expect_content=self.error_resp)
        self.json_req('/groups/create/test x1', expect_content=self.ok_resp)
