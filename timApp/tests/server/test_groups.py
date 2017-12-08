from timApp.tests.server.timroutetest import TimRouteTest
from timApp.timdb.models.user import User
from timApp.timdb.models.usergroup import UserGroup
from timApp.timdb.tim_models import db


class GroupTest(TimRouteTest):
    error_resp = {'error': 'Usergroup must contain at least one digit and one letter and must '
                           'be alphanumeric.'}

    def test_groups(self):
        self.init_admin()

        names = ['t' + str(i) for i in range(1, 5)]
        t1 = names[0]
        t2 = names[1]
        t3 = names[2]
        t4 = names[3]
        t5 = 't5'
        uids = [User.create_with_group(name)[0].id for name in names]
        db.session.commit()
        self.get('/groups/show/testgroup1', expect_status=404,
                 expect_content={'error': 'User group not found'})
        self.get('/groups/create/testgroup1', expect_content=self.ok_resp)
        self.get('/groups/create/testgroup1', expect_content={'error': 'User group already exists.'}, expect_status=400)
        self.get(f'/groups/addmember/testgroup1/{t1},{t3}',
                 expect_content={'added': [t1, t3], 'already_belongs': [], 'not_exist': []})
        self.get(f'/groups/addmember/testgroup1/{t1},{t3}',
                 expect_content={'already_belongs': [t1, t3], 'added': [], 'not_exist': []})
        self.get(f'/groups/addmember/testgroup1/{t1},{t2},{t3},{t4},{t5}',
                 expect_content={'already_belongs': [t1, t3], 'added': [t2, t4], 'not_exist': [t5]})
        self.get(f'/groups/addmember/testgroup1/{t1},{t2}',
                 expect_content={'already_belongs': [t1, t2], 'added': [], 'not_exist': []})
        self.get(f'/groups/usergroups/{t1}',
                 expect_content=[{'id': 9, 'name': 't1'}, {'id': 13, 'name': 'testgroup1'}])
        self.get(f'/groups/belongs/{t1}/testgroup1',
                 expect_content={'status': True})

        self.get('/groups/show/testgroup1', expect_content=[
            {'email': t1 + '@example.com', 'id': uids[0], 'name': names[0], 'real_name': names[0]},
            {'email': t2 + '@example.com', 'id': uids[1], 'name': names[1], 'real_name': names[1]},
            {'email': t3 + '@example.com', 'id': uids[2], 'name': names[2], 'real_name': names[2]},
            {'email': t4 + '@example.com', 'id': uids[3], 'name': names[3], 'real_name': names[3]}
        ])

        self.get(f'/groups/removemember/testgroup1/{t1},{t3}',
                 expect_content={'removed': [t1, t3], 'does_not_belong': [], 'not_exist': []})
        self.get(f'/groups/removemember/testgroup1/{t1},{t3}',
                 expect_content={'removed': [], 'does_not_belong': [t1, t3], 'not_exist': []})
        self.get(f'/groups/removemember/testgroup1/{t1},{t2},{t3},{t4},{t5}',
                 expect_content={'removed': [t2, t4], 'does_not_belong': [t1, t3], 'not_exist': [t5]})
        self.get('/groups/show/testgroup1', expect_content=[])

    def init_admin(self):
        u = User.get_by_name('testuser3')
        admin_group = UserGroup.get_admin_group()
        if u not in admin_group.users:
            u.groups.append(admin_group)
            db.session.commit()
        self.login_test3()

    def test_invalid_groups(self):
        self.init_admin()
        self.get('/groups/create/testgroup', expect_status=400, expect_content=self.error_resp)
        self.get('/groups/create/1', expect_status=400, expect_content=self.error_resp)
        self.get('/groups/create/a1@a', expect_status=400, expect_content=self.error_resp)
        self.get('/groups/create/ok ok', expect_status=400, expect_content=self.error_resp)
        self.get('/groups/create/test x1', expect_content=self.ok_resp)

        self.get('/groups/addmember/Logged-in users/testuser1', expect_status=400,
                 expect_content={'error': 'Cannot add members to special groups.'})
        self.get('/groups/removemember/Logged-in users/testuser1', expect_status=400,
                 expect_content={'error': 'Cannot remove members from special groups.'})

    def test_nonexistent(self):
        self.init_admin()
        self.get(f'/groups/belongs/asd/testuser1',
                 expect_content={'error': 'User not found'}, expect_status=404)
        self.get(f'/groups/belongs/testuser1/asd',
                 expect_content={'error': 'User group not found'}, expect_status=404)
        self.get(f'/groups/usergroups/asd',
                 expect_content={'error': 'User not found'}, expect_status=404)
        self.get(f'/groups/addmember/asd/testuser1',
                 expect_content={'error': 'User group not found'}, expect_status=404)

    def test_groups_trim(self):
        self.init_admin()
        self.get('/groups/create/testing1', expect_content=self.ok_resp)
        self.get('/groups/addmember/testing1/testuser1 ,testuser2  ',
                 expect_content={'added': ['testuser1', 'testuser2'], 'already_belongs': [], 'not_exist': []})
