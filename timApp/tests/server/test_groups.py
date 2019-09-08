from timApp.document.docentry import DocEntry
from timApp.tests.server.timroutetest import TimRouteTest
from timApp.timdb.sqa import db
from timApp.user.user import User
from timApp.user.usergroup import UserGroup


class GroupTest(TimRouteTest):
    def error_resp(self, name):
       return {'error': 'Usergroup must contain at least one digit and one letter and must '
                           'not have special chars: "' + name + '"'}

    def enum_admin_and_groupadmin(self):
        yield self.init_admin()
        yield self.init_groupadmin()

    def test_groups(self):
        for is_admin in self.enum_admin_and_groupadmin():
            names = [f't{i}{is_admin}' for i in range(1, 5)]
            t1 = names[0]
            t2 = names[1]
            t3 = names[2]
            t4 = names[3]
            t5 = f't5{is_admin}'
            users_and_groups = [User.create_with_group(name, name, email=name + '@example.com') for name in names]
            db.session.flush()
            t1gid = users_and_groups[0][1].id
            uids = [u.id for u, g in users_and_groups]
            db.session.commit()
            groupname = f'testgroup1{is_admin}'
            self.get(f'/groups/show/{groupname}', expect_status=404,
                     expect_content={'error': 'User group not found'})
            self.get(f'/groups/create/{groupname}')
            self.get(f'/groups/create/{groupname}',
                     expect_content={'error': 'User group already exists.'},
                     expect_status=400)
            self.json_post(f'/groups/addmember/{groupname}', {'names': f'{t1},{t3}'.split(',')},
                     expect_content={'added': [t1, t3], 'already_belongs': [], 'not_exist': []})
            self.json_post(f'/groups/addmember/{groupname}', {'names': f'{t1},{t3}'.split(',')},
                     expect_content={'already_belongs': [t1, t3], 'added': [], 'not_exist': []})
            self.json_post(f'/groups/addmember/{groupname}', {'names': f'{t1},{t2},{t3},{t4},{t5}'.split(',')},
                     expect_content={'already_belongs': [t1, t3], 'added': [t2, t4], 'not_exist': [t5]})
            self.json_post(f'/groups/addmember/{groupname}', {'names': f'{t1},{t2}'.split(',')},
                     expect_content={'already_belongs': [t1, t2], 'added': [], 'not_exist': []})
            ug = UserGroup.get_by_name(groupname)
            if is_admin:
                self.get(f'/groups/usergroups/{t1}',
                         expect_content=[{'id': t1gid, 'name': t1}, {'id': ug.id, 'name': groupname}])
                self.get(f'/groups/belongs/{t1}/{groupname}',
                         expect_content={'status': True})
            else:
                self.get(
                    f'/groups/usergroups/{t1}',
                    expect_status=403,
                    json_key='error',
                    expect_content='This action requires administrative rights.',
                )
                self.get(
                    f'/groups/belongs/{t1}/{groupname}',
                )

            self.get(f'/groups/show/{groupname}', expect_content=[
                {'email': t1 + '@example.com', 'id': uids[0], 'name': names[0], 'real_name': names[0]},
                {'email': t2 + '@example.com', 'id': uids[1], 'name': names[1], 'real_name': names[1]},
                {'email': t3 + '@example.com', 'id': uids[2], 'name': names[2], 'real_name': names[2]},
                {'email': t4 + '@example.com', 'id': uids[3], 'name': names[3], 'real_name': names[3]}
            ])

            self.json_post(f'/groups/removemember/{groupname}', {'names': f'{t1},{t3}'.split(',')},
                     expect_content={'removed': [t1, t3], 'does_not_belong': [], 'not_exist': []})
            self.json_post(f'/groups/removemember/{groupname}', {'names': f'{t1},{t3}'.split(',')},
                     expect_content={'removed': [], 'does_not_belong': [t1, t3], 'not_exist': []})
            self.json_post(f'/groups/removemember/{groupname}', {'names': f'{t1},{t2},{t3},{t4},{t5}'.split(',')},
                     expect_content={'removed': [t2, t4], 'does_not_belong': [t1, t3], 'not_exist': [t5]})
            self.get(f'/groups/show/{groupname}', expect_content=[])

    def init_admin(self):
        u = self.test_user_3
        self.make_admin(u)
        self.login_test3()
        return True

    def init_groupadmin(self):
        u = self.test_user_2
        self.add_to_group(UserGroup.get_groupadmin_group(), u)
        self.login_test2()
        return False

    def test_invalid_groups(self):
        for is_admin in self.enum_admin_and_groupadmin():
            self.get('/groups/create/testgroup', expect_status=400, expect_content=self.error_resp("testgroup"))
            self.get('/groups/create/1', expect_status=400, expect_content=self.error_resp("1"))
            self.get('/groups/create/a1@a', expect_status=400, expect_content=self.error_resp("a1@a"))
            self.get('/groups/create/ok ok', expect_status=400, expect_content=self.error_resp("ok ok"))
            self.get(f'/groups/create/test x1{is_admin}')

            self.json_post('/groups/addmember/Logged-in users', {'names': f'testuser1'.split(',')}, expect_status=400,
                     expect_content={'error': 'Cannot edit special groups.'})
            if not is_admin:
                self.json_post('/groups/addmember/Group admins', {'names': f'testuser1'.split(',')},
                    expect_status=403,
                    expect_content={'error': 'This action requires administrative rights.'},
                )
                self.json_post('/groups/addmember/Administrators', {'names': f'testuser1'.split(',')},
                    expect_status=403,
                    expect_content={'error': 'This action requires administrative rights.'},
                )
                self.json_post('/groups/removemember/Administrators', {'names': f'testuser1'.split(',')},
                    expect_status=403,
                    expect_content={'error': 'This action requires administrative rights.'},
                )
            else:
                self.json_post('/groups/addmember/Group admins', {'names': f'testuser1'.split(',')})
            self.json_post('/groups/addmember/testuser1', {'names': f'testuser2'.split(',')}, expect_status=400,
                     expect_content={'error': 'Cannot edit personal groups.'})
            self.json_post('/groups/removemember/testuser1', {'names': f'testuser1'.split(',')}, expect_status=400,
                     expect_content={'error': 'Cannot edit personal groups.'})
            self.json_post('/groups/removemember/Logged-in users', {'names': f'testuser1'.split(',')}, expect_status=400,
                     expect_content={'error': 'Cannot edit special groups.'})

    def test_nonexistent(self):
        self.init_admin()
        self.get(f'/groups/belongs/asd/testuser1',
                 expect_content={'error': 'User not found'}, expect_status=404)
        self.get(f'/groups/belongs/testuser1/asd',
                 expect_content={'error': 'User group not found'}, expect_status=404)
        self.get(f'/groups/usergroups/asd',
                 expect_content={'error': 'User not found'}, expect_status=404)
        self.json_post(f'/groups/addmember/asd', {'names': f'testuser1'.split(',')},
                 expect_content={'error': 'User group not found'}, expect_status=404)

    def test_groups_trim(self):
        self.init_admin()
        self.get('/groups/create/testing1')
        self.json_post('/groups/addmember/testing1', {'names': f'testuser1 ,testuser2  '.split(',')},
                 expect_content={'added': ['testuser1', 'testuser2'], 'already_belongs': [], 'not_exist': []})

    def test_invalid_group_setting(self):
        d = self.create_doc(settings={'group': ["a", "b"]})
        html = self.get(d.get_url_for_view('teacher'))
        self.assertIn("The setting &#39;group&#39; must be a string.", html)

    def test_doc_group_setting_access(self):
        self.login_test1()
        no_access_msg = "You don&#39;t have access to group &#39;testuser1&#39;."
        d = self.create_doc(settings={'group': self.current_user.get_personal_group().name})
        self.assertNotIn(no_access_msg, self.get(d.get_url_for_view('teacher')))
        self.test_user_2.grant_access(d, 'teacher')
        self.login_test2()
        self.assertIn(no_access_msg, self.get(d.get_url_for_view('teacher')))

class GroupTest2(TimRouteTest):
    def test_group_edit_access(self):
        self.test_user_3.make_admin()
        db.session.commit()
        self.login_test3()
        self.get('/groups/create/edittest1')
        d = DocEntry.find_by_path('groups/edittest1')
        self.assertEqual('Administrators', d.parent.owner.name)
        self.assertEqual({'group': 'edittest1', 'fields': ['info'], 'maxRows': '40em'},
                         d.document.get_settings().get_dict()['macros'])
        self.login_test1()
        self.get('/groups/show/edittest1', expect_status=403)
        self.test_user_1.grant_access(d, 'view')
        self.get(d.url)
        self.get(d.parent.url)
        self.get('/groups/show/edittest1')
        self.json_post('/groups/addmember/edittest1', {'names': f'testuser1'.split(',')}, expect_status=403)
        self.test_user_1.grant_access(d, 'edit')
        self.json_post('/groups/addmember/edittest1', {'names': f'testuser1'.split(',')},
                 expect_content={'added': ['testuser1'], 'already_belongs': [], 'not_exist': []})
