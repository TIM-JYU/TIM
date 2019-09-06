from operator import itemgetter
from typing import List

from timApp.auth.accesstype import AccessType
from timApp.document.docentry import DocEntry
from timApp.notification.notify import sent_mails_in_testing
from timApp.sisu.scim import SISU_GROUP_PREFIX
from timApp.tests.server.timroutetest import TimRouteTest
from timApp.timdb.sqa import db
from timApp.user.user import User, UserOrigin
from timApp.user.usergroup import UserGroup, DELETED_GROUP_PREFIX
from timApp.util.utils import seq_to_str

a = ('t', 'pass')


def add_name_parts(datas):
    for data in datas:
        fullname = data.get('display')
        if fullname is None:
            continue
        fullname = fullname.split(' ')
        data['name'] = {
            'givenName': fullname[0],
            'familyName': fullname[-1],
            'middleName': ' '.join(fullname[1:-1]) if len(fullname) > 2 else None,
        }
    return datas


class ScimTest(TimRouteTest):
    def test_scim(self):
        eid = 'jy-CUR-8888-teachers'
        display_name = 'ITKP102 2021-09-09--2021-12-20: Opettajat'
        self.json_post(
            '/scim/Groups',
            **scim_error('This action requires authentication.', 401),
        )
        self.json_post(
            '/scim/Groups',
            auth=('cat', 'dog'),
            **scim_error('Incorrect username or password.', 401),
        )
        self.json_post('/scim/Groups', auth=a,
                       **scim_error(
                           'The request was well-formed but was unable to be followed due to semantic errors.'),
                       )
        r = self.json_post(
            '/scim/Groups',
            json_data={
                'externalId': eid,
                'displayName': display_name,
                'members': add_name_parts([
                    {'value': 'sisuuser', 'display': 'Sisu User', 'email': 'x@example.com'},
                    {'value': 'sisuuser3', 'display': 'Sisu User 3'},
                ]),
            }, auth=a,
            expect_status=201,
            expect_contains={
                'displayName': display_name,
                'id': eid,
                'members': [{'$ref': 'http://localhost/scim/Users/sisuuser',
                             'display': 'Sisu User',
                             'value': 'sisuuser'},
                            {'$ref': 'http://localhost/scim/Users/sisuuser3',
                             'display': 'Sisu User 3',
                             'value': 'sisuuser3'}
                            ],
                'schemas': ['urn:ietf:params:scim:schemas:core:2.0:Group'],
            }
        )

        self.json_post(
            '/scim/Groups',
            json_data={
                'externalId': eid,
                'displayName': display_name,
                'members': [
                    {'value': 'sisuuser', 'display': 'Sisu User', 'email': 'x@example.com'},
                    {'value': 'sisuuser3', 'display': 'Sisu User 3'},
                ],
            }, auth=a,
            expect_status=409,
            expect_content={'detail': f'Group already exists: {eid}',
                            'schemas': ['urn:ietf:params:scim:api:messages:2.0:Error'],
                            'status': '409'}
        )

        create_stamp = r['meta']['created']
        self.assertEqual(create_stamp, r['meta']['lastModified'])
        group_id = r['id']
        self.assertEqual(eid, group_id)
        ru = self.get(
            f'/scim/Users/sisuuser',
            auth=a,
        )
        create_stamp_user = ru['meta']['created']
        self.assertEqual({
            'displayName': 'Sisu User',
            'id': 'sisuuser',
            'externalId': 'sisuuser',
            'emails': [{'value': 'x@example.com'}],
            'meta': {
                'created': create_stamp_user,
                'lastModified': create_stamp_user,
                'location': 'http://localhost/scim/Users/sisuuser',
                'resourceType': 'User',
            },
            'schemas': ['urn:ietf:params:scim:schemas:core:2.0:User'],
        }, ru)

        user_not_found = {
            'detail': 'User not found.',
            'schemas': ['urn:ietf:params:scim:api:messages:2.0:Error'],
            'status': '404',
        }
        self.get(
            f'/scim/Users/xxx',
            auth=a,
            expect_status=404,
            expect_content=user_not_found,
        )
        self.json_put(
            f'/scim/Users/xxx',
            auth=a,
            json_data={
                'displayName': 'Sisu User',
                'emails': [{'value': 'sisuuser@example.com'}],
                'externalId': 'sisuuser',
                'userName': 'sisuuser',
            },
            expect_status=404,
            expect_content=user_not_found,
        )

        def update_and_get():
            self.json_put(
                f'/scim/Users/sisuuser',
                auth=a,
                json_data={
                    'displayName': 'Sisu User',
                    'emails': [{'value': 'sisuuser@example.com'}],
                    'externalId': 'sisuuser',
                    'userName': 'sisuuser',
                },
            )
            return self.get(
                f'/scim/Users/sisuuser',
                auth=a,
                expect_contains={
                    'displayName': 'Sisu User',
                    'id': 'sisuuser',
                    'emails': [{'value': 'sisuuser@example.com'}],
                    'schemas': ['urn:ietf:params:scim:schemas:core:2.0:User'],
                },
            )

        ru2 = update_and_get()
        self.assertEqual(create_stamp_user, ru2['meta']['created'])
        new_modified = ru2['meta']['lastModified']
        self.assertNotEqual(create_stamp_user, new_modified)
        ru3 = update_and_get()
        self.assertEqual(ru3['meta']['lastModified'], new_modified)

        # self.assertIsNone(UserGroup.get_by_name(eid))
        self.json_post(
            '/scim/Groups',
            json_data={
                'externalId': eid,
                'displayName': display_name,
                'members': [],
            }, auth=a,
            expect_status=409,
        )
        self.json_put(
            f'/scim/Groups/{group_id}', auth=a,
            **scim_error('JSON payload missing.'),
        )
        self.json_put(
            f'/scim/Groups/{group_id}', json_data={}, auth=a,
            **scim_error('{"displayName": ["Missing data for required field."], '
                         '"externalId": ["Missing data for required field."], "members": '
                         '["Missing data for required field."]}'),
        )
        self.json_put(f'/scim/Groups/xxx', auth=a, expect_status=404)
        self.json_put(f'/scim/Groups/group-999', auth=a, expect_status=404)
        self.json_put(f'/scim/Groups/group-xxx', auth=a, expect_status=404)
        self.json_put(
            f'/scim/Groups/{group_id}',
            json_data={
                'externalId': eid,
                'displayName': display_name,
                'members': add_name_parts([
                    {
                        'display': 'Sisu User',
                        'value': 'sisuuser',
                    },
                    {
                        'display': 'Sisu User 2',
                        'value': 'sisuuser2',
                    },
                ]),
            }, auth=a,
            expect_content={
                'displayName': display_name,
                'id': group_id,
                'externalId': eid,
                'members': [{'$ref': 'http://localhost/scim/Users/sisuuser',
                             'display': 'Sisu User',
                             'value': 'sisuuser'},
                            {'$ref': 'http://localhost/scim/Users/sisuuser2',
                             'display': 'Sisu User 2',
                             'value': 'sisuuser2'}],
                'meta': {'created': create_stamp,
                         'lastModified': create_stamp,
                         'location': f'http://localhost/scim/Groups/{eid}',
                         'resourceType': 'Group'},
                'schemas': ['urn:ietf:params:scim:schemas:core:2.0:Group']},
        )

        r = self.get(f'/scim/Groups', auth=a, query_string={'filter': 'externalId sw jy-CUR-8888'})
        self.assertEqual(
            {
                'Resources': [
                    {
                        'id': group_id,
                        'externalId': eid,
                        'meta': {
                            'created': create_stamp,
                            'lastModified': create_stamp,
                            'location': f'http://localhost/scim/Groups/{eid}',
                            'resourceType': 'Group',
                        },
                    },
                ],
                'schemas': ['urn:ietf:params:scim:api:messages:2.0:ListResponse'],
                'totalResults': 1,
            }, r)
        r = self.get(
            f'/scim/Groups', auth=a, query_string={'filter': 'asd sw sisu-'},
            **scim_error('Unsupported filter'),
        )
        r = self.get(
            f'/scim/Groups/{group_id}',
            auth=a,
            expect_content={
                'displayName': display_name,
                'id': group_id,
                'externalId': eid,
                'members': [{'$ref': 'http://localhost/scim/Users/sisuuser',
                             'display': 'Sisu User',
                             'value': 'sisuuser'},
                            {'$ref': 'http://localhost/scim/Users/sisuuser2',
                             'display': 'Sisu User 2',
                             'value': 'sisuuser2'}
                            ],
                'meta': {
                    'created': create_stamp,
                    'lastModified': create_stamp,
                    'location': f'http://localhost/scim/Groups/{eid}',
                    'resourceType': 'Group'},
                'schemas': [
                    'urn:ietf:params:scim:schemas:core:2.0:Group']},
        )
        self.json_delete(f'/scim/Groups/{group_id}', auth=a, expect_status=204)
        self.json_delete(f'/scim/Groups/{group_id}', auth=a, expect_status=404)
        deleted_group = UserGroup.get_by_name(f'{DELETED_GROUP_PREFIX}{SISU_GROUP_PREFIX}{eid}')
        self.assertIsNotNone(deleted_group)

        r = self.json_post(
            '/scim/Groups',
            json_data={
                'externalId': eid,
                'displayName': display_name,
                'members': add_name_parts([
                    {'value': 'sisuuser', 'display': 'Sisu User'},
                    {'value': 'sisuuser3', 'display': 'Sisu User 3'},
                ]),
            }, auth=a,
            expect_status=201,
            expect_contains={
                'displayName': display_name,
                'id': group_id,
                'members': [{'$ref': 'http://localhost/scim/Users/sisuuser',
                             'display': 'Sisu User',
                             'value': 'sisuuser'},
                            {'$ref': 'http://localhost/scim/Users/sisuuser3',
                             'display': 'Sisu User 3',
                             'value': 'sisuuser3'}
                            ],
                'schemas': ['urn:ietf:params:scim:schemas:core:2.0:Group'],
            }
        )
        self.assertNotEqual(create_stamp, r['meta']['lastModified'])
        g = UserGroup.get_by_name(f'{DELETED_GROUP_PREFIX}{eid}')
        self.assertIsNone(g)

    def test_no_display_in_members(self):
        r = self.json_post(
            '/scim/Groups',
            json_data={
                "schemas": [
                    "urn:ietf:params:scim:schemas:core:2.0:Group"
                ],
                "externalId": "jy-CUR-4406-jy-studysubgroup-8514-teachers",
                "displayName": "XKV0201 2019-08-12--2019-12-23: Harjoitusryhm\u00e4: Opettajat",
                "members": add_name_parts([
                    {
                        "value": "someuser1",
                        "$ref": "https://timdevs02-5.it.jyu.fi/scim/Users/someuser1",
                        "type": "User",
                    },
                    {
                        "value": "someuser2",
                        "$ref": "https://timdevs02-5.it.jyu.fi/scim/Users/someuser2",
                        "type": "User",
                    },
                    {
                        "value": "someuser3",
                        "$ref": "https://timdevs02-5.it.jyu.fi/scim/Users/someuser3",
                        "type": "User",
                    }
                ])
            }, auth=a,
            expect_status=201,
            expect_contains={
                'displayName': 'XKV0201 2019-08-12--2019-12-23: Harjoitusryhmä: Opettajat',
                'id': 'jy-CUR-4406-jy-studysubgroup-8514-teachers',
                'members': [
                    {'$ref': 'http://localhost/scim/Users/someuser1',
                     'display': None,
                     'value': 'someuser1'},
                    {'$ref': 'http://localhost/scim/Users/someuser2',
                     'display': None,
                     'value': 'someuser2'},
                    {'$ref': 'http://localhost/scim/Users/someuser3',
                     'display': None,
                     'value': 'someuser3'},
                ],
                'schemas': ['urn:ietf:params:scim:schemas:core:2.0:Group'],
            }
        )

    def test_schema_and_id_in_groups_put(self):
        r = self.json_post(
            '/scim/Groups',
            json_data={
                'externalId': 'jy-CUR-1236-teachers',
                'displayName': 'ITKP106 2019-09-09--2019-12-20: Luento 1: Opettajat',
                'members': add_name_parts([
                    {'value': 'someone', 'display': 'Sisu User'},
                ]),
            }, auth=a,
            expect_status=201,
        )
        r.pop('meta')
        r['members'][0]['display'] = 'Changed Name'
        self.json_put(
            '/scim/Groups/jy-CUR-1236-teachers',
            json_data=r,
            auth=a,
        )

    def test_duplicate_email(self):
        self.json_post(
            '/scim/Groups',
            json_data={
                'externalId': 'jy-CUR-1234-teachers',
                'displayName': 'ITKP102 2019-09-09--2019-12-20: Luento 1: Opettajat',
                'members': add_name_parts([
                    {'value': 'someone', 'display': 'Sisu User', 'email': 'zzz@example.com'},
                    {'value': 'someone2', 'display': 'Sisu User', 'email': 'zzz@example.com'},
                ]),
            }, auth=a,
            **scim_error("The users do not have distinct emails."),
        )
        self.json_post(
            '/scim/Groups',
            json_data={
                'externalId': 'jy-CUR-1234-teachers',
                'displayName': 'ITKP103 2019-09-09--2019-12-20: Luento 1: Opettajat',
                'members': add_name_parts([
                    {'value': 'someone', 'display': 'Sisu User', 'email': 'zzz@example.com'},
                ]),
            }, auth=a,
            expect_status=201,
        )
        self.json_post(
            '/scim/Groups',
            json_data={
                'externalId': 'jy-CUR-1239-teachers',
                'displayName': 'ITKP103 2019-09-09--2019-12-20: Luento 2: Opettajat',
                'members': add_name_parts([
                    {'value': 'someone2', 'display': 'Sisu User', 'email': 'zzz@example.com'},
                ]),
            }, auth=a,
            **scim_error("Key (email)=(zzz@example.com) already exists. Conflicting username is: someone2"),
        )

        User.create_with_group(name='xxx@example.com', real_name='Some Guy', email='xxx@example.com',
                               origin=UserOrigin.Email)
        db.session.commit()
        # Email user can be upgraded
        self.json_post(
            '/scim/Groups',
            json_data={
                'externalId': 'jy-CUR-1240-teachers',
                'displayName': 'ITKP108 2019-09-09--2019-12-20: Luento 1: Opettajat',
                'members': add_name_parts([
                    {'value': 'bbb', 'display': 'Sisu User', 'email': 'xxx@example.com'},
                ]),
            }, auth=a,
            expect_status=201,
        )
        self.json_post(
            '/scim/Groups',
            json_data={
                'externalId': 'jy-CUR-1241-teachers',
                'displayName': 'ITKP109 2019-09-09--2019-12-20: Luento 1: Opettajat',
                'members': add_name_parts([
                    {'value': 'ccc', 'display': 'Sisu User', 'email': 'xxx@example.com'},
                ]),
            }, auth=a,
            **scim_error("Key (email)=(xxx@example.com) already exists. Conflicting username is: ccc"),
        )

    def test_duplicate_usernames(self):
        self.json_post(
            '/scim/Groups',
            json_data={
                'externalId': 'jy-CUR-1235-teachers',
                'displayName': 'ITKP104 2019-09-09--2019-12-20: Luento 1: Opettajat',
                'members': add_name_parts([
                    {'value': 'aaa', 'display': 'Sisu User', 'email': 'aaa@example.com'},
                    {'value': 'aaa', 'display': 'Sisu User', 'email': 'aaa2@example.com'},
                ]),
            }, auth=a,
            **scim_error("The users do not have distinct usernames."),
        )

    def test_inconsistent_name(self):
        # self.json_post(
        #     '/scim/Groups',
        #     json_data={
        #         'externalId': 'inconsistent',
        #         'displayName': 'inconsistent',
        #         'members': [
        #             {
        #                 'value': 'aaa',
        #                 'display': 'John Matt Henry Doe',
        #                 'email': 'aaa2@example.com',
        #                 'name': {
        #                     'givenName': 'John',
        #                     'middleName': 'Matt Henryx',
        #                     'familyName': 'Doe',
        #                 },
        #             },
        #         ],
        #     }, auth=a,
        #     **scim_error("The display attribute 'John Matt Henry Doe' is inconsistent with "
        #                  "the name attributes: given='John', middle='Matt Henryx', "
        #                  "family='Doe'."),
        # )
        self.json_post(
            '/scim/Groups',
            json_data={
                'externalId': 'jy-CUR-1235-teachers',
                'displayName': 'ITKP105 2019-09-09--2019-12-20: Luento 1: Opettajat',
                'members': [
                    {
                        'value': 'aaa',
                        'display': 'John Doe',
                        'email': 'aaa2@example.com',
                        'name': {
                            'givenName': 'John',
                            'middleName': None,
                            'familyName': 'Doex',
                        },
                    },
                ],
            }, auth=a,
            **scim_error("The display attribute 'John Doe' is inconsistent with the name "
                         "attributes: given='John', middle='None', family='Doex'."),
        )
        self.json_post(
            '/scim/Groups',
            json_data={
                'externalId': 'jy-CUR-1235-teachers',
                'displayName': 'ITKP105 2019-09-09--2019-12-20: Luento 1: Opettajat',
                'members': [
                    {
                        'value': 'aaa',
                        'display': 'John Doe Matt',
                        'email': 'aaa2@example.com',
                        'name': {
                            'givenName': 'Matt',
                            'middleName': 'John',
                            'familyName': 'Doe',
                        },
                    },
                ],
            }, auth=a,
            **scim_error("The display attribute 'John Doe Matt' is inconsistent with the "
                         "name attributes: given='Matt', middle='John', family='Doe'."),
        )
        self.json_post(
            '/scim/Groups',
            json_data={
                'externalId': 'jy-CUR-1235-teachers',
                'displayName': 'ITKP105 2019-09-09--2019-12-20: Luento 1: Opettajat',
                'members': [
                    {
                        'value': 'aaa',
                        'display': 'John Matt Doe',
                        'email': 'aaa2@example.com',
                        'name': {
                            'givenName': 'Matt',
                            'middleName': 'John',
                            'familyName': 'Doe',
                        },
                    },
                ],
            }, auth=a,
            expect_status=201,
        )

    def test_potential_groups(self):
        sent_mails_in_testing.clear()
        responsible_teachers = (
            'jy-CUR-4668-responsible-teachers', 'ITKP102 P1 2019-09-09--2019-12-20: Rooli - responsible-teacher',
            ['urt-1', 'urt-2'])
        entries = [
            ('jy-CUR-4668-administrative-persons', 'ITKP102 P1 2019-09-09--2019-12-20: Rooli - administrative-person',
             ['uap-1', 'uap-2']),
            responsible_teachers,
            ('jy-CUR-4668-teachers', 'ITKP102 P1 2019-09-09--2019-12-20: Rooli - teacher',
             ['ut-1', 'ut-2']),
            ('jy-CUR-4668-students', 'ITKP102 P1 2019-09-09--2019-12-20: Kaikki opiskelijat',
             ['us-1', 'us-2']),

            ('jy-CUR-4668-jy-studysubgroup-9515-teachers', 'ITKP102 2019-09-09--2019-12-20: Luento 1: Opettajat',
             ['u9515-t1', 'u9515-t2']),
            ('jy-CUR-4668-jy-studysubgroup-9516-teachers', 'ITKP102 2019-09-09--2019-12-20: Luento 2: Opettajat',
             ['u9516-t1', 'u9516-t2']),
            ('jy-CUR-4668-studysubgroup-teachers', 'ITKP102 P1 2019-09-09--2019-12-20: Opetusryhmien opettajat',
             ['ussg-t1', 'ussg-t2']),

            ('jy-CUR-4668-jy-studysubgroup-9515-students', 'ITKP102 2019-09-09--2019-12-20: Luento 1: Opiskelijat',
             ['u9515-s1', 'u9515-s2']),
            ('jy-CUR-4668-jy-studysubgroup-9516-students', 'ITKP102 2019-09-09--2019-12-20: Luento 2: Opiskelijat',
             ['u9516-s1', 'u9516-s2']),
            ('jy-CUR-4668-studysubgroup-students', 'ITKP102 P1 2019-09-09--2019-12-20: Opetusryhmien Opiskelijat',
             ['ussg-s1', 'ussg-s2']),
        ]
        for (external_id, display_name, users) in entries:
            self.json_post(
                '/scim/Groups', {
                    'externalId': external_id,
                    'displayName': display_name,
                    'members': [
                        {'value': u, 'display': u, 'email': f'{u}@example.com'} for u in users
                    ],
                },
                auth=a,
                expect_status=201,
            )

        # Make sure the sisugroup document exists and has proper settings.
        d = DocEntry.find_by_path('groups/2019/itkp102/09/sisugroups')
        self.assertEqual({
            'global_plugin_attrs': {
                'all': {
                    'sisugroups': 'jy-CUR-4668',
                }
            },
            'macros': {
                'course': 'ITKP102 P1 2019-09-09--2019-12-20',
            },
            'preamble': 'sisugroups',
        },
            d.document.get_settings().get_dict().values)
        self.assertEqual(
            {
                ('jy-CUR-4668-administrative-persons', AccessType.owner.value),
                ('jy-CUR-4668-responsible-teachers', AccessType.owner.value),
                ('jy-CUR-4668-teachers', AccessType.owner.value),
                ('jy-CUR-4668-studysubgroup-teachers', AccessType.owner.value),
            },
            {(ac.usergroup.external_id.external_id, ac.type) for ac in d.block.accesses})
        admin = UserGroup.get_admin_group().id
        self.assertEqual(admin, d.document.get_changelog().entries[0].group_id)
        self.login(username='ut-1')
        self.get(d.url)

        all_groups = [
            {'display_name': 'ITKP102 P1 2019-09-09--2019-12-20: Rooli - '
                             'administrative-person',
             'doc': None,
             'external_id': 'jy-CUR-4668-administrative-persons',
             'name': 'itkp102-19p1-administrative-persons'},
            {'display_name': 'ITKP102 P1 2019-09-09--2019-12-20: Rooli - responsible-teacher',
             'doc': None,
             'external_id': 'jy-CUR-4668-responsible-teachers',
             'name': 'itkp102-19p1-responsible-teachers'},
            {'display_name': 'ITKP102 P1 2019-09-09--2019-12-20: Rooli - teacher',
             'doc': None,
             'external_id': 'jy-CUR-4668-teachers',
             'name': 'itkp102-19p1-teachers'},
            {'display_name': 'ITKP102 P1 2019-09-09--2019-12-20: Kaikki opiskelijat',
             'doc': None,
             'external_id': 'jy-CUR-4668-students',
             'name': 'itkp102-19p1-students'},
            {'display_name': 'ITKP102 2019-09-09--2019-12-20: Luento 1: Opettajat',
             'doc': None,
             'external_id': 'jy-CUR-4668-jy-studysubgroup-9515-teachers',
             'name': 'itkp102-190909-luento-1-teachers'},
            {'display_name': 'ITKP102 2019-09-09--2019-12-20: Luento 2: Opettajat',
             'doc': None,
             'external_id': 'jy-CUR-4668-jy-studysubgroup-9516-teachers',
             'name': 'itkp102-190909-luento-2-teachers'},
            {'display_name': 'ITKP102 P1 2019-09-09--2019-12-20: Opetusryhmien opettajat',
             'doc': None,
             'external_id': 'jy-CUR-4668-studysubgroup-teachers',
             'name': 'itkp102-19p1-studysubgroup-teachers'},
            {'display_name': 'ITKP102 2019-09-09--2019-12-20: Luento 1: Opiskelijat',
             'doc': None,
             'external_id': 'jy-CUR-4668-jy-studysubgroup-9515-students',
             'name': 'itkp102-190909-luento-1-students'},
            {'display_name': 'ITKP102 2019-09-09--2019-12-20: Luento 2: Opiskelijat',
             'doc': None,
             'external_id': 'jy-CUR-4668-jy-studysubgroup-9516-students',
             'name': 'itkp102-190909-luento-2-students'},
            {'display_name': 'ITKP102 P1 2019-09-09--2019-12-20: Opetusryhmien Opiskelijat',
             'doc': None,
             'external_id': 'jy-CUR-4668-studysubgroup-students',
             'name': 'itkp102-19p1-studysubgroup-students'}]
        self.check_potential_groups(
            'urt-1', all_groups)
        self.check_potential_groups(
            'uap-1', all_groups)
        self.check_potential_groups(
            'ussg-t1', all_groups)
        self.check_potential_groups(
            'u9515-s1', [])
        self.check_potential_groups(
            'us-1', [])
        self.check_potential_groups(
            'u9515-t1',
            [{'display_name': 'ITKP102 2019-09-09--2019-12-20: Luento 1: Opettajat',
              'doc': None,
              'external_id': 'jy-CUR-4668-jy-studysubgroup-9515-teachers',
              'name': 'itkp102-190909-luento-1-teachers'},
             {'display_name': 'ITKP102 2019-09-09--2019-12-20: Luento 1: Opiskelijat',
              'doc': None,
              'external_id': 'jy-CUR-4668-jy-studysubgroup-9515-students',
              'name': 'itkp102-190909-luento-1-students'}])

        self.check_no_group_access('us-1', ['jy-CUR-4668-students'])
        self.check_no_group_access('u9515-t1', ['jy-CUR-4668-students', 'jy-CUR-4668-teachers'])
        self.check_no_group_access(
            'u9515-t1',
            ['jy-CUR-4668-students', 'jy-CUR-4668-jy-studysubgroup-9515-students'],
            no_access_expected=['jy-CUR-4668-students'],
        )

        self.login(username='u9515-t1')
        self.json_post(
            '/sisu/createGroupDocs', json_data=[
                {'externalId': 'jy-CUR-4668-jy-studysubgroup-9515-students'},
                {'externalId': 'jy-CUR-4668-jy-studysubgroup-9515-teachers', 'name': 'teachers'},
            ],
            expect_status=400,
            expect_content='Usergroup must contain at least one digit and one letter and must not have special chars.',
            json_key='error',
        )

        r = self.json_post(
            '/sisu/createGroupDocs', json_data=[
                {'externalId': 'jy-CUR-4668-jy-studysubgroup-9515-students'},
                {'externalId': 'jy-CUR-4668-jy-studysubgroup-9515-teachers', 'name': 'teachers1'},
            ],
        )
        r['created'].sort(key=itemgetter('path'))
        self.assertEqual('groups/2019/itkp102/09/itkp102-190909-luento-1-students', r['created'][0]['path'])
        self.assertEqual('groups/2019/itkp102/09/teachers1', r['created'][1]['path'])

        r = self.json_post(
            '/sisu/createGroupDocs', json_data=[
                {'externalId': 'jy-CUR-4668-jy-studysubgroup-9515-students', 'name': 'students1'},
                {'externalId': 'jy-CUR-4668-jy-studysubgroup-9515-teachers', 'name': 'teachers2'},
            ],
        )
        r['updated'].sort(key=itemgetter('path'))
        self.assertEqual('groups/2019/itkp102/09/teachers2', r['updated'][1]['path'])
        self.assertEqual('groups/2019/itkp102/09/students1', r['updated'][0]['path'])
        self.assertEqual('teachers2', r['updated'][1]['title'])
        self.assertEqual('students1', r['updated'][0]['title'])
        self.assertEqual([], r['created'])

        r = self.json_post(
            '/sisu/createGroupDocs', json_data=[
                {'externalId': 'jy-CUR-4668-jy-studysubgroup-9515-students', 'name': 'students1'},
                {'externalId': 'jy-CUR-4668-jy-studysubgroup-9515-teachers', 'name': 'teachers2'},
            ],
        )
        self.assertEqual([], r['created'])
        self.assertEqual([], r['updated'])

        d = DocEntry.find_by_path('groups/2019/itkp102/09/students1')
        self.assertEqual(
            {
                ('jy-CUR-4668-administrative-persons', AccessType.owner.value),
                ('jy-CUR-4668-jy-studysubgroup-9515-teachers', AccessType.owner.value),
                ('jy-CUR-4668-responsible-teachers', AccessType.owner.value),
                ('jy-CUR-4668-studysubgroup-teachers', AccessType.owner.value),
                ('jy-CUR-4668-teachers', AccessType.owner.value),
            },
            {(ac.usergroup.external_id.external_id, ac.type) for ac in d.block.accesses})
        self.assertEqual({
            'macros': {
                'group': 'students1',
                'fields': ['info'],
                'maxRows': '40em',
                'sisugroup': 'jy-CUR-4668-jy-studysubgroup-9515-students',
            },
        },
            d.document.get_settings().get_dict().values)
        self.assertEqual(admin, d.document.get_changelog().entries[0].group_id)
        self.json_post(
            '/sisu/createGroupDocs', json_data=[
                {'externalId': 'jy-CUR-4668-jy-studysubgroup-9515-students', 'name': 'teachers1'},
                {'externalId': 'jy-CUR-4668-jy-studysubgroup-9515-teachers', 'name': 'teachers1'},
            ],
            expect_status=403,
            expect_content="Item with a same name already exists.",
            json_key='error',
        )

        # Make sure there won't be duplicate mails for responsible teachers.
        for (external_id, display_name, users) in [responsible_teachers]:
            self.json_put(
                f'/scim/Groups/{external_id}', {
                    'externalId': external_id,
                    'displayName': display_name,
                    'members': [
                        {'value': u, 'display': u, 'email': f'{u}@example.com'} for u in users
                    ],
                },
                auth=a,
            )

        self.assertEqual([
            {'mail_from': 'tim@jyu.fi',
             'msg': 'Kurssin ITKP102 Sisussa olevat ryhmät on kopioitu TIMiin. Ne '
                    'löytyvät dokumentista:\n'
                    '\n'
                    'http://localhost/view/groups/2019/itkp102/09/sisugroups\n'
                    '\n'
                    'Dokumentissa on ohjeet ryhmien käyttämiseen TIMissä.\n'
                    '\n'
                    'Tämä viesti tulee kaikille kurssin vastuuopettajille.',
             'rcpt': 'urt-1@example.com',
             'reply_to': 'no-reply@tim.jyu.fi',
             'subject': 'Kurssin ITKP102 Sisu-ryhmät on kopioitu TIMiin'},
            {'mail_from': 'tim@jyu.fi',
             'msg': 'Kurssin ITKP102 Sisussa olevat ryhmät on kopioitu TIMiin. Ne '
                    'löytyvät dokumentista:\n'
                    '\n'
                    'http://localhost/view/groups/2019/itkp102/09/sisugroups\n'
                    '\n'
                    'Dokumentissa on ohjeet ryhmien käyttämiseen TIMissä.\n'
                    '\n'
                    'Tämä viesti tulee kaikille kurssin vastuuopettajille.',
             'rcpt': 'urt-2@example.com',
             'reply_to': 'no-reply@tim.jyu.fi',
             'subject': 'Kurssin ITKP102 Sisu-ryhmät on kopioitu TIMiin'}],
            sorted(sent_mails_in_testing, key=itemgetter('rcpt')))
        self.assertIn(UserGroup.get_teachers_group(), User.get_by_name('urt-1').groups)
        self.assertNotIn(UserGroup.get_teachers_group(), User.get_by_name('us-1').groups)

    def test_scim_group_manual_member_update(self):
        eid = 'jy-CUR-7777-teachers'
        self.json_post(
            '/scim/Groups', {
                'externalId': eid,
                'displayName': 'ITKP102 2020-09-09--2020-12-20: Rooli - teacher',
                'members': [
                    {'value': u, 'display': u, 'email': f'{u}@example.com'} for u in ['abc']
                ],
            },
            auth=a,
            expect_status=201,
        )
        u, _ = User.create_with_group('anon@example.com', 'Anon User', 'anon@example.com', origin=UserOrigin.Email)
        u2, _ = User.create_with_group('mameikal', 'Matti Meikäläinen', 'mameikal@example.com',
                                       origin=UserOrigin.Korppi)
        self.make_admin(u)
        self.login(username=u.name)
        ug = UserGroup.get_by_external_id('jy-CUR-7777-teachers')
        self.json_post(
            f'/groups/addmember/{ug.name}', {'names': [u.name, u2.name]}
        )

        # The SCIM routes must not report the manually added users.
        r = self.get(
            f'/scim/Groups/{eid}',
            auth=a,
        )
        self.assertEqual(
            [{'$ref': 'http://localhost/scim/Users/abc', 'display': 'abc', 'value': 'abc'}],
            r['members'],
        )

        self.json_put(
            f'/scim/Groups/{eid}', {
                'externalId': eid,
                'displayName': 'ITKP102 2020-09-09--2020-12-20: Rooli - teacher',
                'members': [
                    {'value': u, 'display': u, 'email': f'{u}@example.com'} for u in ['abc']
                ],
            },
            auth=a,
        )

        # The manually added user should not get deleted on SCIM update.
        ug = UserGroup.get_by_external_id(eid)
        self.assertEqual(3, len(ug.users))

        self.json_post(
            f'/groups/removemember/{ug.name}', {'names': ['abc']},
            expect_status=400,
            expect_content='Cannot remove not-manually-added users from Sisu groups.',
            json_key='error',
        )
        self.json_post(
            f'/groups/removemember/{ug.name}', {'names': ['anon@example.com']}
        )
        self.json_post(
            f'/groups/removemember/{ug.name}', {'names': ['mameikal']}
        )

    def check_no_group_access(self, username: str, externalids: List[str], no_access_expected=None):
        self.login(username=username)
        if no_access_expected is None:
            no_access_expected = externalids
        self.json_post(
            '/sisu/createGroupDocs', json_data=[
                {'externalId': externalid} for externalid in externalids
            ],
            expect_status=403,
            expect_content=f"You don't have access to all the requested groups: {seq_to_str(no_access_expected)}",
            json_key='error',
        )

    def check_potential_groups(self, uname: str, expected):
        self.login(username=uname)

        r = self.get(
            '/sisu/getPotentialGroups',
        )
        for g in r:
            g.pop('id')
        self.assertEqual(
            expected, r)


def scim_error(msg: str, code=422):
    return dict(
        expect_status=code,
        expect_content={
            'detail': msg,
            'schemas': ['urn:ietf:params:scim:api:messages:2.0:Error'],
            'status': str(code)},
    )
