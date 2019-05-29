from timApp.sisu.scim import DELETED_GROUP_PREFIX, CUMULATIVE_GROUP_PREFIX, SISU_GROUP_PREFIX
from timApp.tests.server.timroutetest import TimRouteTest
from timApp.user.usergroup import UserGroup

error_422 = {'detail': 'The request was well-formed but was unable to be followed due to semantic errors.',
             'schemas': ['urn:ietf:params:scim:api:messages:2.0:Error'],
             'status': '422'}


class ScimTest(TimRouteTest):
    def test_scim(self):
        self.json_post('/scim/Groups', expect_status=401, expect_content='This action requires authentication.')
        self.json_post(
            '/scim/Groups',
            auth=('cat', 'dog'),
            expect_status=401,
            expect_content='Incorrect username or password.',
        )
        a = ('t', 'pass')
        self.json_post('/scim/Groups', auth=a, expect_status=422, expect_content=error_422)
        r = self.json_post(
            '/scim/Groups',
            json_data={
                'externalId': 'sisu-something',
                'displayName': 'Sisu something',
                'members': [
                    {'value': 'sisuuser', 'display': 'Sisu User'},
                    {'value': 'sisuuser3', 'display': 'Sisu User 3'},
                ],
            }, auth=a,
            expect_status=201,
            expect_contains={
                'displayName': 'Sisu something',
                'id': 'sisu-something',
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
        self.assertIsNone(UserGroup.get_by_name('sisu-something'))
        create_stamp = r['meta']['created']
        self.assertEqual(create_stamp, r['meta']['lastModified'])
        group_id = r['id']
        self.json_post(
            '/scim/Groups',
            json_data={
                'externalId': 'sisu-something',
                'displayName': 'Sisu something',
                'members': [],
            }, auth=a,
            expect_status=409,
        )
        self.json_put(
            f'/scim/Groups/{group_id}', auth=a, expect_status=422,
            expect_content={
                'detail': 'JSON payload missing.',
                'schemas': ['urn:ietf:params:scim:api:messages:2.0:Error'],
                'status': '422',
            })
        self.json_put(
            f'/scim/Groups/{group_id}', json_data={}, auth=a, expect_status=422,
            expect_content={'detail': '{"displayName": ["Missing data for required field."], '
                                      '"externalId": ["Missing data for required field."], "members": '
                                      '["Missing data for required field."]}',
                            'schemas': ['urn:ietf:params:scim:api:messages:2.0:Error'],
                            'status': '422'})
        self.json_put(f'/scim/Groups/xxx', auth=a, expect_status=404)
        self.json_put(f'/scim/Groups/group-999', auth=a, expect_status=404)
        self.json_put(f'/scim/Groups/group-xxx', auth=a, expect_status=404)
        self.json_put(
            f'/scim/Groups/{group_id}',
            json_data={
                'externalId': 'sisu-something',
                'displayName': 'Sisu something',
                'members': [
                    {
                        'display': 'Sisu User',
                        'value': 'sisuuser',
                    },
                    {
                        'display': 'Sisu User 2',
                        'value': 'sisuuser2',
                    },
                ],
            }, auth=a,
            expect_content={
                'displayName': 'Sisu something',
                'id': group_id,
                'members': [{'$ref': 'http://localhost/scim/Users/sisuuser',
                             'display': 'Sisu User',
                             'value': 'sisuuser'},
                            {'$ref': 'http://localhost/scim/Users/sisuuser2',
                             'display': 'Sisu User 2',
                             'value': 'sisuuser2'}],
                'meta': {'created': create_stamp,
                         'lastModified': create_stamp,
                         'location': 'http://localhost/scim/Groups/sisu-something',
                         'resourceType': 'Group'},
                'schemas': ['urn:ietf:params:scim:schemas:core:2.0:Group']},
        )

        r = self.get(f'/scim/Groups', auth=a, query_string={'filter': 'externalId sw sisu-'})
        self.assertEqual(
            {
                'Resources': [
                    {
                        'id': group_id,
                        'meta': {
                            'created': create_stamp,
                            'lastModified': create_stamp,
                            'location': 'http://localhost/scim/Groups/sisu-something',
                            'resourceType': 'Group',
                        },
                    },
                ],
                'schemas': ['urn:ietf:params:scim:api:messages:2.0:ListResponse'],
                'totalResults': 1,
            }, r)
        r = self.get(
            f'/scim/Groups', auth=a, query_string={'filter': 'asd sw sisu-'},
            expect_status=422,
            expect_content={
                'detail': 'Unsupported filter',
                'schemas': ['urn:ietf:params:scim:api:messages:2.0:Error'],
                'status': '422'})
        r = self.get(
            f'/scim/Groups/{group_id}',
            auth=a,
            expect_content={
                'displayName': 'Sisu something',
                'id': group_id,
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
                    'location': 'http://localhost/scim/Groups/sisu-something',
                    'resourceType': 'Group'},
                'schemas': [
                    'urn:ietf:params:scim:schemas:core:2.0:Group']},
        )
        self.json_delete(f'/scim/Groups/{group_id}', auth=a, expect_status=204)
        self.json_delete(f'/scim/Groups/{group_id}', auth=a, expect_status=404)
        g = UserGroup.get_by_name(f'{CUMULATIVE_GROUP_PREFIX}{SISU_GROUP_PREFIX}sisu-something')
        self.assertEqual(3, len(g.users.all()))
        deleted_group = UserGroup.get_by_name(f'{DELETED_GROUP_PREFIX}{SISU_GROUP_PREFIX}sisu-something')
        self.assertIsNotNone(deleted_group)

        r = self.json_post(
            '/scim/Groups',
            json_data={
                'externalId': 'sisu-something',
                'displayName': 'Sisu something',
                'members': [
                    {'value': 'sisuuser', 'display': 'Sisu User'},
                    {'value': 'sisuuser3', 'display': 'Sisu User 3'},
                ],
            }, auth=a,
            expect_status=201,
            expect_contains={
                'displayName': 'Sisu something',
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
        g = UserGroup.get_by_name(f'{DELETED_GROUP_PREFIX}sisu-something')
        self.assertIsNone(g)
