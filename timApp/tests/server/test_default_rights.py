from operator import itemgetter

from dateutil import parser

from tests.server.timroutetest import TimRouteTest
from timdb.blocktypes import from_str, blocktypes
from timdb.models.docentry import DocEntry
from timdb.models.folder import Folder
from timdb.special_group_names import KORPPI_GROUPNAME


class DefaultRightTest(TimRouteTest):
    def test_document_default_rights(self):
        self.login_test1()
        doc = self.create_doc().document
        timdb = self.get_db()
        docentry = DocEntry.query.filter_by(id=doc.doc_id).one()
        folder = docentry.parent

        users_folder = Folder.find_by_path('users')
        timdb.users.grant_default_access([timdb.users.get_korppi_group_id()], users_folder.id, 'view',
                                         blocktypes.DOCUMENT)

        # Make sure an exception won't be thrown if trying to add a right again
        acs = timdb.users.grant_default_access([timdb.users.get_korppi_group_id()], users_folder.id, 'view',
                                               blocktypes.DOCUMENT)
        for obj_type_str in ('document', 'folder'):
            obj_type = from_str(obj_type_str)
            def_rights = timdb.users.get_default_rights_holders(folder.id, obj_type)
            self.assertListEqual([], def_rights)

            rights_doc = folder.get_document(timdb.users.default_right_paths[obj_type])
            self.assertIsNone(rights_doc)

            self.json_put(
                '/defaultPermissions/{}/add/{}/{}/{}'.format(obj_type_str, folder.id,
                                                             'Anonymous users;testuser2',
                                                             'view'), {'type': 'always'},
                expect_content=self.ok_resp)

            def_rights = self.get('/defaultPermissions/{}/get/{}'.format(obj_type_str, folder.id),
                                  expect_status=200)
            default_rights = [{'access_name': 'view',
                               'access_type': 1,
                               'fullname': 'Test user 2',
                               'gid': 8,
                               'name': 'testuser2',
                               'duration': None,
                               'accessible_from': def_rights['grouprights'][0]['accessible_from'],
                               'accessible_to': None,
                               'duration_from': None,
                               'duration_to': None},
                              {'access_name': 'view',
                               'access_type': 1,
                               'fullname': None,
                               'gid': 2,
                               'name': 'Anonymous users',
                               'duration': None,
                               'accessible_from': def_rights['grouprights'][1]['accessible_from'],
                               'accessible_to': None,
                               'duration_from': None,
                               'duration_to': None}]
            self.maxDiff = None
            default_rights = sorted(default_rights, key=itemgetter('gid'))
            self.assertDictEqual(
                {'grouprights': default_rights},
                def_rights)
            for d in default_rights:
                d['accessible_from'] = parser.parse(d['accessible_from'])
                d['accessible_to'] = parser.parse(d['accessible_to']) if d['accessible_to'] else None
            rights_doc = folder.get_document(timdb.users.default_right_paths[obj_type])
            self.assertIsNotNone(rights_doc)

            if obj_type == blocktypes.DOCUMENT:
                new_doc = self.create_doc().document
                new_item_rights = timdb.users.get_rights_holders(new_doc.doc_id)
                default_rights.append(
                    {'gid': timdb.users.get_korppi_group_id(),
                     'name': KORPPI_GROUPNAME,
                     'access_type': 1,
                     'fullname': None,
                     'access_name': 'view',
                     'duration': None,
                     'accessible_from': acs[0].accessible_from,
                     'accessible_to': None,
                     'duration_from': None,
                     'duration_to': None
                     })
            elif obj_type == blocktypes.FOLDER:
                f = self.create_folder(self.current_user.get_personal_folder().path + '/asd', 'folder')
                new_item_rights = timdb.users.get_rights_holders(f['id'])
            else:
                raise Exception('error in test: object type should be document or folder')
            self.maxDiff = None
            new_item_rights = [right for right in new_item_rights if right['access_name'] != 'owner']
            self.assertListEqual(sorted(default_rights, key=itemgetter('gid', 'access_type')),
                                 sorted(new_item_rights, key=itemgetter('gid', 'access_type')))
            self.json_put('/defaultPermissions/{}/remove/{}/{}/{}'
                          .format(obj_type_str, folder.id,
                                  timdb.users.get_anon_group_id(),
                                  'view'),
                          expect_content=self.ok_resp)
