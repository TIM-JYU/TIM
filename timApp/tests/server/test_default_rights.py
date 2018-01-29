from operator import itemgetter

from dateutil import parser

from timApp.tests.server.timroutetest import TimRouteTest
from timApp.timdb.blocktypes import from_str, blocktypes
from timApp.timdb.models.docentry import DocEntry
from timApp.timdb.models.folder import Folder
from timApp.timdb.models.usergroup import UserGroup
from timApp.timdb.special_group_names import KORPPI_GROUPNAME
from timApp.timdb.tim_models import db
from timApp.timdb.userutils import get_anon_group_id, grant_default_access, default_right_paths


class DefaultRightTest(TimRouteTest):

    def test_document_default_rights(self):
        self.login_test1()
        doc = self.create_doc().document
        timdb = self.get_db()
        docentry = DocEntry.query.filter_by(id=doc.doc_id).one()
        folder = docentry.parent
        korppi_id = UserGroup.get_korppi_group().id
        users_folder = Folder.find_by_path('users')
        grant_default_access([korppi_id], users_folder.id, 'view',
                             blocktypes.DOCUMENT)

        # Make sure an exception won't be thrown if trying to add a right again
        acs = grant_default_access([korppi_id], users_folder.id, 'view',
                                   blocktypes.DOCUMENT)
        db.session.commit()
        anon_id = UserGroup.get_anonymous_group().id
        for obj_type_str in ('document', 'folder'):
            obj_type = from_str(obj_type_str)
            def_rights = timdb.users.get_default_rights_holders(folder.id, obj_type)
            self.assertListEqual([], def_rights)

            rights_doc = folder.get_document(default_right_paths[obj_type])
            self.assertIsNone(rights_doc)

            self.json_put(
                f'/defaultPermissions/{obj_type_str}/add/{folder.id}/{"Anonymous users;testuser2"}/{"view"}',
                {'type': 'always'},
                expect_content=self.ok_resp)

            def_rights = self.get(f'/defaultPermissions/{obj_type_str}/get/{folder.id}',
                                  expect_status=200)
            expected_default_rights = [{'access_name': 'view',
                                        'access_type': 1,
                                        'fullname': 'Test user 2',
                                        'gid': self.get_test_user_2_group_id(),
                                        'name': 'testuser2',
                                        'duration': None,
                                        'accessible_from': def_rights['grouprights'][0]['accessible_from'],
                                        'accessible_to': None,
                                        'duration_from': None,
                                        'duration_to': None},
                                       {'access_name': 'view',
                                        'access_type': 1,
                                        'fullname': None,
                                        'gid': anon_id,
                                        'name': 'Anonymous users',
                                        'duration': None,
                                        'accessible_from': def_rights['grouprights'][1]['accessible_from'],
                                        'accessible_to': None,
                                        'duration_from': None,
                                        'duration_to': None}]
            expected_default_rights = sorted(expected_default_rights, key=itemgetter('gid'))
            self.assertDictEqual(
                {'grouprights': expected_default_rights},
                def_rights)
            for d in expected_default_rights:
                d['accessible_from'] = parser.parse(d['accessible_from'])
                d['accessible_to'] = parser.parse(d['accessible_to']) if d['accessible_to'] else None
            rights_doc = folder.get_document(default_right_paths[obj_type])
            self.assertIsNotNone(rights_doc)

            if obj_type == blocktypes.DOCUMENT:
                new_doc = self.create_doc().document
                new_item_rights = timdb.users.get_rights_holders(new_doc.doc_id)
                expected_default_rights.append(
                    {'gid': korppi_id,
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
            new_item_rights = [right for right in new_item_rights if right['access_name'] != 'owner']
            self.assertListEqual(sorted(expected_default_rights, key=itemgetter('gid', 'access_type')),
                                 sorted(new_item_rights, key=itemgetter('gid', 'access_type')))
            self.json_put(f'/defaultPermissions/{obj_type_str}/remove/{folder.id}/{get_anon_group_id()}/{"view"}',
                          expect_content=self.ok_resp)
            def_rights = timdb.users.get_default_rights_holders(folder.id, obj_type)
            expected_default_rights = [r for r in expected_default_rights if r['gid'] not in (anon_id, korppi_id)]
            self.assertListEqual(expected_default_rights, def_rights)
