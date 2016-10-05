from operator import itemgetter

from timdb.blocktypes import from_str, blocktypes
from timdb.models.docentry import DocEntry
from timdb.models.folder import Folder
from timdb.special_group_names import ANONYMOUS_GROUPNAME, KORPPI_GROUPNAME
from timroutetest import TimRouteTest


class DefaultRightTest(TimRouteTest):
    def test_document_default_rights(self):
        self.login_test1()
        doc = self.create_doc().document
        timdb = self.get_db()
        docentry = DocEntry.query.filter_by(id=doc.doc_id).one()
        folder = docentry.get_parent()

        users_folder = Folder.find_by_full_path('users')
        timdb.users.grant_default_access(timdb.users.get_korppi_group_id(), users_folder.id, 'view',
                                         blocktypes.DOCUMENT)

        # Make sure an exception won't be thrown if trying to add a right again
        timdb.users.grant_default_access(timdb.users.get_korppi_group_id(), users_folder.id, 'view',
                                         blocktypes.DOCUMENT)

        for obj_type_str in ('document', 'folder'):
            obj_type = from_str(obj_type_str)
            def_rights = timdb.users.get_default_rights_holders(folder.id, obj_type)
            self.assertListEqual([], def_rights)

            rights_doc = folder.get_document(timdb.users.default_right_paths[obj_type])
            self.assertIsNone(rights_doc)

            self.assertDictResponse(self.ok_resp,
                                    self.json_put(
                                        '/defaultPermissions/{}/add/{}/{}/{}'.format(obj_type_str, folder.id,
                                                                                     'Anonymous users',
                                                                                     'view'), expect_status=200)
                                    )
            def_rights = self.get('/defaultPermissions/{}/get/{}'.format(obj_type_str, folder.id), as_json=True,
                                  expect_status=200)
            default_rights = [{'gid': timdb.users.get_anon_group_id(), 'name': ANONYMOUS_GROUPNAME, 'access_type': 1,
                               'access_name': 'view'}]
            self.assertDictEqual(
                {'grouprights': default_rights},
                def_rights)
            rights_doc = folder.get_document(timdb.users.default_right_paths[obj_type])
            self.assertIsNotNone(rights_doc)

            if obj_type == blocktypes.DOCUMENT:
                new_doc = self.create_doc().document
                new_item_rights = timdb.users.get_rights_holders(new_doc.doc_id)
                default_rights.append(
                    {'gid': timdb.users.get_korppi_group_id(), 'name': KORPPI_GROUPNAME, 'access_type': 1,
                     'access_name': 'view'})
            elif obj_type == blocktypes.FOLDER:
                f = self.json_post('/createFolder',
                                   {"name": 'users/testuser1/asd',
                                    "owner": self.current_user_name()}, as_json=True)
                new_item_rights = timdb.users.get_rights_holders(f['id'])
            else:
                raise Exception('error in test: object type should be document or folder')
            self.assertListEqual(sorted(default_rights, key=itemgetter('gid', 'access_type')),
                                 sorted(new_item_rights, key=itemgetter('gid', 'access_type')))
