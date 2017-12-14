from timApp.tests.db.timdbtest import TEST_USER_2_ID
from timApp.tests.server.timroutetest import TimRouteTest
from timApp.timdb.userutils import grant_access


class ManageTest(TimRouteTest):

    def test_manage(self):
        self.login_test1()
        doc = self.create_doc(initial_par='testing manage').document
        self.get('/manage/' + str(doc.doc_id))
        self.get('/notify/' + str(doc.doc_id),
                 expect_content={"email_doc_modify": False,
                                 "email_comment_add": False,
                                 "email_comment_modify": False
                                 })

        for new_settings in {"email_doc_modify": True,
                             "email_comment_add": False,
                             "email_comment_modify": False
                             }, {"email_doc_modify": False,
                                 "email_comment_add": True,
                                 "email_comment_modify": True
                                 }:
            self.json_post('/notify/' + str(doc.doc_id), new_settings)
            self.get('/notify/' + str(doc.doc_id), expect_content=new_settings)
        self.login_test2()
        self.get('/manage/' + str(doc.doc_id), expect_status=403)
        timdb = self.get_db()
        grant_access(self.get_test_user_2_group_id(), doc.doc_id, 'manage')
        self.get('/manage/' + str(doc.doc_id))

    def test_item_rights(self):
        self.login_test1()
        pf = self.current_user.get_personal_folder()
        d1 = self.create_doc(self.get_personal_item_path('d1'))
        d2 = self.create_doc(self.get_personal_item_path('x/d2'))
        f = Folder.find_by_path(self.get_personal_item_path('x'))
        self.login_test2()
        d3 = self.create_doc()
        new_alias = f'{f.path}/z/y'
        self.json_put(f'/alias/{d3.id}/{new_alias}', expect_status=403)
        self.current_user.grant_access(f.id, 'view')
        self.json_put(f'/alias/{d3.id}/{new_alias}', expect_status=403)
        self.current_user.grant_access(f.id, 'edit')
        self.json_put(f'/alias/{d3.id}/{new_alias}')

        new_alias_2 = f'{pf.path}/z'
        self.json_post(f'/alias/{new_alias}',
                       {'new_name': new_alias_2},
                       expect_status=403,
                       json_key='error',
                       expect_content="You don't have permission to write to the destination folder.")
        self.current_user.grant_access(pf.id, 'view')
        self.json_post(f'/alias/{new_alias}',
                       {'new_name': new_alias_2},
                       expect_status=403,
                       json_key='error',
                       expect_content="You don't have permission to write to the destination folder.")
        self.current_user.grant_access(pf.id, 'edit')
        self.json_post(f'/alias/{new_alias}',
                       {'new_name': new_alias_2})

        self.current_user.remove_access(pf.id, 'edit')
        db.session.commit()
        self.json_post(f'/alias/{new_alias_2}',
                       {'new_name': new_alias},
                       expect_status=403,
                       json_key='error',
                       expect_content="You don't have permission to write to the source folder.")
