from timApp.tests.server.timroutetest import TimRouteTest
from timApp.folder.folder import Folder
from timApp.timdb.sqa import db
from timApp.user.userutils import grant_access


class ManageTest(TimRouteTest):

    def test_manage(self):
        self.login_test1()
        d = self.create_doc()
        self.get(f'/manage/{d.id}')
        self.get(f'/notify/{d.id}',
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
            self.json_post(f'/notify/{d.id}', new_settings)
            self.get(f'/notify/{d.id}', expect_content=new_settings)
        self.login_test2()
        self.get(f'/manage/{d.id}', expect_status=403)
        grant_access(self.get_test_user_2_group_id(), d.id, 'manage')
        self.get(f'/manage/{d.id}')

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
