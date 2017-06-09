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
        grant_access(timdb.users.get_personal_usergroup_by_id(TEST_USER_2_ID), doc.doc_id, 'manage')
        self.get('/manage/' + str(doc.doc_id))
