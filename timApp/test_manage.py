from timroutetest import TimRouteTest


class ManageTest(TimRouteTest):
    def test_manage(self):
        self.login_test1()
        doc = self.create_doc(initial_par='testing manage').document
        self.assertResponseStatus(self.app.get('/manage/' + str(doc.doc_id)))
        self.assertDictResponse({"email_doc_modify": False,
                                 "email_comment_add": False,
                                 "email_comment_modify": False
                                 },
                                self.app.get('/notify/' + str(doc.doc_id)))

        for new_settings in {"email_doc_modify": True,
                             "email_comment_add": False,
                             "email_comment_modify": False
                             }, {"email_doc_modify": False,
                                 "email_comment_add": True,
                                 "email_comment_modify": True
                                 }:
            self.assertDictResponse(self.ok_resp,
                                    self.json_post('/notify/' + str(doc.doc_id), new_settings))
            self.assertDictResponse(new_settings,
                                    self.app.get('/notify/' + str(doc.doc_id)))
