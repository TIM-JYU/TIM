from timApp.auth.accesstype import AccessType
from timApp.document.docentry import DocEntry
from timApp.folder.folder import Folder
from timApp.tests.server.timroutetest import TimRouteTest
from timApp.timdb.sqa import db


class ManageTest(TimRouteTest):
    def test_manage(self):
        self.login_test1()
        d = self.create_doc()
        self.get(f"/manage/{d.id}")
        self.get(
            f"/notify/{d.id}",
            expect_content={
                "email_doc_modify": False,
                "email_comment_add": False,
                "email_comment_modify": False,
                "email_answer_add": False,
                "email_annotation_add": False,
                "email_annotation_modify": False,
            },
        )

        for new_settings in {
            "email_doc_modify": True,
            "email_comment_add": False,
            "email_comment_modify": False,
            "email_answer_add": False,
            "email_annotation_add": False,
            "email_annotation_modify": False,
        }, {
            "email_doc_modify": False,
            "email_comment_add": True,
            "email_comment_modify": True,
            "email_answer_add": True,
            "email_annotation_add": True,
            "email_annotation_modify": True,
        }:
            self.json_post(f"/notify/{d.id}", new_settings)
            self.get(f"/notify/{d.id}", expect_content=new_settings)
        self.login_test2()
        self.get(f"/manage/{d.id}", expect_status=403)
        self.test_user_2.grant_access(d, AccessType.manage)
        db.session.commit()
        self.get(f"/manage/{d.id}")

    def test_item_rights(self):
        self.login_test1()
        pf = self.current_user.get_personal_folder()
        d1 = self.create_doc(self.get_personal_item_path("d1"))
        d2 = self.create_doc(self.get_personal_item_path("x/d2"))
        f = Folder.find_by_path(self.get_personal_item_path("x"))
        self.login_test2()
        d3 = self.create_doc()
        new_alias = f"{f.path}/z/y"
        self.json_put(
            f"/alias/{d3.id}/{new_alias}",
            expect_status=403,
            expect_content="You cannot create documents in this folder.",
        )
        self.current_user.grant_access(f, AccessType.view)
        db.session.commit()
        self.json_put(
            f"/alias/{d3.id}/{new_alias}",
            expect_status=403,
            expect_content="You cannot create documents in this folder.",
        )
        self.current_user.grant_access(f, AccessType.edit)
        db.session.commit()
        self.json_put(f"/alias/{d3.id}/{new_alias}")

        new_alias_2 = f"{pf.path}/z"
        self.json_post(
            f"/alias/{new_alias}",
            {"new_name": new_alias_2},
            expect_status=403,
            expect_content="You don't have permission to write to the destination folder.",
        )
        self.current_user.grant_access(pf, AccessType.view)
        db.session.commit()
        self.json_post(
            f"/alias/{new_alias}",
            {"new_name": new_alias_2},
            expect_status=403,
            expect_content="You don't have permission to write to the destination folder.",
        )
        self.current_user.grant_access(pf, AccessType.edit)
        db.session.commit()
        self.json_post(f"/alias/{new_alias}", {"new_name": new_alias_2})

        self.current_user.remove_access(pf.id, "edit")
        db.session.commit()
        self.json_post(
            f"/alias/{new_alias_2}",
            {"new_name": new_alias},
            expect_status=200,
        )

    def test_alias_no_empty_path_part(self):
        self.login_test1()
        d = self.create_doc()
        self.json_post(
            f"/alias/{d.path}",
            {"new_name": self.current_user.get_personal_folder().path + "//foo"},
            expect_status=400,
            expect_content="The document path cannot have empty parts.",
        )

    def test_document_delete(self):
        self.login_test1()
        d = self.create_doc(self.get_personal_item_path("test"))
        self.json_put(f"/alias/{d.id}/{d.location}/alias")
        old_path = d.path
        s = d.short_name
        self.json_delete(f"/documents/{d.id}")
        d_deleted = DocEntry.find_by_path(f"roskis/{s}")
        self.assertIsNotNone(d_deleted)
        self.assertIsNone(DocEntry.find_by_path(old_path))
        self.json_delete(f"/documents/{d.id}")
        self.assertEqual(1, len(d.aliases))

        d = self.create_doc(self.get_personal_item_path("x/test"))
        s = d.short_name
        self.json_delete(f"/documents/{d.id}")
        d_deleted = DocEntry.find_by_path(f"roskis/{s}_1")
        self.assertIsNotNone(d_deleted)

    def test_shortname_public_toggle(self):
        self.login_test1()
        d = self.create_doc()
        self.json_put(f"/alias/{d.id}/{d.path}x")
        self.json_post(f"/alias/{d.path}x", {"new_name": d.path + "x", "public": False})

        self.json_post(
            f"/alias/{d.path}",
            {"new_name": d.path, "public": False},
            expect_status=400,
            expect_content="This is the only visible name for this document, so you cannot make it invisible.",
        )
