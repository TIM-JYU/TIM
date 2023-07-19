"""
Tested routes from velp.py:
  /<int:doc_id>/get_default_velp_group
  /<int:doc_id>/get_velps
  /<int:doc_id>/get_velp_groups
  /<int:doc_id>/get_velp_labels
  /add_velp
  /<int:doc_id>/update_velp
  /add_velp_label
  /update_velp_label
  /<int:doc_id>/create_velp_group
  /<int:doc_id>/create_default_velp_group
"""
import json

from sqlalchemy import select

from timApp.auth.accesshelper import get_doc_or_abort
from timApp.auth.accesstype import AccessType
from timApp.auth.get_user_rights_for_item import get_user_rights_for_item
from timApp.auth.sessioninfo import get_current_user_object
from timApp.document.docentry import DocEntry
from timApp.document.docinfo import DocInfo
from timApp.folder.folder import Folder
from timApp.tests.server.timroutetest import TimRouteTest
from timApp.timdb.sqa import db
from timApp.user.usergroup import UserGroup
from timApp.util.utils import get_current_time
from timApp.velp.annotation import Annotation
from timApp.velp.velp import create_new_velp, DEFAULT_PERSONAL_VELP_GROUP_NAME
from timApp.velp.velp_models import (
    VelpGroup,
    VelpInGroup,
    VelpGroupSelection,
    VelpGroupDefaults,
    VelpGroupsInDocument,
)
from timApp.velp.velpgroups import get_groups_from_document_table


class VelpTest(TimRouteTest):
    def test_velp(self):
        self.login_test1()
        user_folder = self.current_user.get_personal_folder().path  # users/testuser1
        deeper_folder = f"{user_folder}/test"  # users/testuser1/test
        test_doc1 = f"{user_folder}/test1"
        test_doc2 = f"{user_folder}/test2"
        test_doc3 = f"{deeper_folder}/test3"
        root_velp_group_folder = "velp-groups"
        user_velp_group_folder = (
            f"{user_folder}/velp-groups"  # users/testuser1/velp groups
        )
        deep_velp_group_folder = (
            f"{deeper_folder}/velp-groups"  # users/testuser1/test/velp groups
        )
        t1g = self.test_user_1.get_personal_group()
        t2g = self.test_user_2.get_personal_group()
        Folder.create(user_folder, t1g)
        Folder.create(deeper_folder, t1g)
        Folder.create(root_velp_group_folder, t1g)
        Folder.create(user_velp_group_folder, t1g)
        Folder.create(deep_velp_group_folder, t1g)
        doc1 = DocEntry.create(
            test_doc1, t1g
        ).document  # users/testuser1/test1 #owner: testuser1
        doc1_id = doc1.doc_id
        doc2 = DocEntry.create(
            test_doc2, t2g
        ).document  # users/testuser1/test2 #owner: testuser2
        doc2_id = doc2.doc_id
        doc3 = DocEntry.create(
            test_doc3, t1g
        ).document  # users/testuser1/test/test3 #owner: testuser1
        doc3_id = doc3.doc_id
        db.session.commit()

        # Try to get velp groups for document that has none
        resp = self.get(f"/{doc1_id}/get_default_velp_group")
        self.assertEqual(-1, resp["id"])
        resp = self.get(f"/{doc1_id}/get_velp_groups")
        self.assertEqual(len(resp), 0)

        # Create a default velp group for document
        j = self.json_post(f"/{doc1_id}/create_default_velp_group")
        self.assertEqual("test1_default", j["name"])
        self.assertNotEqual(-1, j["id"])
        resp = self.get(f"/{doc1_id}/get_default_velp_group")
        self.assertNotEqual(-1, resp["id"])
        default_group_id = resp["id"]  # Grab ID of newly created group for later use

        # Create new velp group (after default group) so we should have 2 for document in total
        j = self.json_post(
            f"/{doc1_id}/create_velp_group",
            {"name": "velp group for doc 1", "target_type": 1},
        )
        self.assertEqual("velp group for doc 1", j["name"])
        resp = self.get(f"/{doc1_id}/get_velp_groups")
        self.assertEqual(len(resp), 2)

        # Add two documents to document's velp group folder manually
        # User should only see one of them as velp group due to lack of view right for the other
        test_group1 = f"{user_velp_group_folder}/test1/test_group1"
        test_group2 = f"{user_velp_group_folder}/test1/test_group2"
        DocEntry.create(test_group1, self.test_user_1.get_personal_group())
        DocEntry.create(test_group2, t2g)
        db.session.commit()
        resp = self.get(f"/{doc1_id}/get_velp_groups")
        self.assertEqual(len(resp), 3)

        # Create default velp group manually for test 3 file which route notices and turns that document to a velp group
        test3_default_path = f"{deep_velp_group_folder}/test3/test3_default"
        test3_default = DocEntry.create(
            test3_default_path, self.test_user_1.get_personal_group()
        ).document
        db.session.commit()
        test3_default_id = test3_default.doc_id
        j = self.json_post(f"/{doc3_id}/create_default_velp_group")
        self.assertEqual("test3_default", j["name"])
        self.assertEqual(test3_default_id, j["id"])

        # Test 3 should now have only default velp group assigned so length of velp group list is 1
        resp = self.get(f"/{doc3_id}/get_velp_groups")
        self.assertEqual(len(resp), 1)

        # Add velp group to root velp group folder
        # Both documents test1 and test3 should now have one more velp group to use
        test_group3 = f"{root_velp_group_folder}/test_group3"
        DocEntry.create(test_group3, self.test_user_1.get_personal_group())
        db.session.commit()
        resp = self.get(f"/{doc1_id}/get_velp_groups")
        self.assertEqual(len(resp), 4)
        resp = self.get(f"/{doc3_id}/get_velp_groups")
        self.assertEqual(len(resp), 2)

        # Try to get (not existing) default velp group and create new default group for document without owner rights
        resp = self.get(f"/{doc2_id}/get_default_velp_group")
        self.assertEqual(-1, resp["id"])
        self.json_post(
            f"/{doc2_id}/create_default_velp_group",
            expect_status=403,
            expect_content={
                "error": "Sorry, you don't have permission to use this resource."
            },
        )

        # There are no velps added to any groups so getting velps for doc1 should give nothing
        resp = self.get(f"/{doc1_id}/get_velps")
        self.assertEqual(len(resp), 0)

        # Add new velp to doc1 default velp group and check velps for that document after
        test_data = {
            "content": "test velp 1",
            "velp_groups": [default_group_id],
            "language_id": "FI",
        }
        velp_id = self.json_post("/add_velp", test_data)
        self.assertEqual(
            velp_id, 1
        )  # Added velp's id is 1 as it was the first ever velp added
        resp = self.get(f"/{doc1_id}/get_velps")
        self.assertEqual(len(resp), 1)
        self.assertEqual(resp[0]["content"], "test velp 1")

        # Change just added velp's content
        test_data = {
            "content": "Is Zorg now",
            "velp_groups": [default_group_id],
            "language_id": "FI",
            "id": 1,
        }
        self.json_post(f"/{doc1_id}/update_velp", test_data)
        resp = self.get(f"/{doc1_id}/get_velps")
        self.assertEqual(resp[0]["content"], "Is Zorg now")

        # Next add velp label, attach it to a velp
        label_id = self.json_post("/add_velp_label", {"content": "test label"})["id"]
        test_data = {
            "content": "Is Zorg now",
            "velp_groups": [default_group_id],
            "language_id": "FI",
            "id": velp_id,
            "labels": [label_id],
        }
        self.json_post(f"/{doc1_id}/update_velp", test_data)
        resp = self.get(f"/{doc1_id}/get_velp_labels")
        self.assertEqual(resp[0]["content"], "test label")

        # Add a new velp label and update previous one
        self.json_post("/add_velp_label", {"content": "test label intensifies"})
        self.json_post("/update_velp_label", {"id": label_id, "content": "Zorg label"})
        resp = self.get(f"/{doc1_id}/get_velp_labels")
        self.assertNotEqual(resp[0]["content"], "test label")
        self.assertEqual(
            len(resp), 1
        )  # Added velp label wasn't added to any velp and thus it can't be found
        # when searching velp labels for doc1

        self.get(
            "/get_default_personal_velp_group",
            expect_content={
                "created_new_group": True,
                "default": False,
                "default_group": True,
                "edit_access": True,
                "id": 18,
                "location": f"users/test-user-1/velp-groups/{DEFAULT_PERSONAL_VELP_GROUP_NAME}",
                "name": f"{DEFAULT_PERSONAL_VELP_GROUP_NAME}",
                "selected": True,
                "show": True,
                "target_id": "0",
                "target_type": 0,
            },
        )

    def test_nonexistent_group(self):
        self.get("/999/get_velp_groups", expect_status=404)
        self.get("/999/get_default_velp_group", expect_status=404)


class VelpAnonymizationTest(TimRouteTest):
    def test_annotation_anonymization(self):
        self.login_test1()
        d = self.create_doc(
            initial_par="""
``` {#text1 plugin="textfield"}
form:false
```

                        """
        )
        d.document.set_settings({"anonymize_reviewers": "teacher"})
        self.test_user_2.grant_access(d, AccessType.view)
        db.session.commit()
        _, velp_ver = create_new_velp(
            self.test_user_1.id,
            "content",
            0,
        )
        ans = self.add_answer(
            d,
            "text1",
            "tu2 answer",
            user=self.test_user_2,
        )
        ann1 = Annotation(
            velp_version_id=velp_ver.id,
            visible_to=4,
            points=1,
            annotator_id=self.test_user_1.id,
            document_id=d.id,
            color=None,
            answer_id=ans.id,
            style=1,
        )
        db.session.add(ann1)
        ann2 = Annotation(
            velp_version_id=velp_ver.id,
            visible_to=4,
            points=1,
            annotator_id=self.test_user_2.id,
            document_id=d.id,
            color=None,
            answer_id=ans.id,
            style=1,
        )
        db.session.add(ann2)
        db.session.commit()
        self.post(
            "/add_annotation_comment",
            query_string={"id": ann1.id, "content": "tu1@own_velp"},
        )
        self.post(
            "/add_annotation_comment",
            query_string={"id": ann2.id, "content": "tu1@tu2's_velp"},
        )
        self.login_test2()
        anns = self.get(f"{d.id}/get_annotations")["annotations"]

        def check_ann_for_testuser1(ann: dict):
            annotator = ann.get("annotator")
            dump = json.dumps(annotator)
            self.assertNotIn(f'id": {self.test_user_1.id}', dump)
            self.assertNotIn(self.test_user_1.name, dump)
            for c in ann.get("comments"):
                dump = json.dumps(c.get("commenter"))
                self.assertNotIn(f'id": {self.test_user_1.id}', dump)
                self.assertNotIn(self.test_user_1.name, dump)

        for ann in anns:
            check_ann_for_testuser1(ann)

        ann = self.post(
            "/add_annotation_comment",
            query_string={"id": ann2.id, "content": "tu2@tu2's_velp"},
        )
        check_ann_for_testuser1(ann)
        ann = self.json_post(
            "/update_annotation",
            {
                "id": ann2.id,
                "visible_to": 4,
                "color": "blue",
            },
        )
        check_ann_for_testuser1(ann)
        d.document.set_settings({"anonymize_reviewers": "view"})
        self.test_user_2.grant_access(d, AccessType.see_answers)
        db.session.commit()
        anns = self.get(f"{d.id}/get_annotations")["annotations"]
        self.assertEqual(anns[0].get("annotator").get("name"), self.test_user_1.name)
        info_anns = self.get("/settings/info")["annotations"]
        for iann in info_anns:
            check_ann_for_testuser1(iann)


class VelpGroupDeletionTest(TimRouteTest):
    def test_delete_velp_group(self):
        # set up docs and velp groups
        self.login_test1()
        d = self.create_doc(title="test velp group delete")
        self.test_user_2.grant_access(d, AccessType.view)

        g = self.json_post(
            f"/{d.document.id}/create_velp_group",
            {"name": "test-group1", "target_type": 1},
        )
        g2 = self.json_post(
            f"/{d.document.id}/create_velp_group",
            {"name": "test-group2", "target_type": 1},
        )
        # get velp group document
        g_doc = get_doc_or_abort(g["id"])
        g_doc2 = get_doc_or_abort(g2["id"])
        self.test_user_2.grant_access(g_doc, AccessType.view)
        self.test_user_2.grant_access(g_doc2, AccessType.view)
        db.session.commit()

        # Case 1:
        # Test user 2 should not be able to delete velp group with view permissions
        self.login_test2()
        # try to delete the document
        self.delete(url=f"/velp/group/{g['id']}", expect_status=403)

        # Case 2:
        # Should not be able to delete with edit permissions
        self.test_user_2.grant_access(g_doc, AccessType.edit)
        db.session.commit()
        # try to delete the document
        self.delete(url=f"/velp/group/{g['id']}", expect_status=403)

        # Case 3:
        # Should not be able to delete with teacher rights
        self.test_user_2.grant_access(g_doc, AccessType.teacher)
        db.session.commit()
        # try to delete the document
        self.delete(url=f"/velp/group/{g['id']}", expect_status=403)

        # Case 4:
        # Should be able to delete with manage rights
        self.test_user_2.grant_access(g_doc, AccessType.manage)
        db.session.commit()
        # try to delete the document
        self.delete(url=f"/velp/group/{g['id']}", expect_status=200)
        # velp group document should now be placed in the TIM 'trash bin' (/roskis)
        deleted = get_doc_or_abort(g["id"])
        self.assertEqual(f"roskis/{g['name']}", deleted.path)

        # database should not contain any references to the velp group
        vg = (
            db.session.execute(select(VelpGroup).filter_by(id=g["id"]).limit(1))
            .scalars()
            .first()
        )
        v_in_g = (
            db.session.execute(select(VelpInGroup).filter_by(velp_group_id=g["id"]))
            .scalars()
            .all()
        )
        vg_sel = (
            db.session.execute(
                select(VelpGroupSelection).filter_by(velp_group_id=g["id"])
            )
            .scalars()
            .all()
        )
        vg_def = (
            db.session.execute(
                select(VelpGroupDefaults).filter_by(velp_group_id=g["id"])
            )
            .scalars()
            .all()
        )
        vg_in_doc = (
            db.session.execute(
                select(VelpGroupsInDocument).filter_by(velp_group_id=g["id"])
            )
            .scalars()
            .all()
        )

        self.assertEqual(None, vg)
        self.assertEqual(0, len(v_in_g))
        self.assertEqual(0, len(vg_sel))
        self.assertEqual(0, len(vg_def))
        self.assertEqual(0, len(vg_in_doc))

        # Case 5:
        # Should be able to delete velp group with owner rights
        self.test_user_2.grant_access(g_doc2, AccessType.owner)
        db.session.commit()
        # try to delete the document
        self.delete(url=f"/velp/group/{g2['id']}", expect_status=200)
        # velp group document should now be placed in the TIM 'trash bin' (/roskis)
        deleted = get_doc_or_abort(g2["id"])
        self.assertEqual(f"roskis/{g2['name']}", deleted.path)

        # database should not contain any references to the velp group
        vg2 = (
            db.session.execute(select(VelpGroup).filter_by(id=g2["id"]).limit(1))
            .scalars()
            .first()
        )
        v_in_g2 = (
            db.session.execute(select(VelpInGroup).filter_by(velp_group_id=g2["id"]))
            .scalars()
            .all()
        )
        vg_sel2 = (
            db.session.execute(
                select(VelpGroupSelection).filter_by(velp_group_id=g2["id"])
            )
            .scalars()
            .all()
        )
        vg_def2 = (
            db.session.execute(
                select(VelpGroupDefaults).filter_by(velp_group_id=g2["id"])
            )
            .scalars()
            .all()
        )
        vg_in_doc2 = (
            db.session.execute(
                select(VelpGroupsInDocument).filter_by(velp_group_id=g2["id"])
            )
            .scalars()
            .all()
        )

        self.assertEqual(None, vg2)
        self.assertEqual(0, len(v_in_g2))
        self.assertEqual(0, len(vg_sel2))
        self.assertEqual(0, len(vg_def2))
        self.assertEqual(0, len(vg_in_doc2))


class VelpGroupPermissionsPropagationTest(TimRouteTest):
    """Tests propagation of document permissions to the document's velp groups.
    The document's velp groups' permissions should be updated as users gain
    or lose permissions to the document.
    """

    def setup_velp_group_test(self) -> tuple[DocInfo, DocInfo]:
        # set up docs and velp groups
        self.login_test1()
        d = self.create_doc(title="test velp group permissions")

        g = self.json_post(
            f"/{d.document.id}/create_velp_group",
            {"name": "test-group1", "target_type": 1},
        )
        db.session.commit()
        # get velp group document
        g_doc = get_doc_or_abort(g["id"])

        return d, g_doc

    def test_velp_group_permissions_view(self):
        d, g_doc = self.setup_velp_group_test()

        # Document and velp group permissions should be the same
        d_perms = get_user_rights_for_item(d, get_current_user_object())
        g_perms = get_user_rights_for_item(g_doc, get_current_user_object())
        self.assertEqual(d_perms, g_perms)

        # Case 1:
        # Test user 2 should initially not be able to access velp group
        self.login_test2()
        self.get(g_doc.url, expect_status=403)

        # Should be able to access velp group with view permissions
        self.login_test1()
        self.json_put(
            f"/permissions/add",
            {
                "time": {
                    "from": get_current_time(),
                    "type": "always",
                },
                "id": d.id,
                "type": AccessType.view.value,
                "groups": ["testuser2"],
                "confirm": False,
            },
        )
        self.login_test2()
        self.get(g_doc.url, expect_status=200)

        self.login_test1()
        self.json_put(
            f"/permissions/remove",
            {
                "id": d.id,
                "type": AccessType.view.value,
                "group": self.test_user_2.get_personal_group().id,
            },
            expect_status=200,
        )
        # Should no longer be able to access velp group
        self.login_test2()
        vgs = get_groups_from_document_table(
            d.id, self.test_user_2.get_personal_group().id
        )
        for vg in vgs:
            vgd = DocInfo.find_by_id(vg.id)
            self.get(vgd.url, expect_status=403)

    def test_velp_group_permissions_edit(self):
        d, g_doc = self.setup_velp_group_test()

        # Document and velp group permissions should be the same
        d_perms = get_user_rights_for_item(d, get_current_user_object())
        g_perms = get_user_rights_for_item(g_doc, get_current_user_object())
        self.assertEqual(d_perms, g_perms)

        # Initially not able to access
        self.login_test2()
        self.get(g_doc.url, expect_status=403)
        # Case 2:
        # Should be able to access velp group with edit permissions
        self.login_test1()
        self.json_put(
            f"/permissions/add",
            {
                "time": {
                    "type": "always",
                },
                "id": d.id,
                "type": AccessType.edit.value,
                "groups": ["testuser2"],
                "confirm": False,
            },
        )
        self.login_test2()
        self.get(g_doc.url, expect_status=200)

        self.login_test1()
        self.json_put(
            f"/permissions/remove",
            {
                "id": d.id,
                "type": AccessType.edit.value,
                "group": self.test_user_2.get_personal_group().id,
            },
        )
        # Should no longer be able to access velp group
        self.login_test2()
        vgs = get_groups_from_document_table(
            d.id, self.test_user_2.get_personal_group().id
        )
        for vg in vgs:
            vgd = DocInfo.find_by_id(vg.id)
            self.get(vgd.url, expect_status=403)

    def test_velp_group_permissions_teacher(self):
        d, g_doc = self.setup_velp_group_test()

        # Document and velp group permissions should be the same
        d_perms = get_user_rights_for_item(d, get_current_user_object())
        g_perms = get_user_rights_for_item(g_doc, get_current_user_object())
        self.assertEqual(d_perms, g_perms)

        # Initially not able to access
        self.login_test2()
        self.get(g_doc.url, expect_status=403)

        # Case 3:
        # Should be able to access velp group with teacher permissions
        self.login_test1()
        self.json_put(
            f"/permissions/add",
            {
                "time": {
                    "type": "always",
                },
                "id": d.id,
                "type": AccessType.teacher.value,
                "groups": ["testuser2"],
                "confirm": False,
            },
        )
        self.login_test2()
        self.get(g_doc.url, expect_status=200)

        self.login_test1()
        self.json_put(
            f"/permissions/remove",
            {
                "id": d.id,
                "type": AccessType.teacher.value,
                "group": self.test_user_2.get_personal_group().id,
            },
        )
        # Should no longer be able to access velp group
        self.login_test2()
        vgs = get_groups_from_document_table(
            d.id, self.test_user_2.get_personal_group().id
        )
        for vg in vgs:
            vgd = DocInfo.find_by_id(vg.id)
            self.get(vgd.url, expect_status=403)

    def test_velp_group_permissions_manage(self):
        d, g_doc = self.setup_velp_group_test()

        # Document and velp group permissions should be the same
        d_perms = get_user_rights_for_item(d, get_current_user_object())
        g_perms = get_user_rights_for_item(g_doc, get_current_user_object())
        self.assertEqual(d_perms, g_perms)

        # Initially not able to access
        self.login_test2()
        self.get(g_doc.url, expect_status=403)

        # Case 4:
        # Should be able to access velp group with manage permissions
        self.login_test1()
        self.json_put(
            f"/permissions/add",
            {
                "time": {
                    "type": "always",
                },
                "id": d.id,
                "type": AccessType.manage.value,
                "groups": ["testuser2"],
                "confirm": False,
            },
        )
        self.login_test2()
        self.get(g_doc.url, expect_status=200)

        self.login_test1()
        self.json_put(
            f"/permissions/remove",
            {
                "id": d.id,
                "type": AccessType.manage.value,
                "group": self.test_user_2.get_personal_group().id,
            },
        )
        # Should no longer be able to access velp group
        self.login_test2()
        vgs = get_groups_from_document_table(
            d.id, self.test_user_2.get_personal_group().id
        )
        for vg in vgs:
            vgd = DocInfo.find_by_id(vg.id)
            self.get(vgd.url, expect_status=403)

    def test_velp_group_permissions_owner(self):
        d, g_doc = self.setup_velp_group_test()

        # Document and velp group permissions should be the same
        d_perms = get_user_rights_for_item(d, get_current_user_object())
        g_perms = get_user_rights_for_item(g_doc, get_current_user_object())
        self.assertEqual(d_perms, g_perms)

        # Initially not able to access
        self.login_test2()
        self.get(g_doc.url, expect_status=403)

        # Case 5:
        # Should be able to access velp group with owner permissions
        self.login_test1()
        self.json_put(
            f"/permissions/add",
            {
                "time": {
                    "type": "always",
                },
                "id": d.id,
                "type": AccessType.owner.value,
                "groups": ["testuser2"],
                "confirm": False,
            },
        )
        self.login_test2()
        self.get(g_doc.url, expect_status=200)

        self.login_test1()
        self.json_put(
            f"/permissions/remove",
            {
                "id": d.id,
                "type": AccessType.owner.value,
                "group": self.test_user_2.get_personal_group().id,
            },
        )
        # Should no longer be able to access velp group
        self.login_test2()
        vgs = get_groups_from_document_table(
            d.id, self.test_user_2.get_personal_group().id
        )
        for vg in vgs:
            vgd = DocInfo.find_by_id(vg.id)
            self.get(vgd.url, expect_status=403)

    def test_velp_group_permissions_new_group(self):
        d, g_doc = self.setup_velp_group_test()

        # Case 6:
        # New velp groups for the document should set permissions for
        # all users with access to the document
        self.login_test2()
        self.test_user_2.grant_access(d, AccessType.view)
        db.session.commit()
        self.login_test1()
        g2 = self.json_post(
            f"/{d.document.id}/create_velp_group",
            {"name": "test-group2", "target_type": 1},
        )
        g2_doc = get_doc_or_abort(g2["id"])
        self.login_test2()
        self.get(g2_doc.url, expect_status=200)

    def test_deleted_velp_group_permissions(self):
        d, g_doc = self.setup_velp_group_test()

        # Case 7:
        # Deleted velp groups should have their permissions cleared
        self.test_user_2.grant_access(g_doc, AccessType.manage)
        db.session.commit()
        self.login_test2()
        self.get(g_doc.url, expect_status=200)
        self.json_delete(f"/velp/group/{g_doc.id}", expect_status=200)
        deleted = get_doc_or_abort(g_doc.id)
        # test user 2 should not have access anymore
        self.get(deleted.url, expect_status=403)
        # admin should still be able to access
        self.make_admin(self.test_user_2)
        self.get(deleted.url, expect_status=200)
        # remove testuser2 from admin group to prevent it from affecting other tests
        self.json_post(
            f"/groups/removemember/{UserGroup.get_admin_group().name}",
            {"names": [self.test_user_2.name]},
            expect_content={
                "removed": [self.test_user_2.name],
                "does_not_belong": [],
                "not_exist": [],
            },
        )

    def test_velp_group_expire_permissions(self):
        d, g_doc = self.setup_velp_group_test()

        # Case 8:
        # Permissions expiry for document should also affect document's VelpGroups

        # Give testuser2 access to document (and it's VelpGroups by extension)
        self.json_put(
            f"/permissions/add",
            {
                "time": {
                    "type": "always",
                },
                "id": d.id,
                "type": AccessType.view.value,
                "groups": ["testuser2"],
                "confirm": False,
            },
        )

        self.login_test2()
        testuser2 = get_current_user_object()
        # testuser2 should have access
        self.get(d.url, expect_status=200)
        self.get(g_doc.url, expect_status=200)

        self.login_test1()
        # Expire document permissions for testuser2
        self.get(f"/permissions/expire/{d.id}/{testuser2.name}")

        self.login_test2()
        # testuser2 should no longer have access
        self.get(d.url, expect_status=403)
        self.get(g_doc.url, expect_status=403)

    def test_velp_group_self_expire_permissions(self):
        d, g_doc = self.setup_velp_group_test()

        # Case 9:
        # Permissions self-expiry for document should also affect document's VelpGroups

        # Give testuser2 access to document (and it's VelpGroups by extension)
        self.json_put(
            f"/permissions/add",
            {
                "time": {
                    "type": "always",
                },
                "id": d.id,
                "type": AccessType.view.value,
                "groups": ["testuser2"],
                "confirm": False,
            },
        )

        self.login_test2()
        # testuser2 should have access
        self.get(d.url, expect_status=200)
        self.get(g_doc.url, expect_status=200)

        # Self-expire document permissions
        self.json_post("/permissions/selfExpire", {"id": d.id})

        # testuser2 should no longer have access
        self.get(d.url, expect_status=403)
        self.get(g_doc.url, expect_status=403)

    def test_velp_group_clear_permissions(self):
        d, g_doc = self.setup_velp_group_test()

        # Case 10:
        # Permissions self-expiry for document should also affect document's VelpGroups

        # Give testuser2 access to document (and it's VelpGroups by extension)
        self.json_put(
            f"/permissions/add",
            {
                "time": {
                    "type": "always",
                },
                "id": d.id,
                "type": AccessType.edit.value,
                "groups": ["testuser2"],
                "confirm": False,
            },
        )

        self.login_test2()
        # testuser2 should have access
        self.get(d.url, expect_status=200)
        self.get(g_doc.url, expect_status=200)

        self.login_test1()
        # Clear document (and doc velp group) permissions
        self.json_put(
            "/permissions/clear",
            {
                "paths": [d.path],
                "type": AccessType.edit.value,
            },
        )

        self.login_test2()
        # testuser2 should no longer have access
        self.get(d.url, expect_status=403)
        self.get(g_doc.url, expect_status=403)

    def test_velp_group_mass_edit_permissions(self):
        self.login_test1()
        d1 = self.create_doc(title="test velp group permissions")
        d2 = self.create_doc(title="test velp group permissions 2")
        d3 = self.create_doc(title="test velp group permissions 3")

        g1 = self.json_post(
            f"/{d1.document.id}/create_velp_group",
            {"name": "test-group1", "target_type": 1},
        )
        g2 = self.json_post(
            f"/{d1.document.id}/create_velp_group",
            {"name": "test-group2", "target_type": 1},
        )
        g3 = self.json_post(
            f"/{d2.document.id}/create_velp_group",
            {"name": "test-group3", "target_type": 1},
        )
        g4 = self.json_post(
            f"/{d2.document.id}/create_velp_group",
            {"name": "test-group4", "target_type": 1},
        )
        g5 = self.json_post(
            f"/{d3.document.id}/create_velp_group",
            {"name": "test-group5", "target_type": 1},
        )
        g6 = self.json_post(
            f"/{d3.document.id}/create_velp_group",
            {"name": "test-group6", "target_type": 1},
        )
        db.session.commit()

        g_doc1 = get_doc_or_abort(g1["id"])
        g_doc2 = get_doc_or_abort(g2["id"])
        g_doc3 = get_doc_or_abort(g3["id"])
        g_doc4 = get_doc_or_abort(g4["id"])
        g_doc5 = get_doc_or_abort(g5["id"])
        g_doc6 = get_doc_or_abort(g6["id"])

        # Initially not able to access
        self.login_test2()
        self.get(g_doc1.url, expect_status=403)
        self.get(g_doc2.url, expect_status=403)
        self.get(g_doc3.url, expect_status=403)
        self.get(g_doc4.url, expect_status=403)
        self.get(g_doc5.url, expect_status=403)
        self.get(g_doc6.url, expect_status=403)
        self.login_test3()
        self.get(g_doc1.url, expect_status=403)
        self.get(g_doc2.url, expect_status=403)
        self.get(g_doc3.url, expect_status=403)
        self.get(g_doc4.url, expect_status=403)
        self.get(g_doc5.url, expect_status=403)
        self.get(g_doc6.url, expect_status=403)

        # Case 11:
        # Should be able to access velp group with edit permissions
        self.login_test1()
        self.json_put(
            f"/permissions/edit",
            {
                "groups": ["testuser2", "testuser3"],
                "type": AccessType.edit.value,
                "action": "add",
                "ids": [d1.document.id, d2.document.id, d3.document.id],
                "time": {
                    "type": "always",
                },
                "confirm": False,
            },
        )
        self.login_test2()
        tu2_velp_groups = []
        tu2id = self.test_user_2.get_personal_group().id
        tu2_velp_groups.extend(get_groups_from_document_table(d1.id, tu2id))
        tu2_velp_groups.extend(get_groups_from_document_table(d2.id, tu2id))
        tu2_velp_groups.extend(get_groups_from_document_table(d3.id, tu2id))
        for vg in tu2_velp_groups:
            vgd = DocInfo.find_by_id(vg.id)
            self.get(vgd.url, expect_status=200)

        self.login_test3()
        tu3_velp_groups = []
        tu3id = self.test_user_3.get_personal_group().id
        tu3_velp_groups.extend(get_groups_from_document_table(d1.id, tu3id))
        tu3_velp_groups.extend(get_groups_from_document_table(d2.id, tu3id))
        tu3_velp_groups.extend(get_groups_from_document_table(d3.id, tu3id))
        for vg in tu3_velp_groups:
            vgd = DocInfo.find_by_id(vg.id)
            self.get(vgd.url, expect_status=200)

        self.login_test1()
        self.json_put(
            f"/permissions/edit",
            {
                "groups": ["testuser2", "testuser3"],
                "type": AccessType.edit.value,
                "action": "remove",
                "ids": [d1.document.id, d2.document.id, d3.document.id],
                "time": {
                    "type": "always",
                },
                "confirm": False,
            },
        )
        # Should no longer be able to access velp group
        self.login_test2()
        self.get(g_doc1.url, expect_status=403)
        self.get(g_doc2.url, expect_status=403)
        self.get(g_doc3.url, expect_status=403)
        self.get(g_doc4.url, expect_status=403)
        self.get(g_doc5.url, expect_status=403)
        self.get(g_doc6.url, expect_status=403)
        self.login_test3()
        self.get(g_doc1.url, expect_status=403)
        self.get(g_doc2.url, expect_status=403)
        self.get(g_doc3.url, expect_status=403)
        self.get(g_doc4.url, expect_status=403)
        self.get(g_doc5.url, expect_status=403)
        self.get(g_doc6.url, expect_status=403)

    def test_velp_group_permissions_path(self):
        self.login_test1()

        d = self.create_doc(title="test velp group permissions")
        g_persnl = self.json_post(
            f"/{d.document.id}/create_velp_group",
            {"name": "personal-group", "target_type": 0},
        )
        g_docmnt = self.json_post(
            f"/{d.document.id}/create_velp_group",
            {"name": "document-group", "target_type": 1},
        )
        g_folder = self.json_post(
            f"/{d.document.id}/create_velp_group",
            {"name": "folder-group", "target_type": 2},
        )
        db.session.commit()
        db.session.expire_all()

        g_persnl_doc = get_doc_or_abort(g_persnl["id"])
        g_docmnt_doc = get_doc_or_abort(g_docmnt["id"])
        g_folder_doc = get_doc_or_abort(g_folder["id"])

        # Initially not able to access
        self.assertFalse(self.test_user_2.has_view_access(g_persnl_doc))
        self.assertFalse(self.test_user_2.has_view_access(g_docmnt_doc))
        self.assertFalse(self.test_user_2.has_view_access(g_folder_doc))

        # Case 11:
        # Permissions should propagate for document velp group ONLY
        self.login_test1()
        self.json_put(
            f"/permissions/add",
            {
                "time": {
                    "from": get_current_time(),
                    "type": "always",
                },
                "id": d.id,
                "type": AccessType.view.value,
                "groups": ["testuser2"],
                "confirm": False,
            },
        )
        # Should only have access to doc velp group
        self.login_test2()
        self.get(g_persnl_doc.url, expect_status=403)
        self.get(g_docmnt_doc.url, expect_status=200)
        self.get(g_folder_doc.url, expect_status=403)

        # Case 12:
        # Should remove permissions only from document velp group
        self.login_test1()
        self.json_put(
            f"/permissions/add",
            {
                "time": {
                    "from": get_current_time(),
                    "type": "always",
                },
                "id": g_persnl_doc.id,
                "type": AccessType.view.value,
                "groups": ["testuser2"],
                "confirm": False,
            },
        )
        self.json_put(
            f"/permissions/add",
            {
                "time": {
                    "from": get_current_time(),
                    "type": "always",
                },
                "id": g_folder_doc.id,
                "type": AccessType.view.value,
                "groups": ["testuser2"],
                "confirm": False,
            },
        )
        self.login_test2()
        self.get(g_persnl_doc.url, expect_status=200)
        self.get(g_docmnt_doc.url, expect_status=200)
        self.get(g_folder_doc.url, expect_status=200)

        self.login_test1()
        self.json_put(
            f"/permissions/remove",
            {
                "id": d.id,
                "type": AccessType.view.value,
                "group": self.test_user_2.get_personal_group().id,
            },
        )
        self.login_test2()
        self.get(g_persnl_doc.url, expect_status=200)

        # Should not be able to access document groups
        vgs = get_groups_from_document_table(
            d.id, self.test_user_2.get_personal_group().id
        )
        for vg in vgs:
            if vg.id != g_docmnt_doc.id:
                continue
            vgd = DocInfo.find_by_id(vg.id)
            self.get(vgd.url, expect_status=403)

        self.get(g_folder_doc.url, expect_status=200)

        self.login_test1()
        self.json_put(
            f"/permissions/remove",
            {
                "id": g_persnl_doc.id,
                "type": AccessType.view.value,
                "group": self.test_user_2.get_personal_group().id,
                "edit_velp_group_perms": False,
            },
        )
        self.json_put(
            f"/permissions/remove",
            {
                "id": g_folder_doc.id,
                "type": AccessType.view.value,
                "group": self.test_user_2.get_personal_group().id,
                "edit_velp_group_perms": False,
            },
        )
        self.login_test2()
        for vg in vgs:
            vgd = DocInfo.find_by_id(vg.id)
            self.get(vgd.url, expect_status=403)

        # Case 13:
        # Should edit permissions only for document velp group
        self.login_test1()
        self.json_put(
            f"/permissions/edit",
            {
                "groups": ["testuser2"],
                "type": AccessType.view.value,
                "action": "add",
                "ids": [d.document.id],
                "time": {
                    "type": "always",
                },
                "confirm": False,
            },
        )
        self.login_test2()
        self.get(g_persnl_doc.url, expect_status=403)
        self.get(g_docmnt_doc.url, expect_status=200)
        self.get(g_folder_doc.url, expect_status=403)

        self.login_test1()
        self.json_put(
            f"/permissions/edit",
            {
                "groups": ["testuser2"],
                "type": AccessType.view.value,
                "action": "add",
                "ids": [g_folder_doc.id, g_persnl_doc.id],
                "time": {
                    "type": "always",
                },
                "confirm": False,
            },
        )
        self.login_test2()
        self.get(g_persnl_doc.url, expect_status=200)
        self.get(g_docmnt_doc.url, expect_status=200)
        self.get(g_folder_doc.url, expect_status=200)

        self.login_test1()
        self.json_put(
            f"/permissions/edit",
            {
                "groups": ["testuser2"],
                "type": AccessType.view.value,
                "action": "remove",
                "ids": [d.document.id],
                "time": {
                    "type": "always",
                },
                "confirm": False,
            },
        )
        self.login_test2()
        self.get(g_persnl_doc.url, expect_status=200)
        for vg in vgs:
            if vg.id != g_docmnt_doc.id:
                continue
            vgd = DocInfo.find_by_id(vg.id)
            self.get(vgd.url, expect_status=403)
        self.get(g_folder_doc.url, expect_status=200)

        self.login_test1()
        self.json_put(
            f"/permissions/edit",
            {
                "groups": ["testuser2"],
                "type": AccessType.view.value,
                "action": "remove",
                "ids": [g_persnl_doc.id, g_folder_doc.id],
                "time": {
                    "type": "always",
                },
                "confirm": False,
            },
        )
        self.login_test2()
        for vg in vgs:
            vgd = DocInfo.find_by_id(vg.id)
            self.get(vgd.url, expect_status=403)

    def test_modify_perms_for_no_vg_perms_user(self):
        """Test modifying document permissions (remove, edit, expire, clear)
        for user who has no permissions to document's velp groups
        """
        d, g_doc = self.setup_velp_group_test()

        self.test_user_2.grant_access(d, access_type=AccessType.view)
        db.session.commit()

        # Testuser2 should now have view access to the document,
        # but not to the document's velp groups
        self.login_test2()
        self.get(d.url, expect_status=200)
        self.get(g_doc.url, expect_status=403)

        self.login_test1()
        # Case 14:
        # Should be able to remove testuser2's permissions to the parent document
        self.json_put(
            f"/permissions/remove",
            {
                "id": d.id,
                "type": AccessType.view.value,
                "group": self.get_test_user_2_group_id(),
            },
            expect_status=200,
        )

        self.test_user_2.grant_access(d, access_type=AccessType.view)
        db.session.commit()
        # Case 15:
        # Should be able to edit/remove testuser2's permissions to the parent document
        self.json_put(
            f"/permissions/edit",
            {
                "groups": ["testuser2"],
                "type": AccessType.view.value,
                "action": "remove",
                "ids": [d.id],
                "time": {
                    "type": "always",
                },
                "confirm": False,
            },
            expect_status=200,
        )

        self.test_user_2.grant_access(d, access_type=AccessType.view)
        db.session.commit()
        # Case 16:
        # Should be able to expire testuser2's permissions to the parent document
        self.get(
            f"/permissions/expire/{d.id}/{self.test_user_2.name}", expect_status=200
        )

        self.test_user_2.grant_access(d, access_type=AccessType.view)
        db.session.commit()
        # Case 17:
        # Should be able to self-expire testuser2's permissions to the parent document
        self.login_test2()
        self.json_post("/permissions/selfExpire", {"id": d.id}, expect_status=200)

        self.login_test1()
        self.test_user_2.grant_access(d, access_type=AccessType.view)
        db.session.commit()
        # Case 18:
        # Should be able to clear testuser2's permissions to the parent document
        self.json_put(
            "/permissions/clear",
            {
                "paths": [d.path],
                "type": AccessType.edit.value,
            },
            expect_status=200,
        )

    def test_velp_group_permissions_parameter(self):
        """Test that velp group perms are not set if edit_velp_group_perms flag is false"""
        d, g_doc = self.setup_velp_group_test()

        # Initially not able to access
        self.login_test2()
        self.get(g_doc.url, expect_status=403)

        # Case 19:
        # Should not set perms for velp groups if edit_velp_group_perms is false
        self.login_test1()
        self.json_put(
            f"/permissions/add",
            {
                "time": {
                    "type": "always",
                },
                "id": d.id,
                "type": AccessType.view.value,
                "groups": ["testuser2"],
                "confirm": False,
                "edit_velp_group_perms": False,
            },
            expect_status=200,
        )

        self.login_test2()
        self.get(g_doc.url, expect_status=403)

        # Case 20:
        # Should not set perms for velp groups if edit_velp_group_perms is false
        self.login_test1()
        self.json_put(
            f"/permissions/edit",
            {
                "groups": ["testuser2"],
                "type": AccessType.view.value,
                "action": "add",
                "ids": [d.id],
                "time": {
                    "type": "always",
                },
                "confirm": False,
                "edit_velp_group_perms": False,
            },
            expect_status=200,
        )

        self.login_test2()
        self.get(g_doc.url, expect_status=403)
