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

from timApp.auth.accesstype import AccessType
from timApp.document.docentry import DocEntry
from timApp.folder.folder import Folder
from timApp.tests.server.timroutetest import TimRouteTest
from timApp.timdb.sqa import db
from timApp.velp.annotation import Annotation
from timApp.velp.velp import create_new_velp


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
                "location": "users/test-user-1/velp-groups/Personal-default",
                "name": "Personal-default",
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
        anns = self.get(f"{d.id}/get_annotations")

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
        info_anns = self.get("/settings/info")["annotations"]
        for iann in info_anns:
            check_ann_for_testuser1(iann)
