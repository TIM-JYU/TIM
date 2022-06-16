from timApp.auth.accesstype import AccessType
from timApp.tests.server.timroutetest import TimRouteTest
from timApp.timdb.sqa import db


class AccessLockTest(TimRouteTest):
    """Tests for access locking and unlocking"""

    def tearDown(self):
        with self.client.session_transaction() as s:
            s.clear()

    def test_access_lock_document_redirect(self):
        """Test that document viewing is restricted when access level is locked."""

        self.login_test1()
        d = self.create_doc()
        db.session.commit()

        self.get(d.get_url_for_view("view"), expect_status=200)
        self.get(d.get_url_for_view("teacher"), expect_status=200)

        self.json_post("/access/lock", json_data={"access_type": AccessType.view.value})

        self.get(d.get_url_for_view("view"), expect_status=200)
        self.get(
            d.get_url_for_view("teacher"),
            expect_status=302,
            expect_content=d.get_relative_url_for_view("view"),
        )

        self.json_post("/access/lock", json_data={"access_type": AccessType.edit.value})

        self.get(d.get_url_for_view("view"), expect_status=200)
        self.get(
            d.get_url_for_view("teacher"),
            expect_status=302,
            expect_content=d.get_relative_url_for_view("view"),
        )
        self.get(
            d.get_url_for_view("answers"),
            expect_status=302,
            expect_content=d.get_relative_url_for_view("view"),
        )

        self.json_post("/access/lock", json_data={"access_type": None})
        self.get(d.get_url_for_view("teacher"), expect_status=200)
        self.get(d.get_url_for_view("answers"), expect_status=200)

    def test_access_lock_document_access(self):
        """Test that locking access does not affect document access rules."""

        self.login_test1()
        d = self.create_doc()
        db.session.commit()

        # Case 1: Test user 2 doesn't have access to document, locks access to view
        # -> user must not get access because they didn't originally have access
        self.login_test2()
        self.json_post("/access/lock", json_data={"access_type": AccessType.edit.value})
        self.get(d.get_url_for_view("view"), expect_status=403)

        # Case 2: Test user 2 has access to document, locks access to teacher
        # -> user can view the document but cannot access as teacher
        self.test_user_2.grant_access(d, AccessType.view)
        db.session.commit()

        self.get(d.get_url_for_view("view"), expect_status=200)
        self.json_post(
            "/access/lock", json_data={"access_type": AccessType.teacher.value}
        )
        self.get(
            d.get_url_for_view("teacher"),
            expect_status=302,
            expect_content=d.get_relative_url_for_view("view"),
        )

        # Case 3: Test user 2 has only view access, but locks access to edit and tries to somehow edit the document
        # -> user cannot edit the document since their original permission is not sufficient
        self.json_post("/access/lock", json_data={"access_type": AccessType.edit.value})
        self.json_post(
            "/newParagraph/",
            json_data={
                "docId": d.id,
                "tags": {"markread": False},
                "text": "Foo",
                "view": "view",
            },
            expect_status=403,
        )

    def test_access_lock_tasks(self):
        """Test that access locking affects tasks and fields"""

        self.login_test1()
        d = self.create_doc(
            initial_par="""
#- {defaultplugin="textfield" readonly="view" .fieldCell}
Text box: {#foo #}

#-
``` {#pali plugin="pali"}
-points_array: [[0, 0.1], [0.6, 1]]
needed_len: 5
answerLimit: 3
cols: 20
```
"""
        )
        db.session.commit()

        # Case 1: Saving readonly field is possible for owners
        self.post_answer("textfield", f"{d.id}.foo", {"c": "1", "nosave": False})

        # Case 2: Readonly field cannot be saved when access is locked to view
        self.json_post("/access/lock", json_data={"access_type": AccessType.view.value})
        self.post_answer(
            "textfield", f"{d.id}.foo", {"c": "2", "nosave": False}, expect_status=403
        )

        # Case 3: Disable answering for view access, try to answer with locked access -> should fail
        d.document.set_settings({"disable_answer": "view"})
        self.json_post("/access/lock", json_data={"access_type": AccessType.view.value})
        self.post_answer(
            "pali",
            f"{d.id}.pali",
            {"nosave": False, "paliOK": True, "userword": "1"},
            expect_status=403,
        )

        # Case 4: Saving should work again once access is unlocked
        self.json_post("/access/lock", json_data={"access_type": None})
        self.post_answer(
            "pali",
            f"{d.id}.pali",
            {"nosave": False, "paliOK": True, "userword": "1"},
            expect_status=200,
        )
