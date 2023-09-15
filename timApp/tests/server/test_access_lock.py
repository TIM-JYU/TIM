from timApp.auth.accesstype import AccessType
from timApp.item.block import Block
from timApp.markdown.markdownconverter import md_to_html
from timApp.tests.server.timroutetest import TimRouteTest
from timApp.timdb.sqa import db
from timApp.user.usergroup import (
    UserGroup,
    get_logged_in_group_id,
    get_anonymous_group_id,
)
from timApp.user.usergroupmember import UserGroupMember


class AccessLockTest(TimRouteTest):
    """Tests for access locking and unlocking"""

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


class ActiveGroupLockTest(TimRouteTest):
    """Tests for active group locking and unlocking"""

    def test_active_group_lock_visibility(self):
        self.login_test1()

        ug = UserGroup.create("testgroup1")
        admin_doc = self.create_doc()
        ug.admin_doc = admin_doc.block
        self.test_user_3.add_to_group(ug, None)
        self.test_user_1.remove_access(admin_doc.block.id, "owner")
        db.session.commit()
        ug_id = ug.id
        admin_block_id = admin_doc.block.id

        d = self.create_doc(
            initial_par="""
#- {visible="%%'Logged-in users' | belongs%%"}
Logged-in users

#- {visible="%%'testgroup1' | belongs%%"}
Test group 1

#- {visible="%%'testuser2' | belongs%%"}
Test user 2
"""
        )
        d.block.add_rights([UserGroup.get_anonymous_group()], AccessType.view)
        db.session.commit()

        # Basic case: Test user 1 is logged in and has access to the document
        self.get(
            d.get_url_for_view("view"),
            expect_contains=[
                md_to_html("Logged-in users"),
            ],
        )

        # Case 1: User cannot lock access to the document (no access)
        self.json_post(
            "/access/groups/lock",
            {
                "group_ids": [ug_id],
            },
            expect_status=403,
        )

        ug = UserGroup.get_by_name("testgroup1")
        self.test_user_1.add_to_group(ug, None)
        db.session.commit()

        # Case 2: User can lock active group to testgroup1 (testuser1 is in testgroup1)
        self.json_post(
            "/access/groups/lock",
            {
                "group_ids": [
                    ug_id,
                    get_anonymous_group_id(),
                    get_logged_in_group_id(),
                ],
            },
            expect_status=200,
        )
        self.get(
            d.get_url_for_view("view"),
            expect_contains=[
                md_to_html("Logged-in users"),
                md_to_html("Test group 1"),
            ],
        )

        # Case 3: User can lock active group to testgroup1 (testuser1 has edit access)
        # FIXME: SQLAlchemy dynamic
        ugm: UserGroupMember = self.test_user_1.memberships_dyn.filter(
            UserGroupMember.usergroup_id == ug_id
        ).first()
        ugm.set_expired()
        self.test_user_1.grant_access(
            db.session.get(Block, admin_block_id), AccessType.edit
        )
        db.session.commit()
        self.json_post(
            "/access/groups/lock",
            {
                "group_ids": [
                    ug_id,
                    get_anonymous_group_id(),
                    get_logged_in_group_id(),
                ],
            },
            expect_status=200,
        )
        self.get(
            d.get_url_for_view("view"),
            expect_contains=[
                md_to_html("Logged-in users"),
                md_to_html("Test group 1"),
            ],
        )

        # Case 4: Non-admin cannot lock active group to testuser2
        self.json_post(
            "/access/groups/lock",
            {
                "group_ids": [
                    self.test_user_2.get_personal_group().id,
                    ug_id,
                    get_anonymous_group_id(),
                    get_logged_in_group_id(),
                ],
            },
            expect_status=400,
        )

        # Case 5: Admin can lock active group to anyone
        self.make_admin(self.test_user_1)
        self.json_post(
            "/access/groups/lock",
            {
                "group_ids": [
                    self.test_user_2.get_personal_group().id,
                    ug_id,
                    get_anonymous_group_id(),
                    get_logged_in_group_id(),
                ],
            },
            expect_status=200,
        )
        self.get(
            d.get_url_for_view("view"),
            expect_contains=[
                md_to_html("Logged-in users"),
                md_to_html("Test group 1"),
                md_to_html("Test user 2"),
            ],
        )
