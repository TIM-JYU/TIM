import time
from datetime import timedelta

from timApp.auth.accesstype import AccessType
from timApp.document.docentry import DocEntry
from timApp.document.editing.documenteditresult import DocumentEditResult
from timApp.document.translation.synchronize_translations import (
    synchronize_translations,
)
from timApp.item.item import Item
from timApp.tests.server.test_default_rights import convert_to_old_format
from timApp.tests.server.timroutetest import TimRouteTest
from timApp.timdb.sqa import db
from timApp.user.usergroup import UserGroup, get_logged_in_group_id
from timApp.user.userutils import grant_access
from timApp.util.utils import get_current_time


class PermissionTest(TimRouteTest):
    def test_cannot_remove_ownership(self):
        self.login_test1()
        d = self.create_doc()
        self.assertTrue(self.current_user.has_ownership(d))
        self.json_put(
            f"/permissions/add",
            {
                "time": {
                    "from": get_current_time() + timedelta(days=1),
                    "type": "range",
                },
                "id": d.id,
                "type": AccessType.owner.value,
                "groups": ["testuser1"],
                "confirm": False,
            },
            expect_status=403,
            expect_content={"error": "You cannot remove ownership from yourself."},
        )
        db.session.remove()
        d = DocEntry.find_by_id(d.id)
        self.assertTrue(self.current_user.has_ownership(d))
        self.json_put(
            f"/permissions/remove",
            {
                "id": d.id,
                "type": AccessType.owner.value,
                "group": self.get_test_user_1_group_id(),
            },
            expect_status=403,
        )
        db.session.remove()
        d = DocEntry.find_by_id(d.id)
        self.assertTrue(self.current_user.has_ownership(d))

    def test_non_owner_cannot_change_owner(self):
        self.login_test1()
        d = self.create_doc()
        docid = d.id
        self.test_user_2.grant_access(d, AccessType.manage)
        db.session.commit()
        self.login_test2()
        self.json_put(
            f"/permissions/add",
            {
                "time": {
                    "from": get_current_time(),
                    "type": "always",
                },
                "id": docid,
                "type": AccessType.owner.value,
                "groups": ["testuser2"],
                "confirm": False,
            },
            expect_status=403,
        )

    def test_cannot_change_owner_of_personal_folder(self):
        self.login_test1()
        f = self.current_user.get_personal_folder()
        self.json_put(
            f"/permissions/add",
            {
                "id": f.id,
                "type": AccessType.owner.value,
                "groups": ["testuser2"],
                "time": {
                    "type": "always",
                },
                "confirm": False,
            },
            expect_status=403,
            expect_content={"error": "You cannot add owners to your personal folder."},
        )

    def test_trim_whitespace(self):
        self.login_test1()
        f = self.current_user.get_personal_folder()
        self.json_put(
            f"/permissions/add",
            {
                "time": {
                    "from": get_current_time(),
                    "type": "always",
                },
                "id": f.id,
                "type": AccessType.view.value,
                "groups": ["testuser2", "testuser3"],
                "confirm": False,
            },
        )
        f = self.current_user.get_personal_folder()
        self.assertTrue(self.test_user_2.has_view_access(f))
        self.assertTrue(self.test_user_3.has_view_access(f))

    def test_nonexistent_user(self):
        self.login_test1()
        f = self.create_doc()
        self.json_put(
            f"/permissions/add",
            {
                "time": {
                    "from": get_current_time(),
                    "type": "always",
                },
                "id": f.id,
                "type": AccessType.view.value,
                "groups": ["testuser2", "testuserx"],
                "confirm": False,
            },
            expect_content={"not_exist": ["testuserx"]},
        )
        self.json_put(
            f"/permissions/add",
            {
                "time": {
                    "from": get_current_time(),
                    "type": "always",
                },
                "id": f.id,
                "type": AccessType.view.value,
                "groups": ["testuserx"],
                "confirm": False,
            },
            expect_content={"not_exist": ["testuserx"]},
        )

    def test_permissions_get(self):
        self.login_test1()
        for i in (self.current_user.get_personal_folder(), self.create_doc()):
            self.json_put(
                f"/permissions/add",
                {
                    "time": {
                        "from": get_current_time(),
                        "type": "always",
                    },
                    "id": i.id,
                    "type": AccessType.view.value,
                    "groups": ["testuser2"],
                    "confirm": False,
                },
            )
            rights = self.get(f"/permissions/get/{i.id}")
            i = Item.find_by_id(i.id)
            self.assertEqual(
                rights["accesstypes"],
                [
                    {"id": 1, "name": "view"},
                    {"id": 2, "name": "edit"},
                    {"id": 3, "name": "teacher"},
                    {"id": 4, "name": "manage"},
                    {"id": 5, "name": "see answers"},
                    {"id": 6, "name": "owner"},
                    {"id": 7, "name": "copy"},
                ],
            )
            self.assertEqual(
                convert_to_old_format(rights["grouprights"]),
                [
                    {
                        "access_name": "owner",
                        "access_type": 6,
                        "accessible_from": i.block.accesses[
                            (self.get_test_user_1_group_id(), AccessType.owner.value)
                        ].accessible_from.isoformat(),
                        "accessible_to": None,
                        "duration": None,
                        "duration_from": None,
                        "duration_to": None,
                        "fullname": "Test user 1",
                        "gid": self.get_test_user_1_group_id(),
                        "name": "testuser1",
                    },
                    {
                        "access_name": "view",
                        "access_type": 1,
                        "accessible_from": str(
                            i.block.accesses[
                                (self.get_test_user_2_group_id(), AccessType.view.value)
                            ].accessible_from.isoformat()
                        ),
                        "accessible_to": None,
                        "duration": None,
                        "duration_from": None,
                        "duration_to": None,
                        "fullname": "Test user 2",
                        "gid": self.get_test_user_2_group_id(),
                        "name": "testuser2",
                    },
                ],
            )

    def test_logged_in_right(self):
        self.login_test1()
        d = self.create_doc()
        self.json_put(
            f"/permissions/add",
            {
                "time": {
                    "from": get_current_time(),
                    "type": "always",
                },
                "id": d.id,
                "type": AccessType.view.value,
                "groups": ["Logged-in users"],
                "confirm": False,
            },
        )
        self.login_test2()
        self.get(d.url)
        self.login_test1()
        self.json_put(
            f"/permissions/remove",
            {
                "id": d.id,
                "type": AccessType.view.value,
                "group": get_logged_in_group_id(),
            },
        )
        self.login_test2()
        self.get(d.url, expect_status=403)

    def test_recursive_permissions(self):
        self.login_test1()
        paths = [
            "a",
            "b",
            "c",
            "x/a",
            "x/b",
            "x/c",
            "x/x/a",
            "x/x/b",
            "x/x/c",
        ]
        ds = []
        for p in paths:
            d = self.create_doc(self.get_personal_item_path(p))
            ds.append(d)
            t1_f = self.test_user_1.get_personal_folder()
            self.assertFalse(self.test_user_2.has_view_access(d))
            self.assertFalse(self.test_user_2.has_edit_access(d))
            self.assertFalse(self.test_user_2.has_edit_access(t1_f))

        all_ids = [x.id for x in ds]
        self.json_put(
            f"/permissions/edit",
            {
                "groups": ["testuser1"],
                "type": AccessType.owner.value,
                "action": "remove",
                "ids": all_ids,
                "time": {
                    "type": "always",
                },
                "confirm": False,
            },
            expect_status=403,
        )

        self.json_put(
            f"/permissions/edit",
            {
                "groups": ["testuser2", "testuser3"],
                "type": AccessType.view.value,
                "action": "add",
                "ids": all_ids,
                "time": {
                    "type": "always",
                },
                "confirm": False,
            },
        )

        # Refresh the document blocks to make changes visible
        for i in all_ids:
            doc = DocEntry.find_by_id(i)
            db.session.refresh(doc.block)

        t1_f = self.test_user_1.get_personal_folder()
        for p in paths:
            d = DocEntry.find_by_path(self.get_personal_item_path(p))
            self.assertTrue(self.test_user_2.has_view_access(d))
            self.assertTrue(self.test_user_3.has_view_access(d))
            self.assertFalse(self.test_user_2.has_edit_access(d))
            self.assertFalse(self.test_user_2.has_edit_access(t1_f))

        self.json_put(
            f"/permissions/edit",
            {
                "groups": ["testuser2"],
                "type": AccessType.view.value,
                "action": "remove",
                "ids": all_ids,
                "time": {
                    "type": "always",
                },
                "confirm": False,
            },
        )
        t1_f = self.test_user_1.get_personal_folder()
        for p in paths:
            d = DocEntry.find_by_path(self.get_personal_item_path(p))
            self.assertTrue(self.test_user_1.has_ownership(d))
            self.assertFalse(self.test_user_2.has_view_access(d))
            self.assertTrue(self.test_user_3.has_view_access(d))
            self.assertFalse(self.test_user_2.has_edit_access(d))
            self.assertFalse(self.test_user_2.has_edit_access(t1_f))

        self.json_put(
            f"/permissions/edit",
            {
                "groups": ["testuser2", "testuser3"],
                "type": AccessType.view.value,
                "action": "remove",
                "ids": all_ids,
                "time": {
                    "type": "always",
                },
                "confirm": False,
            },
        )
        t1_f = self.test_user_1.get_personal_folder()
        for p in paths:
            d = DocEntry.find_by_path(self.get_personal_item_path(p))
            self.assertTrue(self.test_user_1.has_ownership(d))
            self.assertFalse(self.test_user_2.has_view_access(d))
            self.assertFalse(self.test_user_3.has_view_access(d))
            self.assertFalse(self.test_user_2.has_edit_access(d))
            self.assertFalse(self.test_user_2.has_edit_access(t1_f))

        self.login_test2()
        self.create_doc()
        self.login_test1()
        self.json_put(
            f"/permissions/edit",
            {
                "groups": ["testuser1"],
                "type": AccessType.view.value,
                "action": "add",
                "ids": [
                    x.id
                    for x in self.test_user_2.get_personal_folder().get_all_documents(
                        include_subdirs=True
                    )
                ],
                "time": {
                    "type": "always",
                },
                "confirm": False,
            },
            expect_status=403,
        )
        self.json_put(
            f"/permissions/edit",
            {
                "groups": ["testuser1"],
                "type": "asd",
                "action": "add",
                "ids": all_ids,
                "time": {
                    "type": "always",
                },
                "confirm": False,
            },
            expect_status=422,
        )
        self.json_put(
            f"/permissions/edit",
            {
                "groups": ["nonexistent"],
                "type": AccessType.view.value,
                "action": "add",
                "ids": all_ids,
                "time": {
                    "type": "always",
                },
                "confirm": False,
            },
            expect_status=400,
        )

        # If the user doesn't have access to all documents, the operation should not complete.
        d = DocEntry.find_by_path(self.get_personal_item_path(paths[-1]))
        for a in d.block.accesses.values():
            db.session.delete(a)
        db.session.commit()
        self.json_put(
            f"/permissions/edit",
            {
                "groups": ["testuser2", "testuser3"],
                "type": AccessType.view.value,
                "action": "add",
                "ids": all_ids,
                "time": {
                    "type": "always",
                },
                "confirm": False,
            },
            expect_status=403,
        )
        self.json_put(
            f"/permissions/edit",
            {
                "groups": ["testuser2", "testuser3"],
                "type": AccessType.view.value,
                "action": "remove",
                "ids": all_ids,
                "time": {
                    "type": "always",
                },
                "confirm": False,
            },
            expect_status=403,
        )

    def test_chaining(self):
        self.login_test1()
        d = self.create_doc()
        # Add an expired right
        self.test_user_2.grant_access(
            access_type=AccessType.view,
            accessible_from=get_current_time(),
            accessible_to=get_current_time(),
            block=d,
        )
        db.session.commit()
        d.document.set_settings(
            {
                "auto_confirm": self.get_personal_item_path("nextdoc"),
                "expire_next_doc_message": "My custom message",
            }
        )
        self.login_test2()
        r = self.get(d.url, expect_status=403)
        self.assertIn("auto_confirm document does not exist", r)
        self.login_test1()
        d2 = self.create_doc(path=self.get_personal_item_path("nextdoc"))
        self.login_test2()
        r = self.get(d.url, expect_status=403)
        self.assertIn("Document is not authorized to auto-confirm rights", r)
        d2.document.set_settings(
            {
                "allow_self_confirm_from": d.path,
            }
        )
        r = self.get(d.url, expect_status=403)
        self.assertIn(
            "Cannot get access to target document: No access found for users/test-user-1/nextdoc",
            r,
        )
        self.assertNotIn("My custom message", r)
        self.test_user_2.grant_access(
            access_type=AccessType.view,
            block=d2,
            duration=timedelta(hours=2),
            require_confirm=True,
        )
        db.session.commit()
        r = self.get(d.url, expect_status=403)
        self.assertIn("My custom message", r)
        self.assertIn("Go to the next document", r)

        # Make sure refreshing the page does not change anything.
        r = self.get(d.url, expect_status=403)
        self.assertIn("My custom message", r)
        self.assertIn("Go to the next document", r)

        self.get(d2.url, expect_status=403)
        self.get(d2.url, query_string={"unlock": True})

        # test also range confirm
        self.test_user_2.grant_access(
            access_type=AccessType.view,
            block=d2,
            accessible_to=get_current_time() + timedelta(days=1),
            require_confirm=True,
        )
        db.session.commit()
        # make sure can't access at first
        self.get(d2.url, expect_status=403)
        # this does the autoconfirm
        r = self.get(d.url, expect_status=403)
        self.assertIn("My custom message", r)
        self.assertIn("Go to the next document", r)
        # and now there should be access
        self.get(d2.url)

    def test_chaining_logged_in(self):
        self.login_test1()
        d = self.create_doc()
        self.test_user_2.grant_access(
            access_type=AccessType.view,
            accessible_from=get_current_time(),
            accessible_to=get_current_time(),
            block=d,
        )
        db.session.commit()
        d.document.set_settings(
            {
                "auto_confirm": self.get_personal_item_path("nextdoc2"),
                "expire_next_doc_message": "My custom message",
            }
        )
        d2 = self.create_doc(path=self.get_personal_item_path("nextdoc2"))
        d2.document.set_settings(
            {
                "allow_self_confirm_from": d.path,
            }
        )
        grant_access(
            group=UserGroup.get_logged_in_group(),
            block=d2,
            access_type=AccessType.view,
            require_confirm=True,
        )
        db.session.commit()
        self.login_test2()
        self.get(d2.url, expect_status=403)
        self.get(d.url, expect_status=403)
        self.get(d2.url)

    def test_confirm_with_end_date(self):
        self.login_test1()
        d = self.create_doc()
        self.json_put(
            f"/permissions/add",
            {
                "time": {
                    "from": None,
                    "to": get_current_time() + timedelta(days=1),
                    "type": "range",
                },
                "id": d.id,
                "type": AccessType.view.value,
                "groups": ["testuser2"],
                "confirm": True,
            },
            expect_content={"not_exist": []},
        )
        self.login_test2()
        self.get(d.url, expect_status=403)
        self.login_test1()
        self.json_put(
            "/permissions/confirm",
            {
                "id": d.id,
                "type": AccessType.view.value,
                "group": self.get_test_user_2_group_id(),
            },
        )
        self.login_test2()
        self.get(d.url)

        self.login_test1()
        self.json_put(
            "/permissions/confirm",
            {
                "id": d.id,
                "type": AccessType.view.value,
                "group": self.get_test_user_2_group_id(),
            },
            expect_status=400,
            expect_content="view right for testuser2 does not require confirmation or it was already confirmed.",
        )

    def test_cannot_auto_confirm_too_early(self):
        self.login_test1()
        d = self.create_doc()
        self.test_user_2.grant_access(
            access_type=AccessType.view,
            accessible_from=None,
            accessible_to=get_current_time() + timedelta(days=1),
            require_confirm=True,
            block=d,
        )
        db.session.commit()
        d2 = self.create_doc()
        d.document.set_settings(
            {
                "auto_confirm": d2.path,
                "expire_next_doc_message": "My custom message",
            }
        )
        d2.document.set_settings(
            {
                "allow_self_confirm_from": d.path,
            }
        )
        self.login_test2()
        self.test_user_2.grant_access(
            access_type=AccessType.view,
            block=d2,
            duration=timedelta(hours=2),
            require_confirm=True,
        )
        db.session.commit()
        r = self.get(d.url, expect_status=403)
        self.assertNotIn("My custom message", r)
        self.assertNotIn("Go to the next document", r)

        self.test_user_2.grant_access(
            access_type=AccessType.view,
            duration=timedelta(days=1),
            require_confirm=True,
            block=d,
        )
        db.session.commit()
        r = self.get(d.url, expect_status=403)
        self.assertNotIn("My custom message", r)
        self.assertNotIn("Go to the next document", r)

        self.test_user_2.grant_access(
            access_type=AccessType.view,
            duration=timedelta(days=1),
            require_confirm=False,
            block=d,
        )
        db.session.commit()
        r = self.get(d.url, expect_status=403)
        self.assertNotIn("My custom message", r)
        self.assertNotIn("Go to the next document", r)

    def test_auto_confirm_translation(self):
        self.login_test1()
        d = self.create_doc()
        tr = self.create_translation(d)
        self.test_user_2.grant_access(
            access_type=AccessType.view,
            accessible_to=get_current_time() - timedelta(days=1),
            block=tr,
        )
        db.session.commit()
        d2 = self.create_doc()
        d2tr = self.create_translation(d2)
        trid = d2tr.id
        d.document.set_settings(
            {
                "auto_confirm": d2.path,
                "expire_next_doc_message": "My custom message",
            }
        )
        d2.document.set_settings(
            {
                "allow_self_confirm_from": d.path,
            }
        )
        d = DocEntry.find_by_id(d.id)
        d2 = DocEntry.find_by_id(d2.id)
        d2.document.clear_mem_cache()
        synchronize_translations(
            d, DocumentEditResult(added=d.document.get_paragraphs())
        )
        synchronize_translations(
            d2, DocumentEditResult(added=d2.document.get_paragraphs())
        )
        self.login_test2()
        d2tr = DocEntry.find_by_id(trid)
        self.test_user_2.grant_access(
            access_type=AccessType.view,
            block=d2tr,
            accessible_to=get_current_time() + timedelta(days=1),
            require_confirm=True,
        )
        db.session.commit()
        self.get(d2tr.url, expect_status=403)
        self.get(d2tr.url, expect_status=403)

        r = self.get(tr.url, expect_status=403)
        self.assertIn("My custom message", r)
        self.assertIn("Go to the next document", r)
        self.get(d2tr.url)

    def test_duration_right(self):
        self.login_test1()
        d = self.create_doc()
        self.json_put(
            f"/permissions/add",
            {
                "time": {
                    "from": None,
                    "to": None,
                    "type": "duration",
                    "duration": "PT4H",
                },
                "id": d.id,
                "type": AccessType.view.value,
                "groups": ["testuser2"],
                "confirm": False,
            },
        )

    def test_mass_permission_remove_partially_redundant(self):
        """Rights are removed even if the selection contains redundant rows."""
        self.login_test1()
        d = self.create_doc()
        d2 = self.create_doc()
        self.test_user_2.grant_access(d, access_type=AccessType.view)
        db.session.commit()
        self.json_put(
            f"/permissions/edit",
            {
                "groups": ["testuser2"],
                "type": AccessType.view.value,
                "action": "remove",
                "ids": [d.id, d2.id],
                "time": {
                    "type": "always",
                },
                "confirm": False,
            },
        )
        # The original bug was that db.session.commit() was not getting called when it should have,
        # so doing a rollback here should reveal the bug in the assert if it reappears.
        # TODO this rollback should probably be removed, since it will unconditionally expire session objects.
        #  This may cause issues with other tests. See https://docs.sqlalchemy.org/en/13/errors.html#error-bhk3
        db.session.rollback()
        d = DocEntry.find_by_id(d.id)
        self.assertFalse(self.test_user_2.has_view_access(d))

    def test_access_denied_message(self):
        self.login_test1()
        d = self.create_doc()
        self.login_test2()
        self.get(
            d.url,
            expect_status=403,
            expect_contains="Sorry, you don't have permission to access this resource.",
        )
        d.document.set_settings({"access_denied_message": "You cannot see this."})
        self.get(d.url, expect_status=403, expect_contains="You cannot see this.")

    def test_inherit_rights_from_folder(self):
        self.login_test1()
        d = self.create_doc(
            path=self.get_personal_item_path("x/test"),
            initial_par="#- {plugin=textfield #t}",
        )
        uf = self.upload_file(d, b"test", "test.txt")
        tr = self.create_translation(d)
        f = d.parent
        self.test_user_2.grant_access(f, AccessType.view)
        db.session.commit()
        self.login_test2()
        self.get(d.url, expect_status=403)
        self.get(tr.url, expect_status=403)
        self.get(f'/files/{uf["file"]}', expect_status=403)
        with self.temp_config({"INHERIT_FOLDER_RIGHTS_DOCS": {d.path}}):
            self.get(d.url)
            self.get(tr.url)
            self.get(f'/files/{uf["file"]}')
            self.mark_as_read(d, d.document.get_paragraphs()[0].get_id())
            self.post_answer("textfield", f"{d.id}.t", user_input={"c": "x"})

    def test_no_fulltext_in_manage_with_view(self):
        self.login_test1()
        d = self.create_doc(
            initial_par="""
#- {plugin=csPlugin}
-pointsRule: 
  expectCode: "secret answer"
"""
        )
        self.test_user_2.grant_access(d, AccessType.view)
        db.session.commit()
        self.login_test2()
        r = self.get(d.get_url_for_view("manage"))
        self.assertNotIn("secret answer", r)

    def test_permission_clear(self):
        self.login_test1()
        d = self.create_doc()
        self.test_user_2.grant_access(d, AccessType.view)
        self.test_user_2.grant_access(d, AccessType.edit)
        db.session.commit()
        db.session.refresh(d)
        self.assertTrue(self.test_user_2.has_edit_access(d))
        self.assertTrue(self.test_user_2.has_view_access(d))
        self.json_put(
            "/permissions/clear",
            {
                "paths": [d.path],
                "type": AccessType.edit.value,
            },
        )
        d = DocEntry.find_by_id(d.id)
        self.assertFalse(self.test_user_2.has_edit_access(d))
        self.assertTrue(self.test_user_2.has_view_access(d))
        self.json_put(
            "/permissions/clear",
            {
                "paths": [d.path],
                "type": AccessType.view.value,
            },
        )
        d = DocEntry.find_by_id(d.id)
        self.assertFalse(self.test_user_2.has_edit_access(d))
        self.assertFalse(self.test_user_2.has_view_access(d))
        self.assertTrue(self.test_user_1.has_ownership(d))

    def test_copy_translation_permissions(self):
        self.login_test1()

        # test copying doc perms when creating translation
        d = self.create_doc()
        self.json_put(
            f"/permissions/add",
            {
                "groups": ["testuser2"],
                "type": AccessType.edit.value,
                "action": "add",
                "id": d.id,
                "time": {
                    "type": "always",
                },
                "confirm": False,
            },
        )
        t = self.create_translation(d, "translation", "en")
        self.assertTrue(self.test_user_2.has_edit_access(t))

    def test_add_translation_permissions(self):
        self.login_test1()

        # test adding perms to translations when adding doc perms
        d = self.create_doc()
        t = self.create_translation(d, "translation", "en")
        self.json_put(
            f"/permissions/add",
            {
                "groups": ["testuser2", "testuser3"],
                "type": AccessType.view.value,
                "id": d.id,
                "time": {
                    "type": "always",
                },
                "confirm": False,
            },
            expect_status=200,
        )

        # Refresh the block to make db changes visible
        t = DocEntry.find_by_id(t.id)
        db.session.refresh(t.block)
        # print(t.block.accesses, t.is_original_translation)

        self.assertTrue(self.test_user_2.has_view_access(t))
        self.assertTrue(self.test_user_3.has_view_access(t))

    def test_remove_translation_permissions(self):
        # test removing perms from translations
        self.login_test1()
        d = self.create_doc()
        self.test_user_2.grant_access(d, AccessType.edit)
        self.test_user_3.grant_access(d, AccessType.edit)
        db.session.commit()

        t = self.create_translation(d, "translation", "en")

        self.assertTrue(self.test_user_2.has_view_access(t))
        self.assertTrue(self.test_user_3.has_view_access(t))

        self.json_put(
            f"/permissions/remove",
            {
                "id": d.id,
                "type": AccessType.edit.value,
                "group": self.test_user_2.get_personal_group().id,
            },
            expect_status=200,
        )
        self.json_put(
            f"/permissions/remove",
            {
                "id": d.id,
                "type": AccessType.edit.value,
                "group": self.test_user_3.get_personal_group().id,
            },
            expect_status=200,
        )
        # Refresh the block to make db changes visible
        d = DocEntry.find_by_id(d.id)
        t = DocEntry.find_by_id(t.id)
        db.session.refresh(d.block)
        db.session.refresh(t.block)

        self.assertFalse(self.test_user_2.has_view_access(t))
        self.assertFalse(self.test_user_3.has_view_access(t))

    def test_mass_edit_translation_permissions(self):
        self.login_test1()
        d1 = self.create_doc()
        d2 = self.create_doc()
        t1 = self.create_translation(d1, "translation 1", "en")
        t2 = self.create_translation(d2, "translation 2", "en")

        self.json_put(
            f"/permissions/edit",
            {
                "groups": ["testuser2", "testuser3"],
                "type": AccessType.view.value,
                "action": "add",
                "ids": [d1.id, d2.id],
                "time": {
                    "type": "always",
                },
                "confirm": False,
            },
        )

        # Refresh the blocks to make db changes visible
        t1 = DocEntry.find_by_id(t1.id)
        t2 = DocEntry.find_by_id(t2.id)
        db.session.refresh(t1.block)
        db.session.refresh(t2.block)

        self.assertTrue(self.test_user_2.has_view_access(t1))
        self.assertTrue(self.test_user_2.has_view_access(t2))
        self.assertTrue(self.test_user_3.has_view_access(t1))
        self.assertTrue(self.test_user_3.has_view_access(t2))

        self.json_put(
            f"/permissions/edit",
            {
                "groups": ["testuser2", "testuser3"],
                "type": AccessType.view.value,
                "action": "remove",
                "ids": [d1.id, d2.id],
                "time": {
                    "type": "always",
                },
                "confirm": False,
            },
        )

        # Refresh the blocks to make db changes visible
        t1 = DocEntry.find_by_id(t1.id)
        t2 = DocEntry.find_by_id(t2.id)
        db.session.refresh(t1.block)
        db.session.refresh(t2.block)

        self.assertFalse(self.test_user_2.has_view_access(t1))
        self.assertFalse(self.test_user_2.has_view_access(t2))
        self.assertFalse(self.test_user_3.has_view_access(t1))
        self.assertFalse(self.test_user_3.has_view_access(t2))

    def test_confirm_translation_permissions(self):
        self.login_test1()
        d = self.create_doc()
        t1 = self.create_translation(d, "translation 1", "en")
        t2 = self.create_translation(d, "translation 2", "sv")

        self.json_put(
            f"/permissions/add",
            {
                "time": {
                    "from": None,
                    "to": get_current_time() + timedelta(days=1),
                    "type": "range",
                },
                "id": d.id,
                "type": AccessType.edit.value,
                "groups": ["testuser2", "testuser3"],
                "confirm": True,
            },
            expect_status=200,
        )

        self.assertFalse(self.test_user_2.has_view_access(d))
        self.assertFalse(self.test_user_3.has_view_access(d))
        self.assertFalse(self.test_user_2.has_view_access(t1))
        self.assertFalse(self.test_user_2.has_view_access(t2))
        self.assertFalse(self.test_user_3.has_view_access(t1))
        self.assertFalse(self.test_user_3.has_view_access(t2))

        self.json_put(
            "/permissions/confirm",
            {
                "id": d.id,
                "type": AccessType.edit.value,
                "group": self.test_user_2.get_personal_group().id,
            },
        )

        self.json_put(
            "/permissions/confirm",
            {
                "id": d.id,
                "type": AccessType.edit.value,
                "group": self.test_user_3.get_personal_group().id,
            },
        )

        # Refresh the blocks to make db changes visible
        d = DocEntry.find_by_id(d.id)
        t1 = DocEntry.find_by_id(t1.id)
        t2 = DocEntry.find_by_id(t2.id)
        db.session.refresh(d.block)
        db.session.refresh(t1.block)
        db.session.refresh(t2.block)

        self.assertTrue(self.test_user_2.has_view_access(d))
        self.assertTrue(self.test_user_3.has_view_access(d))
        self.assertTrue(self.test_user_2.has_view_access(t1))
        self.assertTrue(self.test_user_2.has_view_access(t2))
        self.assertTrue(self.test_user_3.has_view_access(t1))
        self.assertTrue(self.test_user_3.has_view_access(t2))
