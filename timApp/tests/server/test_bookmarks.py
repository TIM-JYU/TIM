import re

from flask import g

from timApp.bookmark.bookmarks import Bookmarks
from timApp.document.docentry import DocEntry
from timApp.document.docinfo import DocInfo
from timApp.folder.folder import Folder
from timApp.item.tag import Tag, TagType
from timApp.sisu.scimusergroup import ScimUserGroup
from timApp.tests.server.timroutetest import TimRouteTest
from timApp.timdb.sqa import db
from timApp.user.user import User, UserInfo
from timApp.user.usergroup import UserGroup


class BookmarkTestBase(TimRouteTest):
    def get_bookmarks(self, expect_status=200):
        bms = self.get("/bookmarks/get", expect_status=expect_status)
        # Pop the user from the active globals because the above GET will expire it (since session is expired)
        g.pop("user", None)
        return bms


class BookmarkTest(BookmarkTestBase):
    def test_bookmarks(self):
        self.login_test1()
        bookmarks = self.get_bookmarks()

        # Test to make sure an invalid folder with empty name is not created
        f = Folder.find_by_location(self.current_user.get_personal_folder().path, "")
        self.assertIsNone(f)

        self.assertEqual([], bookmarks)
        group_name = "mygroup"
        group_name2 = "mygroup2"
        item = "test item"
        item_path = "some/path/to/item"
        bookmarks = self.post(f"/bookmarks/createGroup/{group_name}")
        self.assertEqual(
            [{"name": "mygroup", "items": [], "editable": True}], bookmarks
        )
        bookmarks = self.json_post(
            "/bookmarks/add", {"group": group_name2, "name": item, "link": item_path}
        )
        self.assertEqual(
            [
                {"items": [], "name": group_name, "editable": True},
                {
                    "items": [{"name": item, "link": item_path}],
                    "name": group_name2,
                    "editable": True,
                },
            ],
            bookmarks,
        )

        bookmarks = self.json_post("/bookmarks/deleteGroup", {"group": group_name})
        self.assertEqual(
            [
                {
                    "items": [{"name": item, "link": item_path}],
                    "name": group_name2,
                    "editable": True,
                }
            ],
            bookmarks,
        )
        bookmarks = self.json_post("/bookmarks/deleteGroup", {"group": group_name})
        self.assertEqual(
            [
                {
                    "items": [{"name": item, "link": item_path}],
                    "name": group_name2,
                    "editable": True,
                }
            ],
            bookmarks,
        )

        bookmarks = self.json_post(
            "/bookmarks/delete", {"group": group_name2, "name": item}
        )
        self.assertEqual(
            [{"items": [], "name": group_name2, "editable": True}], bookmarks
        )
        bookmarks = self.json_post(
            "/bookmarks/delete", {"group": group_name2, "name": item}
        )
        self.assertEqual(
            [{"items": [], "name": group_name2, "editable": True}], bookmarks
        )

        bookmarks = self.json_post(
            "/bookmarks/add", {"group": group_name2, "name": item, "link": item_path}
        )
        self.assertEqual(
            [
                {
                    "items": [{"name": item, "link": item_path}],
                    "name": group_name2,
                    "editable": True,
                }
            ],
            bookmarks,
        )
        bookmarks = self.json_post(
            "/bookmarks/edit",
            {
                "old": {"group": group_name2, "name": item},
                "new": {"group": group_name2, "name": item, "link": "test"},
            },
        )
        self.assertEqual(
            [
                {
                    "items": [{"name": item, "link": "test"}],
                    "name": group_name2,
                    "editable": True,
                }
            ],
            bookmarks,
        )
        self.logout()
        self.get_bookmarks(expect_status=403)

    def test_recently_edited(self):
        self.login_test2()
        d = self.create_doc()
        view = "/view/"
        self.assertEqual(
            [
                {
                    "name": "Last edited",
                    "items": [{"name": d.title, "link": view + d.path}],
                    "editable": False,
                }
            ],
            self.get_bookmarks(),
        )
        d2 = self.create_doc()
        self.assertEqual(
            [
                {
                    "name": "Last edited",
                    "items": [
                        {"name": d2.title, "link": view + d2.path},
                        {"name": d.title, "link": view + d.path},
                    ],
                    "editable": False,
                }
            ],
            self.get_bookmarks(),
        )
        d3 = self.create_doc()
        self.assertEqual(
            [
                {
                    "name": "Last edited",
                    "items": [
                        {"name": d3.title, "link": view + d3.path},
                        {"name": d2.title, "link": view + d2.path},
                        {"name": d.title, "link": view + d.path},
                    ],
                    "editable": False,
                }
            ],
            self.get_bookmarks(),
        )
        self.new_par(d.document, "test")
        self.assertEqual(
            [
                {
                    "name": "Last edited",
                    "items": [
                        {"name": d.title, "link": view + d.path},
                        {"name": d3.title, "link": view + d3.path},
                        {"name": d2.title, "link": view + d2.path},
                    ],
                    "editable": False,
                }
            ],
            self.get_bookmarks(),
        )
        d4 = self.create_doc()
        # LAST_EDITED_BOOKMARK_LIMIT = 3 when testing
        self.assertEqual(
            [
                {
                    "name": "Last edited",
                    "items": [
                        {"name": d4.title, "link": view + d4.path},
                        {"name": d.title, "link": view + d.path},
                        {"name": d3.title, "link": view + d3.path},
                    ],
                    "editable": False,
                }
            ],
            self.get_bookmarks(),
        )

    def test_bookmark_migration_to_db(self):
        adm, _ = User.create_with_group(
            info=UserInfo(username="someadmin"), is_admin=True
        )
        db.session.commit()
        self.get("/ping")
        self.login(username="someadmin")
        d = self.create_doc(
            path=f"{self.test_user_3.get_personal_folder().path}/Bookmarks",
            initial_par="""
``` {settings=""}
bookmarks:
- testgroup:
  - testlink: https://example.com
```""",
        )
        b = Bookmarks(self.test_user_3)
        self.assertEqual(
            [
                {
                    "editable": True,
                    "items": [{"link": "https://example.com", "name": "testlink"}],
                    "name": "testgroup",
                }
            ],
            b.as_dict(),
        )
        b.add_bookmark("testgroup", "testlink2", "https://example.com/2")
        b.save_bookmarks()
        db.session.commit()
        md = d.document.export_markdown(export_ids=False)
        self.assertEqual(
            """
``` {settings=""}
bookmarks:
- testgroup:
  - testlink: https://example.com
```""".strip()
            + "\n",
            md,
        )
        b = Bookmarks(self.test_user_3)
        self.assertEqual(
            [
                {
                    "editable": True,
                    "items": [
                        {"link": "https://example.com/2", "name": "testlink2"},
                        {"link": "https://example.com", "name": "testlink"},
                    ],
                    "name": "testgroup",
                }
            ],
            b.as_dict(),
        )


class BookmarkTest2(BookmarkTestBase):
    def test_automatic_course_bookmark_update(self):
        self.login_test1()
        d = self.create_doc()
        d.block.tags.append(Tag(name="TIEP111", type=TagType.CourseCode))
        d.block.tags.append(Tag(name="group:ohj1opiskelijat", type=TagType.Regular))
        db.session.commit()
        d2 = self.create_doc()
        d2.block.tags.append(Tag(name="TIEP112", type=TagType.CourseCode))
        d2.block.tags.append(Tag(name="group:ohj2opiskelijat", type=TagType.Regular))
        db.session.commit()
        ug = UserGroup(name="ohj1opiskelijat", display_name="asd asd")
        tu1 = self.test_user_1
        tu1.groups.append(ug)
        # Flush here to simulate separate group add events
        db.session.flush()
        ug.external_id = ScimUserGroup(external_id="jy-CUR-4668-students")
        ug = UserGroup(name="ohj2opiskelijat", display_name="asd asd")
        tu1.groups.append(ug)
        ug.external_id = ScimUserGroup(external_id="jy-CUR-4669-students")
        db.session.commit()
        self.get("/")
        tu1 = User.get_by_name(self.test_user_1.name)
        b = Bookmarks(tu1)
        self.assertEqual(
            {
                "editable": True,
                "items": [
                    {"link": d2.url_relative, "name": d2.title},
                    {"link": d.url_relative, "name": d.title},
                ],
                "name": "My courses",
            },
            b.as_dict()[1],
            "Bookmarks should contain course bookmarks automatically",
        )
        self.get("/")
        tu1 = User.get_by_name(self.test_user_1.name)
        b = Bookmarks(tu1)
        self.assertEqual(
            {
                "editable": True,
                "items": [
                    {"link": d2.url_relative, "name": d2.title},
                    {"link": d.url_relative, "name": d.title},
                ],
                "name": "My courses",
            },
            b.as_dict()[1],
            "Bookmarks list should not change after second visit",
        )
        self.json_post(f"/bookmarks/delete", {"group": "My courses", "name": d.title})
        tu1 = User.get_by_name(self.test_user_1.name)
        b = Bookmarks(tu1)
        self.assertEqual(
            [
                {
                    "editable": True,
                    "items": [
                        {"link": d2.url_relative, "name": d2.title},
                    ],
                    "name": "My courses",
                },
                {
                    "editable": True,
                    "items": [
                        {"link": d.url_relative, "name": d.title},
                    ],
                    "name": "Hidden courses",
                },
            ],
            b.as_dict()[1:],
            "Automatically enrolled courses should be hidden instead of deleted",
        )
        self.get("/")
        tu1 = User.get_by_name(self.test_user_1.name)
        b = Bookmarks(tu1)
        self.assertEqual(
            [
                {
                    "editable": True,
                    "items": [
                        {"link": d2.url_relative, "name": d2.title},
                    ],
                    "name": "My courses",
                },
                {
                    "editable": True,
                    "items": [
                        {"link": d.url_relative, "name": d.title},
                    ],
                    "name": "Hidden courses",
                },
            ],
            b.as_dict()[1:],
            "Hidden course should not appear in bookmarks after automatic update",
        )

        # If the group name is changed, the tag name does not change automatically.
        # Make sure a warning is displayed.
        ug = UserGroup.get_by_name("ohj2opiskelijat")
        ug.name = "someothername"
        db.session.commit()
        self.assertIn(
            "Document has incorrect group tags: ohj2opiskelijat", self.get(d2.url)
        )

    def refresh(self, i: DocInfo):
        return DocEntry.find_by_path(i.path)

    def test_manual_enroll(self):
        self.login_test1()
        d = self.create_doc()
        path = d.path
        params = {"path": path, "require_group": True}
        self.json_post(
            "/bookmarks/addCourse",
            params,
            expect_status=400,
            expect_content="Document is not tagged as a course",
        )
        d = self.refresh(d)
        d.block.tags.append(Tag(type=TagType.CourseCode, name="XXXX111"))
        db.session.commit()
        self.json_post(
            "/bookmarks/addCourse",
            params,
            expect_status=400,
            expect_content="Course does not allow manual enrollment.",
        )
        d.document.set_settings({"course_allow_manual_enroll": True})
        self.json_post(
            "/bookmarks/addCourse",
            params,
            expect_status=400,
            expect_content="Document does not have associated course group",
        )
        d.document.add_setting("groups", ["testcourse"])
        d.document.add_setting("group", "anothertestcourse")
        r = self.json_post(
            "/bookmarks/addCourse",
            params,
            expect_status=400,
        )
        err = r["error"]
        self.assertIn("Multiple course groups found", err)
        self.assertSetEqual(
            {m for m in re.findall("'(.*?)'", err)}, {"testcourse", "anothertestcourse"}
        )
        d.document.set_settings(
            {"course_allow_manual_enroll": True, "group": "testcourse"}
        )
        self.json_post(
            "/bookmarks/addCourse",
            params,
            expect_status=400,
            expect_content='The specified course group "testcourse" does not exist.',
        )
        UserGroup.create("testcourse")
        db.session.commit()
        self.json_post(
            "/bookmarks/addCourse",
            params,
            expect_status=400,
            expect_content="Document group setting not found in tags.",
        )
        d = self.refresh(d)
        d.block.tags.append(Tag(type=TagType.Regular, name="group:testcourse"))
        db.session.commit()
        self.json_post(
            "/bookmarks/addCourse",
            params,
            expect_status=400,
            expect_content='Some of the document owners does not have edit access to the course group "testcourse".',
        )
        nd = self.create_doc()
        ug = UserGroup.get_by_name("testcourse")
        ug.admin_doc = nd.block
        db.session.commit()
        r = self.json_post(
            "/bookmarks/addCourse",
            params,
        )
        self.assertTrue(r["added_to_group"])
        ug = UserGroup.get_by_name("testcourse")
        self.assertIn(self.test_user_1, ug.users)
        self.json_post(f"/bookmarks/addCourse", {"path": path})
        self.json_post(f"/bookmarks/delete", {"group": "My courses", "name": d.title})
        ug = UserGroup.get_by_name("testcourse")
        self.assertNotIn(self.test_user_1, ug.users)
        d = self.create_doc()
        self.json_post(
            f"/bookmarks/add",
            {"group": "My courses", "name": d.title, "link": d.url_relative},
        )
        self.json_post(f"/bookmarks/delete", {"group": "My courses", "name": d.title})
