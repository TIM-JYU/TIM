from timApp.item.tag import TagType
from timApp.tests.server.timroutetest import TimRouteTest
from timApp.timdb.sqa import db
from timApp.user.special_group_names import TEACHERS_GROUPNAME
from timApp.user.usergroup import UserGroup


class TagTest(TimRouteTest):
    def test_tag_adding_without_manage(self):
        self.login_test3()
        d = self.create_doc()
        self.login_test1()
        self.json_post(
            f"/tags/add/{d.path}",
            {
                "tags": [
                    {"name": "test", "expires": None, "type": TagType.Regular},
                    {"name": "test2", "expires": None, "type": TagType.Regular},
                ]
            },
            expect_status=403,
        )

    def test_tag_adding_with_manage(self):
        self.login_test1()
        d = self.create_doc()
        self.json_post(
            f"/tags/add/{d.path}",
            {
                "tags": [
                    {"name": "test", "expires": None, "type": TagType.Regular},
                    {"name": "test2", "expires": None, "type": TagType.Regular},
                ]
            },
        )

    def test_tag_adding_with_special_chars(self):
        self.login_test1()
        d = self.create_doc()
        self.json_post(
            f"/tags/add/{d.path}",
            {
                "tags": [
                    {"name": "test", "expires": None, "type": TagType.Regular},
                    {"name": "test2#¤%&/()=", "expires": None, "type": TagType.Regular},
                ]
            },
            expect_status=200,
        )

    def test_special_tag_adding_without_rights(self):
        self.login_test1()
        d = self.create_doc()
        self.json_post(
            f"/tags/add/{d.path}",
            {
                "tags": [
                    {"name": "TEST123", "expires": None, "type": TagType.CourseCode},
                    {
                        "name": "testing subject",
                        "expires": None,
                        "type": TagType.Subject,
                    },
                ]
            },
            expect_status=400,
            expect_content={
                "error": f"Managing this tag requires admin or {TEACHERS_GROUPNAME} rights."
            },
        )

    def test_special_tag_adding_with_teachers_rights(self):
        u = self.test_user_3
        teachers_group = UserGroup.get_by_name(TEACHERS_GROUPNAME)
        if u not in teachers_group.users:
            u.groups.append(teachers_group)
        db.session.commit()
        self.login_test3()
        d = self.create_doc()
        self.json_post(
            f"/tags/add/{d.path}",
            {
                "tags": [
                    {"name": "TEST123", "expires": None, "type": TagType.CourseCode},
                    {
                        "name": "testing subject",
                        "expires": None,
                        "type": TagType.Subject,
                    },
                ]
            },
        )

    def test_special_tag_adding_with_admin_rights(self):
        self.login_test1()
        d = self.create_doc()
        u = self.test_user_2
        self.make_admin(u)
        d_path = d.path
        self.login_test2()
        self.json_post(
            f"/tags/add/{d_path}",
            {
                "tags": [
                    {"name": "TEST123", "expires": None, "type": TagType.CourseCode},
                    {
                        "name": "testing subject",
                        "expires": None,
                        "type": TagType.Subject,
                    },
                ]
            },
        )

    def test_get_docs_by_tag(self):
        self.login_test1()
        d = self.create_doc()
        self.json_post(
            f"/tags/add/{d.path}",
            {
                "tags": [
                    {"name": "test", "expires": None, "type": TagType.Regular},
                    {"name": "test2", "expires": None, "type": TagType.Regular},
                ]
            },
        )
        self.get(
            f"/tags/getTags/{d.path}",
            expect_content=[
                {
                    "name": "test",
                    "expires": None,
                    "block_id": d.id,
                    "type": TagType.Regular.value,
                },
                {
                    "name": "test2",
                    "expires": None,
                    "block_id": d.id,
                    "type": TagType.Regular.value,
                },
            ],
        )

    def test_adding_duplicate_tag(self):
        self.login_test1()
        d = self.create_doc()
        self.json_post(
            f"/tags/add/{d.path}",
            {
                "tags": [
                    {"name": "test", "expires": None, "type": TagType.Regular},
                    {"name": "test", "expires": None, "type": TagType.Regular},
                ]
            },
            expect_status=400,
            expect_content={"error": "Tag name is already in use."},
        )

    def test_get_all_tags(self):
        self.login_test1()
        d = self.create_doc()
        d2 = self.create_doc()
        self.json_post(
            f"/tags/add/{d.path}",
            {
                "tags": [
                    {"name": "test", "expires": None, "type": TagType.Regular},
                    {"name": "test2", "expires": None, "type": TagType.Regular},
                ]
            },
        )
        self.json_post(
            f"/tags/add/{d2.path}",
            {"tags": [{"name": "test3", "expires": None, "type": TagType.Regular}]},
        )
        self.get(
            f"/tags/getAllTags",
            expect_status=200,
            expect_content=["test", "test2", "test3"],
        )

    def test_tag_removal_without_manage(self):
        self.login_test3()
        d = self.create_doc()
        self.json_post(
            f"/tags/add/{d.path}",
            {
                "tags": [
                    {"name": "test", "expires": None, "type": TagType.Regular},
                    {"name": "test2", "expires": None, "type": TagType.Regular},
                ]
            },
        )
        self.login_test1()
        self.json_post(
            f"/tags/remove/{d.path}",
            {"tag": {"name": "test", "expires": None, "type": TagType.Regular}},
            expect_status=403,
        )

    def test_tag_removal_with_manage(self):
        self.login_test1()
        d = self.create_doc()
        self.json_post(
            f"/tags/add/{d.path}",
            {
                "tags": [
                    {"name": "test", "expires": None, "type": TagType.Regular},
                    {"name": "test2", "expires": None, "type": TagType.Regular},
                ]
            },
        )
        self.json_post(
            f"/tags/remove/{d.path}",
            {"tag": {"name": "test", "expires": None, "type": TagType.Regular}},
        )

    def test_special_tag_removal_without_rights(self):
        u = self.test_user_3
        self.login_test3()
        d = self.create_doc()
        self.json_post(
            f"/tags/add/{d.path}",
            {
                "tags": [
                    {"name": "TEST123", "expires": None, "type": TagType.CourseCode},
                    {
                        "name": "testing subject",
                        "expires": None,
                        "type": TagType.Subject,
                    },
                ]
            },
        )

        self.login_test1()
        self.json_post(
            f"/tags/remove/{d.path}",
            {
                "tag": {
                    "name": "TEST123",
                    "expires": None,
                    "type": TagType.CourseCode,
                }
            },
            expect_status=403,
        )

    def test_special_tag_removal_with_teachers_rights(self):
        u = self.test_user_3
        teachers_group = UserGroup.get_by_name(TEACHERS_GROUPNAME)
        if u not in teachers_group.users:
            u.groups.append(teachers_group)
        db.session.commit()
        self.login_test3()
        d = self.create_doc()
        self.json_post(
            f"/tags/add/{d.path}",
            {
                "tags": [
                    {"name": "TEST123", "expires": None, "type": TagType.CourseCode},
                    {
                        "name": "testing subject",
                        "expires": None,
                        "type": TagType.Subject,
                    },
                ]
            },
        )

        self.login_test1()
        self.json_post(
            f"/tags/remove/{d.path}",
            {
                "tag": {
                    "name": "TEST123",
                    "expires": None,
                    "type": TagType.CourseCode,
                }
            },
            expect_status=403,
        )

    def test_get_document_by_id(self):
        u = self.test_user_1
        self.login_test1()
        d = self.create_doc()
        self.json_post(
            f"/tags/add/{d.path}",
            {
                "tags": [
                    {"name": "test", "expires": None, "type": TagType.Regular},
                    {"name": "test2", "expires": None, "type": TagType.Regular},
                ]
            },
        )

        self.get(
            f"/tags/getDoc/{d.id}",
            expect_content={
                "id": d.id,
                "isFolder": False,
                "location": d.location,
                "modified": "just now",
                "name": "doc5",
                "owners": [{"id": self.get_test_user_1_group_id(), "name": u.name}],
                "path": d.path,
                "public": True,
                "rights": {
                    "browse_own_answers": True,
                    "can_comment": True,
                    "can_mark_as_read": True,
                    "copy": True,
                    "editable": True,
                    "manage": True,
                    "owner": True,
                    "see_answers": True,
                    "teacher": True,
                },
                "tags": [
                    {
                        "block_id": d.id,
                        "expires": None,
                        "name": "test",
                        "type": TagType.Regular.value,
                    },
                    {
                        "block_id": d.id,
                        "expires": None,
                        "name": "test2",
                        "type": TagType.Regular.value,
                    },
                ],
                "title": d.title,
                "unpublished": True,
                "visibility": 5,
            },
        )

    def test_tag_edit(self):
        self.login_test1()
        d = self.create_doc()
        old_tag = {"name": "cat", "expires": None, "type": TagType.Regular}
        new_tag = {"name": "dog", "expires": None, "type": TagType.Regular}
        self.json_post(f"/tags/add/{d.path}", {"tags": [old_tag]})
        self.json_post(
            f"/tags/edit/{d.path}",
            {"old_tag": old_tag, "new_tag": new_tag},
            expect_status=200,
        )
