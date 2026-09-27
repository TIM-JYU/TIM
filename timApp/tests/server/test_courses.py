from timApp.document.docentry import DocEntry
from timApp.item.tag import TagType, Tag
from timApp.tests.server.timroutetest import TimRouteTest
from timApp.timdb.sqa import db


class CoursesTest(TimRouteTest):
    def test_get_course_settings_doc_not_found(self):
        self.login_test1()
        self.get(f"/courses/settings", expect_content={})

    def test_get_course_settings_find_doc(self):
        u = self.test_user_2
        self.make_admin(u)
        self.login_test2()
        d = self.create_doc(
            path="settings/courses",
            settings={"course_subjects": ["testing subject 1", "testing subject 1"]},
        )
        self.login_test1()
        self.get(
            f"/courses/settings",
            expect_content={
                "course_subjects": ["testing subject 1", "testing subject 1"]
            },
        )

    def test_get_documents_from_bookmark_folder_folder_not_found(self):
        self.login_test1()
        self.get(f"/courses/documents/Test bookmarks", expect_content=[])

    def test_get_documents_from_bookmark_folder_folder_found(self):
        u = self.test_user_2
        self.make_admin(u)
        self.login_test2()
        d = self.create_doc(path="some/path/test")
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

        self.json_post(
            "/bookmarks/add",
            {"group": "Test bookmarks", "name": "test", "link": d.path},
        )

        date_re = r"\d{4}-\d{2}-\d{2}T\d{2}:\d{2}:\d{2}\+\d{2}:\d{2}"
        response_json = str(self.get("/courses/documents/Test bookmarks"))
        self.assertRegex(
            response_json,
            rf"""
\[
    {{
        'name': 'test', 
        'path': '{d.path}', 
        'title': '{d.title}', 
        'location': '{d.location}', 
        'id': {d.id}, 
        'modified': 'just now', 
        'modifiedTimeFull': '{date_re}', 
        'owners': \[{{'id': {self.get_test_user_2_group_id()}, 'name': '{u.name}'}}\], 
        'rights': {{
            'editable': True, 
            'can_mark_as_read': True, 
            'can_comment': True, 
            'copy': True, 
            'browse_own_answers': True, 
            'teacher': True, 
            'see_answers': True, 
            'manage': True, 
            'owner': True
        }}, 
        'unpublished': True, 
        'public': True, 
        'tags': \[
            {{
                'block_id': {d.id}, 
                'name': 'TEST123', 
                'type': {TagType.CourseCode.value}, 
                'expires': None
            }}, 
            {{
                'block_id': {d.id}, 
                'name': 'testing subject', 
                'type': {TagType.Subject.value}, 
                'expires': None
            }}
        \], 
        'isFolder': False
    }}
\]""".replace(
                "\n", ""
            ).replace(
                "    ", ""
            ),
        )

    def test_add_course_route(self):
        self.login_test1()
        d = self.create_doc()
        self.json_post(
            f"/bookmarks/addCourse",
            {"path": d.path, "require_group": True},
            expect_status=400,
            expect_content="Document is not tagged as a course",
        )

        d = DocEntry.find_by_id(d.id)
        d.block.tags.append(Tag(name="test", type=TagType.CourseCode))
        db.session.commit()
        self.json_post(
            f"/bookmarks/addCourse",
            {"path": d.path, "require_group": True},
            expect_status=400,
            expect_content="Course does not allow manual enrollment.",
        )
