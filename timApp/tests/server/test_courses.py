from timApp.item.tag import TagType
from timApp.tests.server.timroutetest import TimRouteTest
from timApp.user.usergroup import UserGroup
from timApp.timdb.sqa import db


class CoursesTest(TimRouteTest):

    def test_get_course_settings_doc_not_found(self):
        self.login_test1()
        self.get(f'/courses/settings', expect_content={})

    def test_get_course_settings_find_doc(self):
        u = self.test_user_2
        self.make_admin(u)
        self.login_test2()
        d = self.create_doc(path="settings/courses",
                            settings={'course_subjects': ['testing subject 1', 'testing subject 1']})
        self.login_test1()
        self.get(f'/courses/settings',
                 expect_content={'course_subjects': ['testing subject 1', 'testing subject 1']})

    def test_get_documents_from_bookmark_folder_folder_not_found(self):
        self.login_test1()
        self.get(f'/courses/documents/Test bookmarks', expect_content=[])

    def test_get_documents_from_bookmark_folder_folder_found(self):
        u = self.test_user_2
        self.make_admin(u)
        self.login_test2()
        d = self.create_doc(path="some/path/test")
        self.json_post(f'/tags/add/{d.path}',
                       {'tags': [{'name': 'TEST123', 'expires': None, 'type': TagType.CourseCode},
                                 {'name': 'testing subject', 'expires': None, 'type': TagType.Subject}]})

        self.json_post('/bookmarks/add', {'group': 'Test bookmarks', 'name': "test", 'link': d.path})
        self.get(f'/courses/documents/Test bookmarks', expect_content=[{'id': d.id,
                                                                        'isFolder': False,
                                                                        'location': d.location,
                                                                        'modified': 'just now',
                                                                        'name': "test",
                                                                        'owner': {'id': d.id, 'name': u.name},
                                                                        'path': d.path,
                                                                        'public': True,
                                                                        'rights': {'browse_own_answers': True,
                                                                                   'can_comment': True,
                                                                                   'can_mark_as_read': True,
                                                                                   'editable': True,
                                                                                   'manage': True,
                                                                                   'owner': True,
                                                                                   'see_answers': True,
                                                                                   'teacher': True},
                                                                        'tags': [{'block_id': d.id, 'expires': None,
                                                                                  'name': 'TEST123',
                                                                                  'type': TagType.CourseCode.value},
                                                                                 {'block_id': d.id,
                                                                                  'expires': None,
                                                                                  'name': 'testing subject',
                                                                                  'type': TagType.Subject.value}],
                                                                        'title': d.title,
                                                                        'unpublished': True}])
