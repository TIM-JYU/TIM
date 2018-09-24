from timApp.document.docentry import DocEntry
from timApp.tests.server.timroutetest import TimRouteTest
from timApp.timdb.sqa import db
from timApp.user.special_group_names import SPECIAL_USERNAMES
from timApp.user.user import User


class SearchTest(TimRouteTest):
    def test_user_search(self):
        self.login_test1()
        self.get('/users/search/test', expect_status=403)
        self.make_admin(self.current_user)
        self.get('/users/search/test',
                 expect_content=[{'email': 'test1@example.com',
                                  'id': 4,
                                  'name': 'testuser1',
                                  'real_name': 'Test user 1'},
                                 {'email': 'test2@example.com',
                                  'id': 5,
                                  'name': 'testuser2',
                                  'real_name': 'Test user 2'},
                                 {'email': 'test3@example.com',
                                  'id': 6,
                                  'name': 'testuser3',
                                  'real_name': 'Test user 3'}])


class MergeTest(TimRouteTest):
    def test_user_merge(self):
        self.login_test1()
        self.get('/users/merge/testuser1/testuser2', expect_status=403)
        self.make_admin(self.current_user)
        self.get('/users/merge/testuser1/testuser1',
                 expect_status=400,
                 expect_content='Users cannot be the same',
                 json_key='error')
        User.create_with_group('someguy', 'Some Guy', 'some.guy@example.com')
        db.session.commit()
        self.get('/users/merge/testuser1/someguy',
                 expect_status=400,
                 expect_content=f'Users testuser1 and someguy do not appear to be duplicates. '
                                f'Merging not allowed to prevent accidental errors.',
                 json_key='error')
        self.get('/users/merge/testuser1/x',
                 expect_status=404,
                 expect_content='User x not found',
                 json_key='error')
        for u in SPECIAL_USERNAMES:
            self.get(f'/users/merge/testuser1/{u}',
                     expect_status=400,
                     expect_content=f'User {u} is a special user',
                     json_key='error')

        d = self.create_doc()
        path = d.path
        self.get('/users/merge/testuser2/testuser1',
                 expect_content={
                     'moved': {
                         'accesses': 3,
                         'annotations': 0,
                         'answers': 0,
                         'lectureanswers': 0,
                         'messages': 0,
                         'notes': 0,
                         'owned_lectures': 0,
                         'readparagraphs': 0,
                         'velps': 0,
                     }
                 })
        self.assertIsNone(DocEntry.find_by_path(path))
        self.get('/users/merge/testuser2/testuser1',
                 expect_content={
                     'moved': {
                         'accesses': 0,
                         'annotations': 0,
                         'answers': 0,
                         'lectureanswers': 0,
                         'messages': 0,
                         'notes': 0,
                         'owned_lectures': 0,
                         'readparagraphs': 0,
                         'velps': 0,
                     }
                 })
        self.get('/users/merge/testuser1/testuser2',
                 expect_content={
                     'moved': {
                         'accesses': 3,
                         'annotations': 0,
                         'answers': 0,
                         'lectureanswers': 0,
                         'messages': 0,
                         'notes': 0,
                         'owned_lectures': 0,
                         'readparagraphs': 0,
                         'velps': 0,
                     }
                 })
        self.assertIsNotNone(DocEntry.find_by_path(path))
        self.get('/users/merge/testuser1/testuser2',
                 expect_content={
                     'moved': {
                         'accesses': 0,
                         'annotations': 0,
                         'answers': 0,
                         'lectureanswers': 0,
                         'messages': 0,
                         'notes': 0,
                         'owned_lectures': 0,
                         'readparagraphs': 0,
                         'velps': 0,
                     }
                 })
