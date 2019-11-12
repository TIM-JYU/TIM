from werkzeug.exceptions import BadRequest, NotFound

from timApp.admin.routes import do_merge, do_soft_delete
from timApp.document.docentry import DocEntry
from timApp.tests.db.timdbtest import TEST_USER_1_ID, TEST_USER_2_ID, TEST_USER_3_ID
from timApp.tests.server.timroutetest import TimRouteTest
from timApp.timdb.sqa import db
from timApp.user.special_group_names import SPECIAL_USERNAMES
from timApp.user.user import User, UserInfo


class SearchTest(TimRouteTest):
    def test_user_search(self):
        self.login_test1()
        self.get('/users/search/test', expect_status=403)
        self.make_admin(self.current_user)
        self.get('/users/search/test',
                 expect_content=[{'email': 'test1@example.com',
                                  'id': TEST_USER_1_ID,
                                  'name': 'testuser1',
                                  'real_name': 'Test user 1'},
                                 {'email': 'test2@example.com',
                                  'id': TEST_USER_2_ID,
                                  'name': 'testuser2',
                                  'real_name': 'Test user 2'},
                                 {'email': 'test3@example.com',
                                  'id': TEST_USER_3_ID,
                                  'name': 'testuser3',
                                  'real_name': 'Test user 3'}])


class MergeTest(TimRouteTest):
    def test_user_merge(self):
        self.login_test1()
        with self.assertRaises(BadRequest):
            do_merge('testuser1', 'testuser1')
        User.create_with_group(UserInfo(username='someguy', full_name='Some Guy', email='some.guy@example.com'))
        db.session.commit()
        with self.assertRaises(BadRequest):
            do_merge('testuser1', 'someguy')
        with self.assertRaises(NotFound):
            do_merge('testuser1', 'x')
        for u in SPECIAL_USERNAMES:
            with self.assertRaises(BadRequest):
                do_merge('testuser1', u)

        d = self.create_doc()
        path = d.path
        r = do_merge('testuser2', 'testuser1')
        self.assertEqual({
            'accesses': 3,
            'annotations': 0,
            'answers': 0,
            'lectureanswers': 0,
            'messages': 0,
            'notes': 0,
            'owned_lectures': 0,
            'readparagraphs': 0,
            'velps': 0,
        }, r)
        db.session.commit()
        self.assertIsNone(DocEntry.find_by_path(path))
        #db.session.refresh(self.test_user_1.get_personal_group())
        #db.session.refresh(self.test_user_2.get_personal_group())
        r = do_merge('testuser2', 'testuser1')
        self.assertEqual({
            'accesses': 0,
            'annotations': 0,
            'answers': 0,
            'lectureanswers': 0,
            'messages': 0,
            'notes': 0,
            'owned_lectures': 0,
            'readparagraphs': 0,
            'velps': 0,
        }, r)
        db.session.commit()
        db.session.refresh(self.test_user_1.get_personal_group())
        db.session.refresh(self.test_user_2.get_personal_group())
        r = do_merge('testuser1', 'testuser2')
        self.assertEqual({
            'accesses': 3,
            'annotations': 0,
            'answers': 0,
            'lectureanswers': 0,
            'messages': 0,
            'notes': 0,
            'owned_lectures': 0,
            'readparagraphs': 0,
            'velps': 0,
        }, r)
        db.session.commit()
        self.assertIsNotNone(DocEntry.find_by_path(path))
        r = do_merge('testuser1', 'testuser2')
        self.assertEqual({
            'accesses': 0,
            'annotations': 0,
            'answers': 0,
            'lectureanswers': 0,
            'messages': 0,
            'notes': 0,
            'owned_lectures': 0,
            'readparagraphs': 0,
            'velps': 0,
        }, r)
        db.session.commit()

        do_soft_delete('testuser2')
        self.assertIsNone(User.get_by_name('testuser2'))
        self.assertIsNotNone(User.get_by_name('testuser2_deleted'))
        with self.assertRaises(NotFound):
            do_soft_delete('testuser2')
        with self.assertRaises(BadRequest):
            do_soft_delete('testuser2_deleted')
