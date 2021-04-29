from timApp.admin.user_cli import find_and_merge_users, find_and_soft_delete
from timApp.document.docentry import DocEntry
from timApp.tests.db.timdbtest import TEST_USER_1_ID, TEST_USER_2_ID, TEST_USER_3_ID
from timApp.tests.server.timroutetest import TimRouteTest
from timApp.timdb.sqa import db
from timApp.user.special_group_names import SPECIAL_USERNAMES
from timApp.user.user import User, UserInfo
from timApp.user.usergroup import UserGroup
from timApp.util.flask.requesthelper import RouteException, NotExist


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
        with self.assertRaises(RouteException):
            find_and_merge_users('testuser1', 'testuser1')
        User.create_with_group(UserInfo(username='someguy', full_name='Some Guy', email='some.guy@example.com'))
        db.session.commit()
        with self.assertRaises(RouteException):
            find_and_merge_users('testuser1', 'someguy')
        with self.assertRaises(NotExist):
            find_and_merge_users('testuser1', 'x')
        for u in SPECIAL_USERNAMES:
            with self.assertRaises(RouteException):
                find_and_merge_users('testuser1', u)

        d = self.create_doc(initial_par="#- {plugin=textfield #t}")
        self.post_answer('textfield', f'{d.id}.t', user_input={'c': 'x'})
        path = d.path
        tg1 = UserGroup.create('tg1')
        tg2 = UserGroup.create('tg2')
        self.test_user_1.add_to_group(tg1, added_by=self.test_user_3)
        self.test_user_1.add_to_group(tg2, added_by=None)
        t1pg = self.test_user_1.get_personal_group()
        t2pg = self.test_user_2.get_personal_group()
        db.session.commit()

        def membership_set(user: User):
            return set((m.usergroup_id, m.added_by) for m in user.memberships)

        def check_memberships(
                primary: User,
                secondary: User,
                primary_personal: UserGroup,
                secondary_personal: UserGroup,
        ):
            db.session.refresh(primary)
            db.session.refresh(secondary)
            self.assertEqual(
                {(primary_personal.id, None), (tg1.id, TEST_USER_3_ID), (tg2.id, None)},
                membership_set(primary)
            )
            self.assertEqual(
                {(secondary_personal.id, None)},
                membership_set(secondary)
            )

        r = find_and_merge_users('testuser2', 'testuser1')
        self.assertEqual(2, r.accesses)
        self.assertEqual(0, r.annotations)
        self.assertEqual(1, r.answers)
        self.assertEqual(0, r.lectureanswers)
        self.assertEqual(0, r.messages)
        self.assertEqual(0, r.notes)
        self.assertEqual(0, r.owned_lectures)
        self.assertEqual(0, r.readparagraphs)
        self.assertEqual(0, r.velps)
        self.assertEqual(2, r.groups)
        db.session.commit()
        self.assertIsNone(DocEntry.find_by_path(path))
        check_memberships(self.test_user_2, self.test_user_1, t2pg, t1pg)

        r = find_and_merge_users('testuser2', 'testuser1')
        self.assertEqual(0, r.accesses)
        self.assertEqual(0, r.annotations)
        self.assertEqual(0, r.answers)
        self.assertEqual(0, r.lectureanswers)
        self.assertEqual(0, r.messages)
        self.assertEqual(0, r.notes)
        self.assertEqual(0, r.owned_lectures)
        self.assertEqual(0, r.readparagraphs)
        self.assertEqual(0, r.velps)
        self.assertEqual(0, r.groups)
        db.session.commit()
        check_memberships(self.test_user_2, self.test_user_1, t2pg, t1pg)

        r = find_and_merge_users('testuser1', 'testuser2')
        self.assertEqual(2, r.accesses)
        self.assertEqual(0, r.annotations)
        self.assertEqual(1, r.answers)
        self.assertEqual(0, r.lectureanswers)
        self.assertEqual(0, r.messages)
        self.assertEqual(0, r.notes)
        self.assertEqual(0, r.owned_lectures)
        self.assertEqual(0, r.readparagraphs)
        self.assertEqual(0, r.velps)
        self.assertEqual(2, r.groups)
        db.session.commit()
        check_memberships(self.test_user_1, self.test_user_2, t1pg, t2pg)
        self.assertIsNotNone(DocEntry.find_by_path(path))

        r = find_and_merge_users('testuser1', 'testuser2')
        self.assertEqual(0, r.accesses)
        self.assertEqual(0, r.annotations)
        self.assertEqual(0, r.answers)
        self.assertEqual(0, r.lectureanswers)
        self.assertEqual(0, r.messages)
        self.assertEqual(0, r.notes)
        self.assertEqual(0, r.owned_lectures)
        self.assertEqual(0, r.readparagraphs)
        self.assertEqual(0, r.velps)
        self.assertEqual(0, r.groups)
        db.session.commit()
        check_memberships(self.test_user_1, self.test_user_2, t1pg, t2pg)

        find_and_soft_delete('testuser2')
        self.assertIsNone(User.get_by_name('testuser2'))
        self.assertIsNotNone(User.get_by_name('testuser2_deleted'))
        with self.assertRaises(RouteException):
            find_and_soft_delete('testuser2')
        with self.assertRaises(RouteException):
            find_and_soft_delete('testuser2_deleted')


class UserDeleteTest(TimRouteTest):
    def test_deleted_user_logout(self):
        u, _ = User.create_with_group(UserInfo(username='m@example.com', email='m@example.com', full_name='M'))
        db.session.commit()
        self.get('/')
        self.login(username='m@example.com')
        d = self.create_doc()
        find_and_soft_delete('m@example.com')
        db.session.commit()
        self.get(d.url, expect_status=302, expect_content='')
        self.get(d.url, expect_status=403)
        self.login(username='m@example.com_deleted')
        self.post_answer(
            'x', f'{d.id}.t',
            user_input={},
            expect_status=403,
            expect_content='Please refresh the page and log in again.',
        )
        self.post_answer(
            'x', f'{d.id}.t',
            user_input={},
            expect_status=400,
            expect_content='Task not found in the document: t',
        )
