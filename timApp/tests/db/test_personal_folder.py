from timApp.folder.folder import Folder
from timApp.tests.server.timroutetest import TimRouteTest
from timApp.tim_app import app
from timApp.timdb.sqa import db
from timApp.user.user import User


class PersonalFolderTest(TimRouteTest):

    def test_personal_folder_name(self):
        with app.test_request_context():
            user, group = User.create_with_group('weirdname?', 'Weird Name?', 'weird@example.com')
            user2, group2 = User.create_with_group('weirdname??', 'Weird Name??', 'weird2@example.com')
            user3, group3 = User.create_with_group('weird?name', 'Weird ?Name', 'weird3@example.com')
            db.session.flush()
            f1 = user.get_personal_folder()
            self.assertEqual('weird-name', f1.name)
            f2 = user2.get_personal_folder()
            self.assertEqual('weird-name2', f2.name)
            f3 = user3.get_personal_folder()
            self.assertEqual('weird-name3', f3.name)
            self.assertEqual('weird-name3', f3.name)
            self.assertEqual("Weird Name?", f1.title)
            self.assertEqual("Weird Name??", f2.title)
            self.assertEqual("Weird ?Name", f3.title)

    def test_anon_personal_folder(self):
        """Make sure personal folders aren't created for each anonymous request."""
        self.logout()
        self.get('/')
        folders = Folder.query.filter_by(location='users').all()
        self.get('/')
        folders_after = Folder.query.filter_by(location='users').all()
        self.assertEqual(len(folders), len(folders_after))

    def test_no_multiple_personal_folders(self):
        self.login_test3()
        self.test_user_3.make_admin()
        db.session.commit()
        for t in ('document', 'folder'):
            self.json_post('/createItem', {
                'item_path': 'users/testing',
                'item_type': t,
                'item_title': 'document '}, expect_status=403
                           )
