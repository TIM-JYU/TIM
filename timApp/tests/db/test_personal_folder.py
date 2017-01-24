from tests.server.timroutetest import TimRouteTest
from tim_app import app


class PersonalFolderTest(TimRouteTest):
    def test_personal_folder_name(self):
        with app.test_request_context():
            db = self.get_db()
            user, group = db.users.create_user_with_group('weirdname?', 'Weird Name?', 'weird@example.com')
            user2, group2 = db.users.create_user_with_group('weirdname??', 'Weird Name??', 'weird2@example.com')
            user3, group3 = db.users.create_user_with_group('weird?name', 'Weird ?Name', 'weird3@example.com')
            f1 = user.get_personal_folder()
            self.assertEqual('weird-name', f1.name)
            f2 = user2.get_personal_folder()
            self.assertEqual('weird-name2', f2.name)
            f3 = user3.get_personal_folder()
            self.assertEqual('weird-name3', f3.name)
            self.assertEqual('weird-name3', f3.name)
            self.assertEqual("Weird Name?'s folder", f1.title)
            self.assertEqual("Weird Name??'s folder", f2.title)
            self.assertEqual("Weird ?Name's folder", f3.title)
