from timApp.tests.server.timroutetest import TimRouteTest
from timApp.timdb.sqa import db
from timApp.user.user import User, UserInfo
from timApp.user.usergroup import UserGroup


class QuickLoginTest(TimRouteTest):
    def test_quicklogin(self):
        self.login_test1()
        self.get(f"/quickLogin/testuser2", expect_status=403)

    def test_quicklogin_model_answer(self):
        self.login_test1()
        m_user, _ = User.create_with_group(UserInfo(username="mallivastaus"))
        db.session.commit()
        self.get(f"/quickLogin/{m_user.name}", expect_status=403)
        t = UserGroup.get_teachers_group()
        self.test_user_1.add_to_group(t, added_by=None)
        db.session.commit()
        self.get(f"/quickLogin/{m_user.name}", expect_status=302)
        self.login_test1()
        self.get(f"/quickLogin/testuser2", expect_status=403)
