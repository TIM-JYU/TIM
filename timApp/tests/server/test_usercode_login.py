from datetime import timedelta

from flask import session

from timApp.auth.logincodes.model import UserLoginCode
from timApp.tests.server.timroutetest import TimRouteTest
from timApp.timdb.sqa import db
from timApp.util.utils import get_current_time


class TestUserCodeLogin(TimRouteTest):
    def test_usercode_login(self):
        with self.temp_config({"LOGIN_CODES_ENABLED": True}):
            u = UserLoginCode.generate_new(
                user=self.test_user_1,
                name="test_code",
                active_to=get_current_time() + timedelta(days=1),
            )
            user_code = u.code
            session_code = u.session_code
            db.session.commit()

            self.json_post(
                "/loginCode/login",
                {"login_code": "wrong"},
                expect_status=403,
                expect_content={"error": "InvalidLoginCode"},
            )

            # Correct code, but not activated
            self.json_post(
                "/loginCode/login",
                {"login_code": user_code},
                expect_status=403,
                expect_content={"error": "LoginCodeNotYetActive"},
            )

            u = db.session.get(UserLoginCode, user_code)
            u.active_from = get_current_time() - timedelta(days=2)
            u.active_to = get_current_time() - timedelta(days=1)
            db.session.commit()

            # Correct code, but expired
            self.json_post(
                "/loginCode/login",
                {"login_code": user_code},
                expect_status=403,
                expect_content={"error": "LoginCodeExpired"},
            )

            u = db.session.get(UserLoginCode, user_code)
            u.active_from = get_current_time() - timedelta(days=1)
            u.active_to = get_current_time() + timedelta(days=1)
            db.session.commit()

            # Correct code, activated
            self.json_post("/loginCode/login", {"login_code": user_code})

            # Check if the login code session is set
            self.assertTrue("login_code_session" in session)
            self.assertEqual(session["login_code_session"], session_code)

            # Check if the user is logged in
            self.assertEqual(self.current_user_id(), self.test_user_1.id)

            # Invalidate the login code by setting the active_to to a date in the past
            u = db.session.get(UserLoginCode, user_code)
            u.active_to = get_current_time() - timedelta(days=1)
            db.session.commit()

            # Doing a request should clear the session
            self.get("/")
            self.assertTrue("login_code_session" not in session)
            self.assertEqual(self.current_user_id(), None)
