from sqlalchemy import select, delete, update

from timApp.auth.accesstype import AccessType
from timApp.auth.session.model import UserSession
from timApp.auth.session.util import verify_session_for
from timApp.tests.server.timroutetest import TimRouteTest
from timApp.timdb.sqa import db, run_sql


class UserSessionsTest(TimRouteTest):
    def forget_session(self):
        """Forget the current session. Simulates user e.g. closing an incognito window."""
        with self.client.session_transaction() as s:
            s.clear()

    def latest_session(self) -> UserSession:
        """Get latest session of Test User 1."""
        return (
            run_sql(
                select(UserSession)
                .filter_by(user_id=self.test_user_1.id)
                .order_by(UserSession.logged_in_at.desc())
                .limit(1)
            )
            .scalars()
            .first()
        )

    def assert_sesion_expired_state(
        self, session_ids: list[str], state: list[bool], msg: str
    ) -> None:
        """Assert the state of Test User 1's sessions."""
        self.assertEqual(
            [
                run_sql(
                    select(UserSession).filter_by(
                        user_id=self.test_user_1.id,
                        session_id=sess,
                    )
                )
                .scalars()
                .one()
                .expired
                for sess in session_ids
            ],
            state,
            msg,
        )

    def test_session_basic(self):
        """Test that sessions are tracked when SESSIONS_ENABLE is True."""
        self.logout()
        with self.temp_config(
            {
                "SESSIONS_ENABLE": True,
                "SESSIONS_MAX_CONCURRENT_SESSIONS_PER_USER": None,
            }
        ):
            run_sql(delete(UserSession))
            db.session.commit()
            self.login_test1(manual=True)
            sessions: list[UserSession] = run_sql(select(UserSession)).scalars().all()
            self.assertEqual(len(sessions), 1)
            self.assertEqual(sessions[0].user.name, self.test_user_1.name)
            self.assertEqual(sessions[0].expired, False)

            self.get(
                "/user/sessions/current",
                expect_content={
                    "sessionId": sessions[0].session_id,
                    "valid": True,
                },
            )

            self.logout()
            sessions: list[UserSession] = run_sql(select(UserSession)).scalars().all()
            self.assertEqual(len(sessions), 1)
            self.assertEqual(sessions[0].user.name, self.test_user_1.name)
            self.assertEqual(sessions[0].expired, True)

    def test_session_access_block(self):
        """Test that expired sessions cannot access documents."""
        self.login_test2()
        d = self.create_doc()
        self.test_user_1.grant_access(d, AccessType.view)
        db.session.commit()
        self.logout()

        with self.temp_config(
            {
                "SESSIONS_ENABLE": True,
                "SESSIONS_MAX_CONCURRENT_SESSIONS_PER_USER": 1,
            }
        ):
            run_sql(delete(UserSession))
            db.session.commit()

            self.login_test1(manual=True)
            self.get(f"/view/{d.id}", expect_status=200)
            self.forget_session()
            self.login_test1(manual=True)
            self.get(f"/view/{d.id}", expect_status=490)

    def test_session_validity(self):
        """Test cases where the session can invalidate itself."""
        self.logout()
        with self.temp_config(
            {
                "SESSIONS_ENABLE": True,
                "SESSIONS_MAX_CONCURRENT_SESSIONS_PER_USER": 1,
            }
        ):
            run_sql(delete(UserSession))
            db.session.commit()

            self.login_test1(manual=True)

            lsess = self.latest_session()
            self.get(
                "/user/sessions/current",
                expect_content={
                    "sessionId": lsess.session_id,
                    "valid": True,
                },
            )

            self.logout()
            lsess = self.latest_session()
            self.assertEqual(
                lsess.expired,
                True,
                "Session should be expired after logging out",
            )
            self.get("/user/sessions/current", expect_status=403)

            self.login_test1(manual=True)
            lsess = self.latest_session()
            prev_id = lsess.session_id
            self.assertEqual(lsess.expired, False)
            self.get(
                "/user/sessions/current",
                expect_content={
                    "sessionId": lsess.session_id,
                    "valid": True,
                },
            )

            self.forget_session()
            self.login_test1(manual=True)

            self.assertEqual(
                lsess.expired, False, "The previous session should still be valid"
            )

            lsess = self.latest_session()
            self.assertEqual(
                lsess.expired,
                True,
                "The new session should be automatically expired (max concurrent sessions reached)",
            )
            self.get(
                "/user/sessions/current",
                expect_content={
                    "sessionId": lsess.session_id,
                    "valid": False,
                },
            )

            verify_session_for(self.test_user_1.name, lsess.session_id)
            db.session.commit()
            # Session is valid after verifying it
            self.get(
                "/user/sessions/current",
                expect_content={
                    "sessionId": lsess.session_id,
                    "valid": True,
                },
            )
            prev_session = (
                run_sql(
                    select(UserSession)
                    .filter_by(user_id=self.test_user_1.id, session_id=prev_id)
                    .limit(1)
                )
                .scalars()
                .first()
            )
            self.assertEqual(
                prev_session.expired,
                True,
                "The previously unexpired session should now be expired",
            )

    def test_session_verify_remote(self):
        """Test that a session can be verified remotely."""
        self.logout()
        with self.temp_config(
            {
                "SESSIONS_ENABLE": True,
                "SESSIONS_MAX_CONCURRENT_SESSIONS_PER_USER": 1,
                "DIST_RIGHTS_SEND_SECRET": "yyy",
                "DIST_RIGHTS_RECEIVE_SECRET": "yyy",
            }
        ):
            run_sql(delete(UserSession))
            db.session.commit()

            session_ids = []

            self.login_test1(manual=True)
            session_ids.append(self.latest_session().session_id)
            self.forget_session()
            self.login_test1(manual=True)
            session_ids.append(self.latest_session().session_id)
            self.forget_session()
            self.login_test1(manual=True)
            session_ids.append(self.latest_session().session_id)
            self.forget_session()

            self.post(
                "/user/sessions/verify",
                data={
                    "session_id": session_ids[0],
                    "username": self.test_user_1.name,
                },
                expect_status=403,
            )

            self.post(
                "/user/sessions/verify",
                data={
                    "session_id": session_ids[0],
                    "username": self.test_user_1.name,
                    "secret": "xxx",
                },
                expect_status=400,
            )

            self.post(
                "/user/sessions/verify",
                data={
                    "session_id": "invalid session id",
                    "username": self.test_user_1.name,
                    "secret": "yyy",
                },
                expect_status=200,
            )

            self.post(
                "/user/sessions/verify",
                data={
                    "session_id": session_ids[0],
                    "username": self.test_user_1.name,
                    "secret": "yyy",
                },
            )

            self.assert_sesion_expired_state(
                session_ids,
                [False, True, True],
                "Specific session should be valid, others expired",
            )

            # Mark all sessions as not expired to test verification of the latest session
            run_sql(
                update(UserSession)
                .where(UserSession.user_id == self.test_user_1.id)
                .values({"expired_at": None})
            )
            db.session.commit()

            self.post(
                "/user/sessions/verify",
                data={
                    "username": self.test_user_1.name,
                    "secret": "yyy",
                },
            )

            self.assert_sesion_expired_state(
                session_ids,
                [True, True, False],
                "Latest session should be valid, others expired",
            )

    def test_session_invalidate(self) -> None:
        self.logout()
        with self.temp_config(
            {
                "SESSIONS_ENABLE": True,
                "SESSIONS_MAX_CONCURRENT_SESSIONS_PER_USER": 1,
                "DIST_RIGHTS_SEND_SECRET": "yyy",
                "DIST_RIGHTS_RECEIVE_SECRET": "yyy",
            }
        ):
            run_sql(delete(UserSession))
            db.session.commit()

            session_ids = []

            self.login_test1(manual=True)
            session_ids.append(self.latest_session().session_id)
            self.forget_session()
            self.login_test1(manual=True)
            session_ids.append(self.latest_session().session_id)
            self.forget_session()
            self.login_test1(manual=True)
            session_ids.append(self.latest_session().session_id)
            self.forget_session()

            self.assert_sesion_expired_state(
                session_ids,
                [False, True, True],
                "Oldest session should be valid, others expired",
            )

            self.post(
                "/user/sessions/invalidate",
                data={
                    "username": self.test_user_1.name,
                    "session_id": session_ids[0],
                },
                expect_status=403,
            )

            self.post(
                "/user/sessions/invalidate",
                data={
                    "username": self.test_user_1.name,
                    "session_id": session_ids[1],
                    "secret": "xxx",
                },
                expect_status=400,
            )

            self.post(
                "/user/sessions/invalidate",
                data={
                    "username": self.test_user_1.name,
                    "session_id": "invalid session id",
                    "secret": "yyy",
                },
                expect_status=200,
            )

            self.assert_sesion_expired_state(
                session_ids,
                [False, True, True],
                "Expiration state should not have changed after wrong invalidation attempts",
            )

            self.post(
                "/user/sessions/invalidate",
                data={
                    "username": self.test_user_1.name,
                    "session_id": session_ids[0],
                    "secret": "yyy",
                },
            )

            self.assert_sesion_expired_state(
                session_ids,
                [True, True, True],
                "Oldest session should be invalidated",
            )

            for session_id in session_ids:
                sess = (
                    run_sql(
                        select(UserSession).filter_by(session_id=session_id).limit(1)
                    )
                    .scalars()
                    .first()
                )
                sess.expired_at = None

            db.session.commit()

            self.assert_sesion_expired_state(
                session_ids,
                [False, False, False],
                "All sessions should be valid after manual validation",
            )

            self.post(
                "/user/sessions/invalidate",
                data={
                    "username": self.test_user_1.name,
                    "secret": "yyy",
                },
            )

            self.assert_sesion_expired_state(
                session_ids,
                [True, True, True],
                "All sessions should be expired after invalidation",
            )
