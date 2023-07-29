from datetime import timedelta

from sqlalchemy import select

from timApp.auth.accesstype import AccessType
from timApp.auth.auth_models import BlockAccess
from timApp.document.docentry import DocEntry
from timApp.tests.server.timroutetest import TimRouteTest
from timApp.timdb.sqa import db, run_sql
from timApp.user.usergroup import UserGroup
from timApp.user.userutils import get_access_type_id
from timApp.user.userutils import grant_access
from timApp.util.flask.filters import humanize_datetime
from timApp.util.utils import get_current_time


class DurationTest(TimRouteTest):
    def get_about_to_access_msg(self, period="a day"):
        return [
            "You are about to access a time-limited document.",
            f"After you click Unlock, your access to this document will be removed in under {period}.",
        ]

    unlock_success = "Item was unlocked successfully."

    def test_duration_unlock(self):
        self.login_test1()
        d = self.create_doc()
        doc_id = d.id
        self.login_test2()
        self.test_user_2.grant_access(d, AccessType.view, duration=timedelta(days=1))
        db.session.commit()
        d = DocEntry.find_by_id(doc_id)
        self.get(
            d.url_relative,
            expect_status=403,
            expect_contains=self.get_about_to_access_msg(),
        )

        self.get(
            d.url_relative,
            query_string={"unlock": "true"},
            expect_contains=self.unlock_success,
        )
        self.get(d.url_relative)
        ba = (
            run_sql(
                select(BlockAccess).filter_by(
                    usergroup_id=self.get_test_user_2_group_id(),
                    block_id=doc_id,
                    type=get_access_type_id("view"),
                )
            )
            .scalars()
            .one()
        )
        ba.accessible_to -= timedelta(days=2)
        db.session.commit()
        self.get(
            d.url_relative,
            expect_status=403,
            expect_contains=self.get_expired_msg(ba.accessible_to),
        )

    def test_duration_value_access_to_clamp(self):
        self.login_test1()
        d = self.create_doc()
        self.login_test2()
        delta_access = timedelta(minutes=5)
        unlock_time = get_current_time()
        access_to_max = unlock_time + delta_access
        self.test_user_2.grant_access(
            d,
            AccessType.view,
            duration=timedelta(minutes=10),
            duration_from=unlock_time,
            accessible_to=access_to_max,
        )
        db.session.commit()
        self.get(d.url_relative, query_string={"unlock": "true"})
        ba = (
            run_sql(
                select(BlockAccess).filter_by(
                    usergroup_id=self.get_test_user_2_group_id(),
                    block_id=d.id,
                    type=AccessType.view.value,
                )
            )
            .scalars()
            .one()
        )
        real_duration = (ba.accessible_to - get_current_time()).total_seconds()
        delta_access_seconds = delta_access.total_seconds()
        self.assertAlmostEqual(real_duration, delta_access_seconds, delta=1)

    def test_unlock_needs_confirm(self):
        self.login_test1()
        d = self.create_doc()
        doc_id = d.id
        self.login_test2()
        self.test_user_2.grant_access(
            d, AccessType.view, duration=timedelta(days=1), require_confirm=True
        )
        db.session.commit()
        d = DocEntry.find_by_id(doc_id)
        err_msg_confirm = (
            f"You can unlock this item only after your permission is confirmed."
        )
        self.get(d.url_relative, expect_status=403, expect_contains=[err_msg_confirm])
        self.get(
            d.url_relative,
            query_string={"unlock": "true"},
            expect_status=403,
            expect_contains=[err_msg_confirm],
        )

    def test_timed_duration_unlock(self):
        self.login_test1()
        d = self.create_doc()
        doc_id = d.id
        self.login_test2()
        delta = timedelta(minutes=3)
        now_plus_minutes = get_current_time() + delta
        self.test_user_2.grant_access(
            d,
            AccessType.view,
            duration=timedelta(days=1),
            duration_from=now_plus_minutes,
        )
        db.session.commit()
        d = DocEntry.find_by_id(doc_id)
        now_minus_minutes = get_current_time() - delta
        err_msg_too_early = (
            f"You can unlock this item in {humanize_datetime(now_plus_minutes)}."
        )
        err_msg_too_late = f"You cannot unlock this item anymore (deadline expired {humanize_datetime(now_minus_minutes)})."
        self.get(d.url_relative, expect_status=403, expect_contains=[err_msg_too_early])
        self.get(
            d.url_relative,
            query_string={"unlock": "true"},
            expect_status=403,
            expect_contains=[err_msg_too_early],
        )

        self.test_user_2.grant_access(
            d,
            AccessType.view,
            duration=timedelta(days=1),
            duration_to=now_minus_minutes,
        )
        db.session.commit()
        self.get(d.url_relative, expect_status=403, expect_contains=[err_msg_too_late])
        self.get(
            d.url_relative,
            query_string={"unlock": "true"},
            expect_status=403,
            expect_contains=[err_msg_too_late],
        )

        self.test_user_2.grant_access(
            d,
            AccessType.view,
            duration=timedelta(days=1),
            duration_from=now_minus_minutes,
        )
        db.session.commit()
        self.get(
            d.url_relative,
            expect_status=403,
            expect_contains=self.get_about_to_access_msg(),
        )
        self.get(
            d.url_relative,
            query_string={"unlock": "true"},
            expect_status=200,
            expect_contains=self.unlock_success,
        )
        self.get(d.url_relative)
        ba = (
            run_sql(
                select(BlockAccess).filter_by(
                    usergroup_id=self.get_test_user_2_group_id(),
                    block_id=doc_id,
                    type=get_access_type_id("view"),
                )
            )
            .scalars()
            .one()
        )
        ba.accessible_to -= timedelta(days=2)
        db.session.commit()
        self.get(
            d.url_relative,
            expect_status=403,
            expect_contains=self.get_expired_msg(ba.accessible_to),
        )

    def get_expired_msg(self, access):
        return f"Your access to this item has expired {humanize_datetime(access)}."

    def test_duration_group_unlock(self):
        self.login_test1()
        d = self.create_doc()
        doc_id = d.id
        self.login_test2()
        duration = timedelta(days=1)
        now = get_current_time()
        access = grant_access(
            UserGroup.get_logged_in_group(),
            d,
            AccessType.view,
            duration=duration,
            duration_to=now + duration,
        )
        db.session.commit()
        accesses = (
            self.current_group().accesses.filter_by(type=AccessType.view.value).all()
        )
        self.assertEqual(0, len(accesses))
        d = DocEntry.find_by_id(doc_id)
        self.get(
            d.url_relative,
            expect_status=403,
            expect_contains=self.get_about_to_access_msg(),
        )
        accesses = (
            self.current_group().accesses.filter_by(type=AccessType.view.value).all()
        )
        self.assertEqual(0, len(accesses))
        self.get(
            d.url_relative,
            query_string={"unlock": "true"},
            expect_contains=self.unlock_success,
        )
        accesses = (
            self.current_group().accesses.filter_by(type=AccessType.view.value).all()
        )
        self.assertEqual(1, len(accesses))
        granted_access = accesses[0]
        self.assertEqual(access.duration, granted_access.duration)
        self.assertEqual(access.block_id, granted_access.block_id)
        self.assertEqual(access.type, granted_access.type)
        self.assertEqual(access.duration_from, granted_access.duration_from)
        self.assertEqual(access.duration_to, granted_access.duration_to)
        self.assertEqual(self.current_group().id, granted_access.usergroup_id)
        self.assertEqual(
            duration, granted_access.accessible_to - granted_access.accessible_from
        )
        granted_access.accessible_to -= duration
        db.session.commit()

        self.get(
            d.url_relative,
            expect_status=403,
            expect_contains=self.get_expired_msg(granted_access.accessible_to),
        )
        self.get(
            d.url_relative,
            expect_status=403,
            query_string={"unlock": "true"},
            expect_contains=self.get_expired_msg(granted_access.accessible_to),
        )
        db.session.expunge_all()
        db.session.delete(granted_access)
        db.session.commit()
        self.get(
            d.url_relative,
            expect_status=403,
            expect_contains=self.get_about_to_access_msg(),
        )

    def test_duration_zero(self):
        self.login_test1()
        d = self.create_doc()
        self.login_test2()
        self.test_user_2.grant_access(d, AccessType.view, duration=timedelta(days=0))
        db.session.commit()
        self.get(
            d.url_relative,
            expect_status=403,
            expect_contains=self.get_about_to_access_msg("a moment"),
        )
        self.get(
            d.url_relative,
            query_string={"unlock": "true"},
            expect_status=403,
            expect_contains=[
                self.unlock_success,
                self.get_expired_msg(get_current_time()),
            ],
        )

    def test_access_future_and_expired(self):
        self.login_test1()
        d = self.create_doc()
        self.login_test3()
        self.test_user_3.grant_access(
            d, AccessType.view, accessible_from=get_current_time() + timedelta(days=1)
        )
        db.session.commit()
        r = self.get(
            d.url_relative,
            expect_status=403,
            as_tree=True,
        )
        cntdwn = r.cssselect("tim-access-countdown")[0]
        waittime = float(cntdwn.attrib["bind-wait-time"])
        diff = 24 * 3600 - waittime
        self.assertLess(0, diff)
        self.assertGreater(5, diff)
        self.test_user_3.grant_access(
            d, AccessType.view, accessible_to=get_current_time() - timedelta(days=1)
        )
        db.session.commit()
        self.get(
            d.url_relative,
            expect_status=403,
            expect_contains="Your access to this item has expired a day ago.",
        )
