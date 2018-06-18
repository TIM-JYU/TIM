from datetime import timedelta, datetime, timezone

from timApp.util.flask.filters import humanize_datetime
from timApp.tests.server.timroutetest import TimRouteTest
from timApp.auth.accesstype import AccessType
from timApp.document.docentry import DocEntry
from timApp.user.usergroup import UserGroup
from timApp.timdb.sqa import db
from timApp.auth.auth_models import BlockAccess
from timApp.user.userutils import get_access_type_id
from timApp.user.userutils import grant_access


class DurationTest(TimRouteTest):

    def get_about_to_access_msg(self, period='a day'):
        return ['You are about to access a time-limited document.',
                f'After you click Unlock, your access to this document will be removed in {period}.']

    unlock_success = 'Item was unlocked successfully.'

    def test_duration_unlock(self):
        self.login_test1()
        d = self.create_doc()
        doc_id = d.id
        self.login_test2()
        grant_access(self.get_test_user_2_group_id(), doc_id, 'view', duration=timedelta(days=1))
        d = DocEntry.find_by_id(doc_id)
        self.get(d.url_relative,
                 expect_status=403,
                 expect_contains=self.get_about_to_access_msg())

        self.get(d.url_relative, query_string={'unlock': 'true'},
                 expect_contains=self.unlock_success)
        self.get(d.url_relative)
        ba = BlockAccess.query.filter_by(usergroup_id=self.get_test_user_2_group_id(), block_id=doc_id,
                                         type=get_access_type_id('view')).one()
        ba.accessible_to -= timedelta(days=2)
        db.session.commit()
        self.get(d.url_relative, expect_status=403,
                 expect_contains=self.get_expired_msg(ba.accessible_to))

    def test_timed_duration_unlock(self):
        self.login_test1()
        d = self.create_doc()
        doc_id = d.id
        self.login_test2()
        delta = timedelta(minutes=3)
        now_plus_minutes = datetime.now(tz=timezone.utc) + delta
        grant_access(self.get_test_user_2_group_id(), doc_id, 'view',
                     duration=timedelta(days=1),
                     duration_from=now_plus_minutes)
        d = DocEntry.find_by_id(doc_id)
        now_minus_minutes = datetime.now(tz=timezone.utc) - delta
        err_msg_too_early = f'You can unlock this item in {humanize_datetime(now_plus_minutes)}.'
        err_msg_too_late = f'You cannot unlock this item anymore (deadline expired {humanize_datetime(now_minus_minutes)}).'
        self.get(d.url_relative,
                 expect_status=403,
                 json_key='error',
                 expect_contains=[err_msg_too_early])
        self.get(d.url_relative, query_string={'unlock': 'true'},
                 expect_status=403,
                 json_key='error',
                 expect_contains=[err_msg_too_early])

        grant_access(self.get_test_user_2_group_id(), doc_id, 'view',
                     duration=timedelta(days=1),
                     duration_to=now_minus_minutes)
        self.get(d.url_relative,
                 expect_status=403,
                 json_key='error',
                 expect_contains=[err_msg_too_late])
        self.get(d.url_relative, query_string={'unlock': 'true'},
                 expect_status=403,
                 json_key='error',
                 expect_contains=[err_msg_too_late])

        grant_access(self.get_test_user_2_group_id(), doc_id, 'view',
                     duration=timedelta(days=1),
                     duration_from=now_minus_minutes)
        self.get(d.url_relative,
                 expect_status=403,
                 expect_contains=self.get_about_to_access_msg())
        self.get(d.url_relative, query_string={'unlock': 'true'},
                 expect_status=200,
                 expect_contains=self.unlock_success)
        self.get(d.url_relative)
        ba = BlockAccess.query.filter_by(usergroup_id=self.get_test_user_2_group_id(), block_id=doc_id,
                                         type=get_access_type_id('view')).one()
        ba.accessible_to -= timedelta(days=2)
        db.session.commit()
        self.get(d.url_relative, expect_status=403,
                 expect_contains=self.get_expired_msg(ba.accessible_to))

    def get_expired_msg(self, access):
        return f'Your access to this item has expired {humanize_datetime(access)}.'

    def test_duration_group_unlock(self):
        self.login_test1()
        d = self.create_doc()
        doc_id = d.id
        self.login_test2()
        duration = timedelta(days=1)
        now = datetime.now(tz=timezone.utc)
        access = grant_access(UserGroup.get_logged_in_group().id, doc_id, 'view',
                              duration=duration,
                              duration_to=now + duration)
        accesses = self.current_group().accesses.filter_by(type=AccessType.view.value).all()
        self.assertEqual(0, len(accesses))
        d = DocEntry.find_by_id(doc_id)
        self.get(d.url_relative,
                 expect_status=403,
                 expect_contains=self.get_about_to_access_msg())
        accesses = self.current_group().accesses.filter_by(type=AccessType.view.value).all()
        self.assertEqual(0, len(accesses))
        self.get(d.url_relative, query_string={'unlock': 'true'},
                 expect_contains=self.unlock_success)
        accesses = self.current_group().accesses.filter_by(type=AccessType.view.value).all()
        self.assertEqual(1, len(accesses))
        granted_access = accesses[0]
        self.assertEqual(access.duration, granted_access.duration)
        self.assertEqual(access.block_id, granted_access.block_id)
        self.assertEqual(access.type, granted_access.type)
        self.assertEqual(access.duration_from, granted_access.duration_from)
        self.assertEqual(access.duration_to, granted_access.duration_to)
        self.assertEqual(self.current_group().id, granted_access.usergroup_id)
        self.assertEqual(duration, granted_access.accessible_to - granted_access.accessible_from)
        granted_access.accessible_to -= duration
        db.session.commit()

        self.get(d.url_relative,
                 expect_status=403,
                 expect_contains=self.get_expired_msg(granted_access.accessible_to))
        self.get(d.url_relative,
                 expect_status=403,
                 query_string={'unlock': 'true'},
                 expect_contains=self.get_expired_msg(granted_access.accessible_to))
        db.session.expunge_all()
        db.session.delete(granted_access)
        db.session.commit()
        self.get(d.url_relative,
                 expect_status=403,
                 expect_contains=self.get_about_to_access_msg())

    def test_duration_zero(self):
        self.login_test1()
        d = self.create_doc()
        doc_id = d.id
        self.login_test2()
        grant_access(self.get_test_user_2_group_id(), doc_id, 'view',
                     duration=timedelta(days=0))
        self.get(d.url_relative,
                 expect_status=403,
                 expect_contains=self.get_about_to_access_msg('a moment'))
        self.get(d.url_relative,
                 query_string={'unlock': 'true'},
                 expect_status=403,
                 expect_contains=[self.unlock_success, self.get_expired_msg(datetime.now(tz=timezone.utc))])

    def test_access_future_and_expired(self):
        self.login_test1()
        d = self.create_doc()
        doc_id = d.id
        self.login_test3()
        self.test_user_3.grant_access(doc_id, 'view', accessible_from=datetime.now(tz=timezone.utc) + timedelta(days=1))
        self.get(d.url_relative,
                 expect_status=403,
                 expect_contains='You can access this item in 23 hours from now.')
        self.test_user_3.grant_access(doc_id, 'view', accessible_to=datetime.now(tz=timezone.utc) - timedelta(days=1))
        self.get(d.url_relative,
                 expect_status=403,
                 expect_contains='Your access to this item has expired a day ago.')
