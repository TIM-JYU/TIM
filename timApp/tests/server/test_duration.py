from datetime import timedelta, datetime, timezone

from routes.filters import humanize_datetime
from tests.server.timroutetest import TimRouteTest
from timdb.accesstype import AccessType
from timdb.models.docentry import DocEntry
from timdb.tim_models import db, BlockAccess


class DurationTest(TimRouteTest):
    def get_about_to_access_msg(self, period='a day'):
        return ['You are about to access a time-limited document.',
                'After you click Unlock, your access to this document will be removed in {}.'.format(period)]

    unlock_success = 'Item was unlocked successfully.'

    def test_duration_unlock(self):
        self.login_test1()
        d = self.create_doc()
        doc_id = d.id
        self.login_test2()
        self.db.users.grant_access(self.get_test_user_2_group_id(), doc_id, 'view', duration=timedelta(days=1))
        d = DocEntry.find_by_id(doc_id)
        self.get('/view/' + d.path,
                 expect_status=403,
                 expect_contains=self.get_about_to_access_msg())

        self.get('/view/' + d.path, query_string={'unlock': 'true'},
                 expect_contains=self.unlock_success)
        self.get('/view/' + d.path)
        ba = BlockAccess.query.filter_by(usergroup_id=self.get_test_user_2_group_id(), block_id=doc_id,
                                         type=self.db.users.get_access_type_id('view')).one()
        ba.accessible_to -= timedelta(days=2)
        db.session.commit()
        self.get('/view/' + d.path, expect_status=403,
                 expect_contains=self.get_expired_msg(ba.accessible_to))

    def test_timed_duration_unlock(self):
        self.login_test1()
        d = self.create_doc()
        doc_id = d.id
        self.login_test2()
        delta = timedelta(minutes=3)
        now_plus_minute = datetime.now(tz=timezone.utc) + delta
        self.db.users.grant_access(self.get_test_user_2_group_id(), doc_id, 'view',
                                   duration=timedelta(days=1),
                                   duration_from=now_plus_minute)
        d = DocEntry.find_by_id(doc_id)
        now = now_plus_minute - delta
        err_msg_too_early = 'You can unlock this item in {}.'.format(humanize_datetime(now_plus_minute))
        err_msg_too_late = 'You cannot unlock this item anymore (deadline expired {}).'.format(humanize_datetime(now))
        self.get('/view/' + d.path,
                 expect_status=403,
                 json_key='error',
                 expect_contains=[err_msg_too_early])
        self.get('/view/' + d.path, query_string={'unlock': 'true'},
                 expect_status=403,
                 json_key='error',
                 expect_contains=[err_msg_too_early])

        self.db.users.grant_access(self.get_test_user_2_group_id(), doc_id, 'view',
                                   duration=timedelta(days=1),
                                   duration_to=now)
        self.get('/view/' + d.path,
                 expect_status=403,
                 json_key='error',
                 expect_contains=[err_msg_too_late])
        self.get('/view/' + d.path, query_string={'unlock': 'true'},
                 expect_status=403,
                 json_key='error',
                 expect_contains=[err_msg_too_late])

        self.db.users.grant_access(self.get_test_user_2_group_id(), doc_id, 'view',
                                   duration=timedelta(days=1),
                                   duration_from=now)
        self.get('/view/' + d.path,
                 expect_status=403,
                 expect_contains=self.get_about_to_access_msg())
        self.get('/view/' + d.path, query_string={'unlock': 'true'},
                 expect_status=200,
                 expect_contains=self.unlock_success)
        self.get('/view/' + d.path)
        ba = BlockAccess.query.filter_by(usergroup_id=self.get_test_user_2_group_id(), block_id=doc_id,
                                         type=self.db.users.get_access_type_id('view')).one()
        ba.accessible_to -= timedelta(days=2)
        db.session.commit()
        self.get('/view/' + d.path, expect_status=403,
                 expect_contains=self.get_expired_msg(ba.accessible_to))

    def get_expired_msg(self, access):
        return 'Your access to this item has expired {}.'.format(humanize_datetime(access))

    def test_duration_group_unlock(self):
        self.login_test1()
        d = self.create_doc()
        doc_id = d.id
        self.login_test2()
        duration = timedelta(days=1)
        now = datetime.now(tz=timezone.utc)
        access = self.db.users.grant_access(self.db.users.get_logged_group_id(), doc_id, 'view',
                                            duration=duration,
                                            duration_to=now + duration)
        accesses = self.current_group().accesses.filter_by(type=AccessType.view.value).all()
        self.assertEqual(0, len(accesses))
        d = DocEntry.find_by_id(doc_id)
        self.get('/view/' + d.path,
                 expect_status=403,
                 expect_contains=self.get_about_to_access_msg())
        accesses = self.current_group().accesses.filter_by(type=AccessType.view.value).all()
        self.assertEqual(0, len(accesses))
        self.get('/view/' + d.path, query_string={'unlock': 'true'},
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

        self.get('/view/' + d.path,
                 expect_status=403,
                 expect_contains=self.get_expired_msg(granted_access.accessible_to))
        self.get('/view/' + d.path,
                 expect_status=403,
                 query_string={'unlock': 'true'},
                 expect_contains=self.get_expired_msg(granted_access.accessible_to))
        db.session.expunge_all()
        db.session.delete(granted_access)
        db.session.commit()
        self.get('/view/' + d.path,
                 expect_status=403,
                 expect_contains=self.get_about_to_access_msg())

    def test_duration_zero(self):
        self.login_test1()
        d = self.create_doc()
        doc_id = d.id
        self.login_test2()
        self.db.users.grant_access(self.get_test_user_2_group_id(), doc_id, 'view',
                                   duration=timedelta(days=0))
        self.get('/view/' + d.path,
                 expect_status=403,
                 expect_contains=self.get_about_to_access_msg('a moment'))
        self.get('/view/' + d.path,
                 query_string={'unlock': 'true'},
                 expect_status=403,
                 expect_contains=[self.unlock_success, self.get_expired_msg(datetime.now(tz=timezone.utc))])
