from datetime import timezone, timedelta, datetime
from pathlib import Path

from isodate import Duration

from timApp.item.distribute_rights import get_current_rights, do_register_right, ChangeTimeOp, ConfirmOp, \
    ChangeTimeGroupOp, UnlockOp, QuitOp, UndoQuitOp, UndoConfirmOp, Right, ChangeStartTimeGroupOp, RightOp, RightLog
from timApp.tests.server.timroutetest import TimRouteTest
from timApp.tim_app import app
from timApp.timdb.sqa import db
from timApp.user.usergroup import UserGroup
from timApp.util.flask.requesthelper import RouteException
from timApp.util.flask.responsehelper import to_json_str
from timApp.util.utils import get_current_time


def register_right_or_raise(self, op: RightOp, target_name: str) -> RightLog:
    res, err = do_register_right(op, target_name)
    if err:
        raise RouteException(err)
    return res


class DistRightsTest(TimRouteTest):
    def check(self, base_date, rights, i, rq, d_f, d_t, d_secs, acc_f, acc_t):
        self.assertEqual(Right(
            require_confirm=rq,
            duration_from=base_date + timedelta(seconds=d_f) if d_f is not None else None,
            duration_to=base_date + timedelta(seconds=d_t) if d_t is not None else None,
            duration=Duration(seconds=d_secs) if d_secs is not None else None,
            accessible_from=base_date + timedelta(seconds=acc_f) if acc_f is not None else None,
            accessible_to=base_date + timedelta(seconds=acc_t) if acc_t is not None else None,
        ), rights.get_right(f'test{i}@example.com'))

    def test_distribute_rights(self):
        tz = timezone(timedelta(0), '+0000')
        dt = datetime(year=2021, month=5, day=25, hour=10, minute=0, second=0, tzinfo=tz)
        target_name = 'test'

        def confirm(i: int, incr: timedelta):
            nonlocal dt
            dt = dt + incr
            return register_right_or_raise(ConfirmOp(type='confirm', email=f'test{i}@example.com', timestamp=dt),
                                           target_name)

        def changetime(i: int, incr: timedelta, secs: int):
            nonlocal dt
            dt = dt + incr
            return register_right_or_raise(
                ChangeTimeOp(type='changetime', email=f'test{i}@example.com', timestamp=dt, secs=secs), target_name)

        def changetimegroup(group: str, incr: timedelta, secs: int):
            nonlocal dt
            dt = dt + incr
            return register_right_or_raise(
                ChangeTimeGroupOp(type='changetimegroup', group=group, timestamp=dt, secs=secs),
                target_name)

        def changestarttimegroup(group: str, incr: timedelta, newtime: datetime):
            nonlocal dt
            dt = dt + incr
            return register_right_or_raise(
                ChangeStartTimeGroupOp(type='changestarttimegroup', group=group, timestamp=dt, starttime=newtime),
                target_name,
            )

        def unlock(i: int, incr: timedelta):
            nonlocal dt
            dt = dt + incr
            return register_right_or_raise(UnlockOp(type='unlock', email=f'test{i}@example.com', timestamp=dt),
                                           target_name)

        def quit(i: int, incr: timedelta):
            nonlocal dt
            dt = dt + incr
            return register_right_or_raise(QuitOp(type='quit', email=f'test{i}@example.com', timestamp=dt), target_name)

        def undoquit(i: int, incr: timedelta):
            nonlocal dt
            dt = dt + incr
            return register_right_or_raise(UndoQuitOp(type='undoquit', email=f'test{i}@example.com', timestamp=dt),
                                           target_name)

        def undoconfirm(i: int, incr: timedelta):
            nonlocal dt
            dt = dt + incr
            return register_right_or_raise(
                UndoConfirmOp(type='undoconfirm', email=f'test{i}@example.com', timestamp=dt),
                target_name)

        base_date = datetime(2021, 5, 25, 10, 0, tzinfo=tz)

        def check(*args):
            return self.check(base_date, *args)

        self.login_test1()
        ug = UserGroup.create('tg1')
        self.test_user_1.add_to_group(ug, None)
        self.test_user_2.add_to_group(ug, None)
        db.session.commit()
        fp = Path(app.config['FILES_PATH']) / f'{target_name}.rights.initial'
        with fp.open('w') as f:
            f.write(to_json_str(Right(
                require_confirm=True,
                duration_from=dt,
                duration_to=dt + timedelta(minutes=10),
                duration=Duration(hours=4),
                accessible_from=None,
                accessible_to=None,
            )) + '\n')
            f.write(to_json_str(ChangeTimeOp(
                type='changetime',
                email='test1@example.com',
                secs=5 * 60,
                timestamp=dt + timedelta(seconds=10),
            )))

        twosecs = timedelta(seconds=2)
        r, _ = get_current_rights(target_name)
        h = 3600
        m = 60
        check(r, 1, True, 0, 10 * m, 4 * h + 5 * m, None, None)
        check(r, 2, True, 0, 10 * m, 4 * h, None, None)
        r = confirm(1, twosecs)
        check(r, 1, False, 0, 10 * m, 4 * h + 5 * m, None, None)
        check(r, 2, True, 0, 10 * m, 4 * h, None, None)
        r = undoconfirm(1, twosecs)
        check(r, 1, True, 0, 10 * m, 4 * h + 5 * m, None, None)
        check(r, 2, True, 0, 10 * m, 4 * h, None, None)
        r = confirm(1, twosecs)
        check(r, 1, False, 0, 10 * m, 4 * h + 5 * m, None, None)
        check(r, 2, True, 0, 10 * m, 4 * h, None, None)
        r = changetime(2, twosecs, 20)
        check(r, 1, False, 0, 10 * m, 4 * h + 5 * m, None, None)
        check(r, 2, True, 0, 10 * m, 4 * h + 20, None, None)
        check(r, 3, True, 0, 10 * m, 4 * h, None, None)
        r = confirm(3, twosecs)
        check(r, 1, False, 0, 10 * m, 4 * h + 5 * m, None, None)
        check(r, 2, True, 0, 10 * m, 4 * h + 20, None, None)
        check(r, 3, False, 0, 10 * m, 4 * h, None, None)
        halfhour = timedelta(minutes=30)
        r = changestarttimegroup('tg1', twosecs, base_date + halfhour)
        halfhoursecs = halfhour.total_seconds()
        check(r, 1, False, halfhoursecs, halfhoursecs + 10 * m, 4 * h + 5 * m, None, None)
        check(r, 2, True, halfhoursecs, halfhoursecs + 10 * m, 4 * h + 20, None, None)
        check(r, 3, False, 0, 10 * m, 4 * h, None, None)
        r = changestarttimegroup('tg1', twosecs, base_date)
        check(r, 1, False, 0, 10 * m, 4 * h + 5 * m, None, None)
        check(r, 2, True, 0, 10 * m, 4 * h + 20, None, None)
        check(r, 3, False, 0, 10 * m, 4 * h, None, None)
        r = unlock(1, twosecs)
        unlock_time = 2 * 8
        check(r, 1, False, 0, 10 * m, 4 * h + 5 * m, unlock_time, 4 * h + 5 * m + unlock_time)
        r = changetime(1, twosecs, 20)
        check(r, 1, False, 0, 10 * m, 4 * h + 5 * m, unlock_time, 4 * h + 5 * m + unlock_time + 20)
        r = changetimegroup('tg1', twosecs, 15)
        check(r, 1, False, 0, 10 * m, 4 * h + 5 * m, unlock_time, 4 * h + 5 * m + unlock_time + 20 + 15)
        check(r, 2, True, 0, 10 * m, 4 * h + 20 + 15, None, None)
        r = changetimegroup('tg1', twosecs, 5)
        check(r, 1, False, 0, 10 * m, 4 * h + 5 * m, unlock_time, 4 * h + 5 * m + unlock_time + 20 + 15 + 5)
        check(r, 2, True, 0, 10 * m, 4 * h + 20 + 15 + 5, None, None)
        check(r, 3, False, 0, 10 * m, 4 * h, None, None)
        with self.assertRaises(RouteException):
            r = undoquit(1, twosecs)
        r = quit(1, twosecs)
        check(r, 1, False, 0, 10 * m, 4 * h + 5 * m, unlock_time, 2 * 13)
        with self.assertRaises(RouteException):
            quit(1, twosecs)
        with self.assertRaises(RouteException):
            changetime(1, twosecs, 20)
        with self.assertRaises(RouteException):
            unlock(1, twosecs)
        r = undoquit(1, twosecs)
        check(r, 1, False, 0, 10 * m, 4 * h + 5 * m, unlock_time, 4 * h + 5 * m + unlock_time + 20 + 15 + 5)
        r = changetime(1, twosecs, 5)
        check(r, 1, False, 0, 10 * m, 4 * h + 5 * m, unlock_time, 4 * h + 5 * m + unlock_time + 20 + 15 + 5 + 5)

        with self.assertRaises(FileNotFoundError):
            get_current_rights('does_not_exist')

        self.json_post(
            '/distRights/register',
            {'op': {'email': 'test1@example.com',
                    'type': 'confirm',
                    'timestamp': get_current_time(),
                    },
             'target': 'test', 'secret': 'xxx'},
            expect_status=400,
            expect_content='DIST_RIGHTS_REGISTER_SECRET not configured.',
        )
        d = self.create_doc()
        with self.temp_config({
            'DIST_RIGHTS_REGISTER_SECRET': 'xxx',
            'DIST_RIGHTS_SEND_SECRET': 'yyy',
            'DIST_RIGHTS_RECEIVE_SECRET': 'yyy',
            'DIST_RIGHTS_HOSTS': {'test': {
                'hosts': [f'http://{app.config["INTERNAL_PLUGIN_DOMAIN"]}:5001'],
                'item': d.path,
            }},
        }):
            with self.internal_container_ctx():
                self.json_post(
                    '/distRights/register',
                    {'op': {'email': 'test1@example.com',
                            'type': 'changetime',
                            'timestamp': get_current_time(),
                            'secs': 4,
                            },
                     'target': 'test', 'secret': 'xxx'},
                    expect_content={'host_errors': []},
                )
        r, _ = get_current_rights(target_name)
        check(r, 1, False, 0, 10 * m, 4 * h + 5 * m, unlock_time, 4 * h + 5 * m + unlock_time + 20 + 15 + 5 + 5 + 4)
        r = changestarttimegroup('testuser1', twosecs, base_date + halfhour)
        check(r, 1, False, 0, 10 * m, 4 * h + 5 * m,
              halfhoursecs, halfhoursecs + 4 * h + 5 * m + 20 + 15 + 5 + 5 + 4)

        self.get(
            '/distRights/changeStartTime',
            query_string={'target': 'test', 'group': 'tg1', 'minutes': 0, 'redir': '/'},
            expect_status=400,
            expect_content='DIST_RIGHTS_START_TIME_GROUP not configured.',
        )
        with self.temp_config({
            'DIST_RIGHTS_START_TIME_GROUP': 'testuser1',
            'DIST_RIGHTS_SEND_SECRET': 'yyy',
            'DIST_RIGHTS_RECEIVE_SECRET': 'yyy',
            'DIST_RIGHTS_HOSTS': {'test': {
                'hosts': [f'http://{app.config["INTERNAL_PLUGIN_DOMAIN"]}:5001'],
                'item': d.path,
            }},
        }):
            self.get(
                '/distRights/changeStartTime',
                query_string={'target': 'test', 'group': 'tg1', 'minutes': 0, 'redir': 'view/somedoc'},
                expect_status=302,
                expect_content='view/somedoc',
            )

    def test_distribute_rights_non_duration(self):
        tz = timezone(timedelta(0), '+0000')
        dt = datetime(year=2021, month=5, day=25, hour=10, minute=0, second=0, tzinfo=tz)
        target_name = 'test2'

        def confirm(i: int, incr: timedelta):
            nonlocal dt
            dt = dt + incr
            return register_right_or_raise(ConfirmOp(type='confirm', email=f'test{i}@example.com', timestamp=dt),
                                           target_name)

        def changetime(i: int, incr: timedelta, secs: int):
            nonlocal dt
            dt = dt + incr
            return register_right_or_raise(
                ChangeTimeOp(type='changetime', email=f'test{i}@example.com', timestamp=dt, secs=secs), target_name)

        def changetimegroup(group: str, incr: timedelta, secs: int):
            nonlocal dt
            dt = dt + incr
            return register_right_or_raise(
                ChangeTimeGroupOp(type='changetimegroup', group=group, timestamp=dt, secs=secs),
                target_name)

        def changestarttimegroup(group: str, incr: timedelta, newtime: datetime):
            nonlocal dt
            dt = dt + incr
            return register_right_or_raise(
                ChangeStartTimeGroupOp(type='changestarttimegroup', group=group, timestamp=dt, starttime=newtime),
                target_name,
            )

        def unlock(i: int, incr: timedelta):
            nonlocal dt
            dt = dt + incr
            return register_right_or_raise(UnlockOp(type='unlock', email=f'test{i}@example.com', timestamp=dt),
                                           target_name)

        def quit(i: int, incr: timedelta):
            nonlocal dt
            dt = dt + incr
            return register_right_or_raise(QuitOp(type='quit', email=f'test{i}@example.com', timestamp=dt), target_name)

        def undoquit(i: int, incr: timedelta):
            nonlocal dt
            dt = dt + incr
            return register_right_or_raise(UndoQuitOp(type='undoquit', email=f'test{i}@example.com', timestamp=dt),
                                           target_name)

        def undoconfirm(i: int, incr: timedelta):
            nonlocal dt
            dt = dt + incr
            return register_right_or_raise(
                UndoConfirmOp(type='undoconfirm', email=f'test{i}@example.com', timestamp=dt),
                target_name)
        
        base_date = datetime(2021, 5, 25, 10, 0, tzinfo=tz)

        self.login_test1()
        gname = 'tg2'
        ug = UserGroup.create(gname)
        self.test_user_1.add_to_group(ug, None)
        self.test_user_2.add_to_group(ug, None)
        db.session.commit()
        fp = Path(app.config['FILES_PATH']) / f'{target_name}.rights.initial'
        with fp.open('w') as f:
            f.write(to_json_str(Right(
                require_confirm=True,
                duration_from=None,
                duration_to=None,
                duration=None,
                accessible_from=base_date,
                accessible_to=base_date + timedelta(hours=4),
            )) + '\n')
            f.write(to_json_str(ChangeTimeOp(
                type='changetime',
                email='test1@example.com',
                secs=5 * 60,
                timestamp=dt + timedelta(seconds=10),
            )))

        twosecs = timedelta(seconds=2)
        r, _ = get_current_rights(target_name)
        fourhours = timedelta(hours=4).total_seconds()

        def check(*args):
            return self.check(base_date, *args)

        check(r, 1, True, None, None, None, 0, fourhours + 5 * 60)
        check(r, 2, True, None, None, None, 0, fourhours)
        r = confirm(1, twosecs)
        check(r, 1, False, None, None, None, 0, fourhours + 5 * 60)
        check(r, 2, True, None, None, None, 0, fourhours)
        r = undoconfirm(1, twosecs)
        check(r, 1, True, None, None, None, 0, fourhours + 5 * 60)
        check(r, 2, True, None, None, None, 0, fourhours)
        r = confirm(1, twosecs)
        check(r, 1, False, None, None, None, 0, fourhours + 5 * 60)
        check(r, 2, True, None, None, None, 0, fourhours)
        r = changetime(2, twosecs, 20)
        check(r, 1, False, None, None, None, 0, fourhours + 5 * 60)
        check(r, 2, True, None, None, None, 0, fourhours + 20)
        check(r, 3, True, None, None, None, 0, fourhours)
        r = confirm(3, twosecs)
        check(r, 1, False, None, None, None, 0, fourhours + 5 * 60)
        check(r, 2, True, None, None, None, 0, fourhours + 20)
        check(r, 3, False, None, None, None, 0, fourhours)
        halfhour = timedelta(minutes=30)
        r = changestarttimegroup(gname, twosecs, base_date + halfhour)
        halfhoursecs = halfhour.total_seconds()
        check(r, 1, False, None, None, None, halfhoursecs, fourhours + 5 * 60 + halfhoursecs)
        check(r, 2, True, None, None, None, halfhoursecs, fourhours + 20 + halfhoursecs)
        check(r, 3, False, None, None, None, 0, fourhours)
        r = changestarttimegroup(gname, twosecs, base_date)
        check(r, 1, False, None, None, None, 0, fourhours + 5 * 60)
        check(r, 2, True, None, None, None, 0, fourhours + 20)
        check(r, 3, False, None, None, None, 0, fourhours)

        # Does nothing; shouldn't happen anyway because the right is not a duration right.
        r = unlock(1, twosecs)

        check(r, 1, False, None, None, None, 0, fourhours + 5 * 60)
        r = changetime(1, twosecs, 20)
        check(r, 1, False, None, None, None, 0, fourhours + 5 * 60 + 20)
        r = changetimegroup(gname, twosecs, 15)
        check(r, 1, False, None, None, None, 0, fourhours + 5 * 60 + 20 + 15)
        check(r, 2, True, None, None, None, 0, fourhours + 20 + 15)
        r = changetimegroup(gname, twosecs, 5)
        check(r, 1, False, None, None, None, 0, fourhours + 5 * 60 + 20 + 15 + 5)
        check(r, 2, True, None, None, None, 0, fourhours + 20 + 15 + 5)
        check(r, 3, False, None, None, None, 0, fourhours)
        with self.assertRaises(RouteException):
            r = undoquit(1, twosecs)
        r = quit(1, twosecs)
        check(r, 1, False, None, None, None, 0, 2 * 13)
        with self.assertRaises(RouteException):
            quit(1, twosecs)
        with self.assertRaises(RouteException):
            changetime(1, twosecs, 20)
        with self.assertRaises(RouteException):
            unlock(1, twosecs)
        r = undoquit(1, twosecs)
        check(r, 1, False, None, None, None, 0, fourhours + 5 * 60 + 20 + 15 + 5)
        r = changetime(1, twosecs, 5)
        check(r, 1, False, None, None, None, 0, fourhours + 5 * 60 + 20 + 15 + 5 + 5)

        with self.assertRaises(FileNotFoundError):
            get_current_rights('does_not_exist')

        self.json_post(
            '/distRights/register',
            {'op': {'email': 'test1@example.com',
                    'type': 'confirm',
                    'timestamp': get_current_time(),
                    },
             'target': 'test', 'secret': 'xxx'},
            expect_status=400,
            expect_content='DIST_RIGHTS_REGISTER_SECRET not configured.',
        )
        d = self.create_doc()
        with self.temp_config({
            'DIST_RIGHTS_REGISTER_SECRET': 'xxx',
            'DIST_RIGHTS_SEND_SECRET': 'yyy',
            'DIST_RIGHTS_RECEIVE_SECRET': 'yyy',
            'DIST_RIGHTS_HOSTS': {'test': {
                'hosts': [f'http://{app.config["INTERNAL_PLUGIN_DOMAIN"]}:5001'],
                'item': d.path,
            }},
        }):
            with self.internal_container_ctx():
                self.json_post(
                    '/distRights/register',
                    {'op': {'email': 'test1@example.com',
                            'type': 'changetime',
                            'timestamp': get_current_time(),
                            'secs': 4,
                            },
                     'target': target_name, 'secret': 'xxx'},
                    expect_content={'host_errors': []},
                )
        r, _ = get_current_rights(target_name)
        check(r, 1, False, None, None, None, 0, fourhours + 5 * 60 + 20 + 15 + 5 + 5 + 4)
        r = changestarttimegroup('testuser1', twosecs, base_date + halfhour)
        check(r, 1, False, None, None, None, halfhoursecs, halfhoursecs + fourhours + 5 * 60 + 20 + 15 + 5 + 5 + 4)

        self.get(
            '/distRights/changeStartTime',
            query_string={'target': 'test', 'group': gname, 'minutes': 0, 'redir': '/'},
            expect_status=400,
            expect_content='DIST_RIGHTS_START_TIME_GROUP not configured.',
        )
        with self.temp_config({
            'DIST_RIGHTS_START_TIME_GROUP': 'testuser1',
            'DIST_RIGHTS_SEND_SECRET': 'yyy',
            'DIST_RIGHTS_RECEIVE_SECRET': 'yyy',
            'DIST_RIGHTS_HOSTS': {target_name: {
                'hosts': [f'http://{app.config["INTERNAL_PLUGIN_DOMAIN"]}:5001'],
                'item': d.path,
            }},
        }):
            self.get(
                '/distRights/changeStartTime',
                query_string={'target': target_name, 'group': gname, 'minutes': 0, 'redir': 'view/somedoc'},
                expect_status=302,
                expect_content='view/somedoc',
            )

    def test_receive_rights(self):
        target_name = "test3"
        dt = datetime.now()
        fp = Path(app.config['FILES_PATH']) / f'{target_name}.rights.initial'
        with fp.open('w') as f:
            f.write(to_json_str(Right(
                require_confirm=False,
                duration_from=None,
                duration_to=None,
                duration=None,
                accessible_from=dt,
                accessible_to=dt + timedelta(minutes=10),
            )) + '\n')

        self.login_test2()
        d = self.create_doc()
        self.login_test1()
        self.get(f'/view/{d.id}', expect_status=403)
        with self.temp_config({
            'DIST_RIGHTS_REGISTER_SECRET': 'xxx',
            'DIST_RIGHTS_SEND_SECRET': 'yyy',
            'DIST_RIGHTS_RECEIVE_SECRET': 'yyy',
            'DIST_RIGHTS_IS_DISTRIBUTOR': True,
            'DIST_RIGHTS_HOSTS': {target_name: {
                'hosts': [f'http://{app.config["INTERNAL_PLUGIN_DOMAIN"]}:5001'],
                'item': d.path,
            }},
        }):
            with self.internal_container_ctx():
                self.json_post(
                    '/distRights/register',
                    {'op': {'email': self.test_user_1.email,
                            'type': 'confirm',
                            'timestamp': get_current_time(),
                            },
                     'target': target_name, 'secret': 'xxx'},
                    expect_content={'host_errors': []},
                )

        self.get(f'/view/{d.id}', expect_status=200)
