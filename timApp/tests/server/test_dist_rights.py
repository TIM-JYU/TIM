from dataclasses import dataclass
from datetime import timezone, timedelta, datetime
from pathlib import Path

from isodate import Duration

from timApp.item.distribute_rights import (
    get_current_rights,
    do_register_right,
    ChangeTimeOp,
    ConfirmOp,
    ChangeTimeGroupOp,
    UnlockOp,
    QuitOp,
    UndoQuitOp,
    UndoConfirmOp,
    Right,
    ChangeStartTimeGroupOp,
    RightOp,
    RightLog,
    ConfirmGroupOp,
)
from timApp.tests.server.timroutetest import TimRouteTest
from timApp.tim_app import app
from timApp.timdb.sqa import db
from timApp.user.usergroup import UserGroup
from timApp.util.flask.requesthelper import RouteException
from timApp.util.flask.responsehelper import to_json_str
from timApp.util.utils import get_current_time


def register_right_or_raise(op: RightOp, target_name: str) -> RightLog:
    res, err = do_register_right(op, target_name)
    if err:
        raise RouteException(err)
    return res


@dataclass
class DistRightsProcessor:
    dt: datetime
    target_name: str

    def confirm(self, i: int, incr: timedelta):
        self.dt = self.dt + incr
        return register_right_or_raise(
            ConfirmOp(type="confirm", email=f"test{i}@example.com", timestamp=self.dt),
            self.target_name,
        )

    def confirmgroup(self, group: str, incr: timedelta):
        self.dt = self.dt + incr
        return register_right_or_raise(
            ConfirmGroupOp(type="confirmgroup", group=group, timestamp=self.dt),
            self.target_name,
        )

    def changetime(self, i: int, incr: timedelta, secs: int):
        self.dt = self.dt + incr
        return register_right_or_raise(
            ChangeTimeOp(
                type="changetime",
                email=f"test{i}@example.com",
                timestamp=self.dt,
                secs=secs,
            ),
            self.target_name,
        )

    def changetimegroup(self, group: str, incr: timedelta, secs: int):
        self.dt = self.dt + incr
        return register_right_or_raise(
            ChangeTimeGroupOp(
                type="changetimegroup", group=group, timestamp=self.dt, secs=secs
            ),
            self.target_name,
        )

    def changestarttimegroup(self, group: str, incr: timedelta, newtime: datetime):
        self.dt = self.dt + incr
        return register_right_or_raise(
            ChangeStartTimeGroupOp(
                type="changestarttimegroup",
                group=group,
                timestamp=self.dt,
                starttime=newtime,
            ),
            self.target_name,
        )

    def unlock(self, i: int, incr: timedelta):
        self.dt = self.dt + incr
        return register_right_or_raise(
            UnlockOp(type="unlock", email=f"test{i}@example.com", timestamp=self.dt),
            self.target_name,
        )

    def quit(self, i: int, incr: timedelta):
        self.dt = self.dt + incr
        return register_right_or_raise(
            QuitOp(type="quit", email=f"test{i}@example.com", timestamp=self.dt),
            self.target_name,
        )

    def undoquit(self, i: int, incr: timedelta):
        self.dt = self.dt + incr
        return register_right_or_raise(
            UndoQuitOp(
                type="undoquit", email=f"test{i}@example.com", timestamp=self.dt
            ),
            self.target_name,
        )

    def undoconfirm(self, i: int, incr: timedelta):
        self.dt = self.dt + incr
        return register_right_or_raise(
            UndoConfirmOp(
                type="undoconfirm", email=f"test{i}@example.com", timestamp=self.dt
            ),
            self.target_name,
        )


h = 3600
m = 60


class DistRightsTest(TimRouteTest):
    def check(self, base_date, rights, i, rq, d_f, d_t, d_secs, acc_f, acc_t):
        self.assertEqual(
            Right(
                require_confirm=rq,
                duration_from=base_date + timedelta(seconds=d_f)
                if d_f is not None
                else None,
                duration_to=base_date + timedelta(seconds=d_t)
                if d_t is not None
                else None,
                duration=Duration(seconds=d_secs) if d_secs is not None else None,
                accessible_from=base_date + timedelta(seconds=acc_f)
                if acc_f is not None
                else None,
                accessible_to=base_date + timedelta(seconds=acc_t)
                if acc_t is not None
                else None,
            ),
            rights.get_right(f"test{i}@example.com"),
        )

    def init_processor(self, target_name: str):
        tz = timezone(timedelta(0), "+0000")
        processor = DistRightsProcessor(
            dt=datetime(
                year=2021,
                month=5,
                day=25,
                hour=10,
                minute=0,
                second=0,
                tzinfo=tz,
            ),
            target_name=target_name,
        )
        base_date = datetime(2021, 5, 25, 10, 0, 0, tzinfo=tz)

        def check(*args):
            return self.check(base_date, *args)

        return processor, base_date, check

    def write_initial(
        self, processor: DistRightsProcessor, initial_right: Right, ops: list[RightOp]
    ):
        fp = Path(app.config["FILES_PATH"]) / f"{processor.target_name}.rights.initial"
        with fp.open("w") as f:
            f.write(to_json_str(initial_right) + "\n")
            for op in ops:
                f.write(to_json_str(op) + "\n")

    def test_group_manipulation(self):
        processor, base_date, check = self.init_processor("test_group_manipulation")
        self.login_test1()
        UserGroup.create("tg_manip1")
        db.session.commit()
        self.write_initial(
            processor,
            Right(
                require_confirm=True,
                duration_from=None,
                duration_to=None,
                duration=None,
                accessible_from=processor.dt,
                accessible_to=processor.dt + timedelta(hours=1),
            ),
            [
                ChangeStartTimeGroupOp(
                    type="changestarttimegroup",
                    group="tg_manip1",
                    starttime=processor.dt + timedelta(hours=1),
                    timestamp=processor.dt,
                ),
            ],
        )

        twosecs = timedelta(seconds=2)
        # Basic case: the user is not in a group, he should get default rights on confirm
        r, _ = get_current_rights(processor.target_name)
        check(r, 1, True, None, None, None, 0, 1 * h)
        r = processor.confirm(1, twosecs)
        check(r, 1, False, None, None, None, 0, 1 * h)

        # Basic remove case: undo confirm
        r = processor.undoconfirm(1, twosecs)
        check(r, 1, True, None, None, None, 0, 1 * h)

        # Group add case: The user is added to the group and then confirmed
        # Intentionally don't flush after adding user to group, simulate UserSelect behaviour

        ug = UserGroup.get_by_name("tg_manip1")
        self.test_user_1.add_to_group(ug, None)

        r = processor.confirm(1, twosecs)
        check(r, 1, False, None, None, None, 1 * h, 2 * h)
        db.session.commit()

        # Group remove case: The user is removed from the group and then unconfirmed
        ug = UserGroup.get_by_name("tg_manip1")
        membership = ug.current_memberships.get(self.test_user_1.id, None)
        if membership:
            membership.set_expired()
        db.session.commit()
        r = processor.undoconfirm(1, twosecs)
        check(r, 1, True, None, None, None, 0, 1 * h)

    def test_distribute_rights(self):
        processor, base_date, check = self.init_processor("test")
        self.login_test1()
        ug = UserGroup.create("tg1")
        self.test_user_1.add_to_group(ug, None)
        self.test_user_2.add_to_group(ug, None)
        empty = UserGroup.create("some_empty_group1")
        db.session.commit()
        self.write_initial(
            processor,
            Right(
                require_confirm=True,
                duration_from=processor.dt,
                duration_to=processor.dt + timedelta(minutes=10),
                duration=Duration(hours=4),
                accessible_from=None,
                accessible_to=None,
            ),
            [
                ChangeTimeOp(
                    type="changetime",
                    email="test1@example.com",
                    secs=5 * 60,
                    timestamp=processor.dt + timedelta(seconds=10),
                )
            ],
        )

        twosecs = timedelta(seconds=2)
        r, _ = get_current_rights(processor.target_name)
        check(r, 1, True, 0, 10 * m, 4 * h + 5 * m, None, None)
        check(r, 2, True, 0, 10 * m, 4 * h, None, None)
        r = processor.confirmgroup("tg1", twosecs)
        check(r, 1, False, 0, 10 * m, 4 * h + 5 * m, None, None)
        check(r, 2, False, 0, 10 * m, 4 * h, None, None)
        check(r, 3, True, 0, 10 * m, 4 * h, None, None)
        r = processor.undoconfirm(1, twosecs)
        r = processor.undoconfirm(2, twosecs)
        check(r, 1, True, 0, 10 * m, 4 * h + 5 * m, None, None)
        check(r, 2, True, 0, 10 * m, 4 * h, None, None)
        r = processor.confirm(1, twosecs)
        check(r, 1, False, 0, 10 * m, 4 * h + 5 * m, None, None)
        check(r, 2, True, 0, 10 * m, 4 * h, None, None)
        r = processor.undoconfirm(1, twosecs)
        check(r, 1, True, 0, 10 * m, 4 * h + 5 * m, None, None)
        check(r, 2, True, 0, 10 * m, 4 * h, None, None)
        r = processor.confirm(1, twosecs)
        check(r, 1, False, 0, 10 * m, 4 * h + 5 * m, None, None)
        check(r, 2, True, 0, 10 * m, 4 * h, None, None)
        r = processor.changetime(2, twosecs, 20)
        check(r, 1, False, 0, 10 * m, 4 * h + 5 * m, None, None)
        check(r, 2, True, 0, 10 * m, 4 * h + 20, None, None)
        check(r, 3, True, 0, 10 * m, 4 * h, None, None)
        r = processor.confirm(3, twosecs)
        check(r, 1, False, 0, 10 * m, 4 * h + 5 * m, None, None)
        check(r, 2, True, 0, 10 * m, 4 * h + 20, None, None)
        check(r, 3, False, 0, 10 * m, 4 * h, None, None)
        halfhour = timedelta(minutes=30)
        r = processor.changestarttimegroup("tg1", twosecs, base_date + halfhour)
        halfhoursecs = halfhour.total_seconds()
        check(
            r, 1, False, halfhoursecs, halfhoursecs + 10 * m, 4 * h + 5 * m, None, None
        )
        check(r, 2, True, halfhoursecs, halfhoursecs + 10 * m, 4 * h + 20, None, None)
        check(r, 3, False, 0, 10 * m, 4 * h, None, None)
        r = processor.changestarttimegroup("tg1", twosecs, base_date)
        check(r, 1, False, 0, 10 * m, 4 * h + 5 * m, None, None)
        check(r, 2, True, 0, 10 * m, 4 * h + 20, None, None)
        check(r, 3, False, 0, 10 * m, 4 * h, None, None)
        r = processor.unlock(1, twosecs)
        unlock_time = 2 * 11
        check(
            r,
            1,
            False,
            0,
            10 * m,
            4 * h + 5 * m,
            unlock_time,
            4 * h + 5 * m + unlock_time,
        )
        r = processor.changetime(1, twosecs, 20)
        check(
            r,
            1,
            False,
            0,
            10 * m,
            4 * h + 5 * m,
            unlock_time,
            4 * h + 5 * m + unlock_time + 20,
        )
        r = processor.changetimegroup("tg1", twosecs, 15)
        check(
            r,
            1,
            False,
            0,
            10 * m,
            4 * h + 5 * m,
            unlock_time,
            4 * h + 5 * m + unlock_time + 20 + 15,
        )
        check(r, 2, True, 0, 10 * m, 4 * h + 20 + 15, None, None)
        r = processor.changetimegroup("tg1", twosecs, 5)
        check(
            r,
            1,
            False,
            0,
            10 * m,
            4 * h + 5 * m,
            unlock_time,
            4 * h + 5 * m + unlock_time + 20 + 15 + 5,
        )
        check(r, 2, True, 0, 10 * m, 4 * h + 20 + 15 + 5, None, None)
        check(r, 3, False, 0, 10 * m, 4 * h, None, None)
        with self.assertRaises(RouteException):
            r = processor.undoquit(1, twosecs)
        r = processor.quit(1, twosecs)
        check(r, 1, False, 0, 10 * m, 4 * h + 5 * m, unlock_time, 2 * 16)
        with self.assertRaises(RouteException):
            processor.quit(1, twosecs)
        with self.assertRaises(RouteException):
            processor.changetime(1, twosecs, 20)
        with self.assertRaises(RouteException):
            processor.unlock(1, twosecs)
        r = processor.undoquit(1, twosecs)
        check(
            r,
            1,
            False,
            0,
            10 * m,
            4 * h + 5 * m,
            unlock_time,
            4 * h + 5 * m + unlock_time + 20 + 15 + 5,
        )
        r = processor.changetime(1, twosecs, 5)
        check(
            r,
            1,
            False,
            0,
            10 * m,
            4 * h + 5 * m,
            unlock_time,
            4 * h + 5 * m + unlock_time + 20 + 15 + 5 + 5,
        )

        with self.assertRaises(FileNotFoundError):
            get_current_rights("does_not_exist")

        self.json_post(
            "/distRights/register",
            {
                "op": {
                    "email": "test1@example.com",
                    "type": "confirm",
                    "timestamp": get_current_time(),
                },
                "target": "test",
                "secret": "xxx",
            },
            expect_status=400,
            expect_content="DIST_RIGHTS_REGISTER_SECRET not configured.",
        )
        d = self.create_doc()
        with self.temp_config(
            {
                "DIST_RIGHTS_REGISTER_SECRET": "xxx",
                "DIST_RIGHTS_SEND_SECRET": "yyy",
                "DIST_RIGHTS_RECEIVE_SECRET": "yyy",
                "DIST_RIGHTS_HOSTS": {
                    "test": {
                        "hosts": [
                            f'http://{app.config["INTERNAL_PLUGIN_DOMAIN"]}:5001'
                        ],
                        "item": d.path,
                    }
                },
            }
        ):
            with self.internal_container_ctx():
                self.json_post(
                    "/distRights/register",
                    {
                        "op": {
                            "email": "test1@example.com",
                            "type": "changetime",
                            "timestamp": get_current_time(),
                            "secs": 4,
                        },
                        "target": "test",
                        "secret": "xxx",
                    },
                    expect_content={"host_errors": []},
                )
        r, _ = get_current_rights(processor.target_name)
        check(
            r,
            1,
            False,
            0,
            10 * m,
            4 * h + 5 * m,
            unlock_time,
            4 * h + 5 * m + unlock_time + 20 + 15 + 5 + 5 + 4,
        )
        r = processor.changestarttimegroup("testuser1", twosecs, base_date + halfhour)
        check(
            r,
            1,
            False,
            0,
            10 * m,
            4 * h + 5 * m,
            halfhoursecs,
            halfhoursecs + 4 * h + 5 * m + 20 + 15 + 5 + 5 + 4,
        )

        # An operation on an empty group should not raise an exception.
        processor.confirmgroup("some_empty_group1", twosecs)

        # An operation on a non-existent group should raise an exception.
        with self.assertRaises(Exception):
            processor.confirmgroup("this_does_not_exist", twosecs)

        self.make_admin(self.test_user_3)
        self.login_test3()
        resp = self.get(
            "/distRights/current",
            query_string={"target": processor.target_name, "groups": "tg1,testuser3"},
        )
        self.assertEqual(
            [
                {
                    "email": "test1@example.com",
                    "right": {
                        "accessible_from": "2021-05-25T10:30:00+00:00",
                        "accessible_to": "2021-05-25T14:35:49+00:00",
                        "duration": "PT4H5M",
                        "duration_from": "2021-05-25T10:00:00+00:00",
                        "duration_to": "2021-05-25T10:10:00+00:00",
                        "require_confirm": False,
                    },
                },
                {
                    "email": "test2@example.com",
                    "right": {
                        "accessible_from": None,
                        "accessible_to": None,
                        "duration": "PT4H40S",
                        "duration_from": "2021-05-25T10:00:00+00:00",
                        "duration_to": "2021-05-25T10:10:00+00:00",
                        "require_confirm": True,
                    },
                },
                {
                    "email": "test3@example.com",
                    "right": {
                        "accessible_from": None,
                        "accessible_to": None,
                        "duration": "PT4H",
                        "duration_from": "2021-05-25T10:00:00+00:00",
                        "duration_to": "2021-05-25T10:10:00+00:00",
                        "require_confirm": False,
                    },
                },
            ],
            resp,
        )
        self.get(
            "/distRights/current",
            query_string={"target": "not_exist", "groups": "tg1,testuser3"},
            expect_status=400,
            expect_content="Unknown target: not_exist",
        )

        self.get(
            "/distRights/changeStartTime",
            query_string={"target": "test", "group": "tg1", "minutes": 0, "redir": "/"},
            expect_status=400,
            expect_content="DIST_RIGHTS_START_TIME_GROUP not configured.",
        )
        with self.temp_config(
            {
                "DIST_RIGHTS_START_TIME_GROUP": "testuser1",
                "DIST_RIGHTS_SEND_SECRET": "yyy",
                "DIST_RIGHTS_RECEIVE_SECRET": "yyy",
                "DIST_RIGHTS_HOSTS": {
                    "test": {
                        "hosts": [
                            f'http://{app.config["INTERNAL_PLUGIN_DOMAIN"]}:5001'
                        ],
                        "item": d.path,
                    }
                },
            }
        ):
            self.get(
                "/distRights/changeStartTime",
                query_string={
                    "target": "test",
                    "group": "tg1",
                    "minutes": 0,
                    "redir": "view/somedoc",
                },
                expect_status=302,
                expect_content="view/somedoc",
            )

    def test_distribute_rights_non_duration(self):
        tz = timezone(timedelta(0), "+0000")
        dt = datetime(
            year=2021, month=5, day=25, hour=10, minute=0, second=0, tzinfo=tz
        )
        target_name = "test2"

        def confirm(i: int, incr: timedelta):
            nonlocal dt
            dt = dt + incr
            return register_right_or_raise(
                ConfirmOp(type="confirm", email=f"test{i}@example.com", timestamp=dt),
                target_name,
            )

        def changetime(i: int, incr: timedelta, secs: int):
            nonlocal dt
            dt = dt + incr
            return register_right_or_raise(
                ChangeTimeOp(
                    type="changetime",
                    email=f"test{i}@example.com",
                    timestamp=dt,
                    secs=secs,
                ),
                target_name,
            )

        def changetimegroup(group: str, incr: timedelta, secs: int):
            nonlocal dt
            dt = dt + incr
            return register_right_or_raise(
                ChangeTimeGroupOp(
                    type="changetimegroup", group=group, timestamp=dt, secs=secs
                ),
                target_name,
            )

        def changestarttimegroup(group: str, incr: timedelta, newtime: datetime):
            nonlocal dt
            dt = dt + incr
            return register_right_or_raise(
                ChangeStartTimeGroupOp(
                    type="changestarttimegroup",
                    group=group,
                    timestamp=dt,
                    starttime=newtime,
                ),
                target_name,
            )

        def unlock(i: int, incr: timedelta):
            nonlocal dt
            dt = dt + incr
            return register_right_or_raise(
                UnlockOp(type="unlock", email=f"test{i}@example.com", timestamp=dt),
                target_name,
            )

        def quit(i: int, incr: timedelta):
            nonlocal dt
            dt = dt + incr
            return register_right_or_raise(
                QuitOp(type="quit", email=f"test{i}@example.com", timestamp=dt),
                target_name,
            )

        def undoquit(i: int, incr: timedelta):
            nonlocal dt
            dt = dt + incr
            return register_right_or_raise(
                UndoQuitOp(type="undoquit", email=f"test{i}@example.com", timestamp=dt),
                target_name,
            )

        def undoconfirm(i: int, incr: timedelta):
            nonlocal dt
            dt = dt + incr
            return register_right_or_raise(
                UndoConfirmOp(
                    type="undoconfirm", email=f"test{i}@example.com", timestamp=dt
                ),
                target_name,
            )

        base_date = datetime(2021, 5, 25, 10, 0, tzinfo=tz)

        self.login_test1()
        gname = "tg2"
        ug = UserGroup.create(gname)
        self.test_user_1.add_to_group(ug, None)
        self.test_user_2.add_to_group(ug, None)
        db.session.commit()
        fp = Path(app.config["FILES_PATH"]) / f"{target_name}.rights.initial"
        with fp.open("w") as f:
            f.write(
                to_json_str(
                    Right(
                        require_confirm=True,
                        duration_from=None,
                        duration_to=None,
                        duration=None,
                        accessible_from=base_date,
                        accessible_to=base_date + timedelta(hours=4),
                    )
                )
                + "\n"
            )
            f.write(
                to_json_str(
                    ChangeTimeOp(
                        type="changetime",
                        email="test1@example.com",
                        secs=5 * 60,
                        timestamp=dt + timedelta(seconds=10),
                    )
                )
            )

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
        check(
            r,
            1,
            False,
            None,
            None,
            None,
            halfhoursecs,
            fourhours + 5 * 60 + halfhoursecs,
        )
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
            get_current_rights("does_not_exist")

        self.json_post(
            "/distRights/register",
            {
                "op": {
                    "email": "test1@example.com",
                    "type": "confirm",
                    "timestamp": get_current_time(),
                },
                "target": "test",
                "secret": "xxx",
            },
            expect_status=400,
            expect_content="DIST_RIGHTS_REGISTER_SECRET not configured.",
        )
        d = self.create_doc()
        with self.temp_config(
            {
                "DIST_RIGHTS_REGISTER_SECRET": "xxx",
                "DIST_RIGHTS_SEND_SECRET": "yyy",
                "DIST_RIGHTS_RECEIVE_SECRET": "yyy",
                "DIST_RIGHTS_HOSTS": {
                    "test": {
                        "hosts": [
                            f'http://{app.config["INTERNAL_PLUGIN_DOMAIN"]}:5001'
                        ],
                        "item": d.path,
                    }
                },
            }
        ):
            with self.internal_container_ctx():
                self.json_post(
                    "/distRights/register",
                    {
                        "op": {
                            "email": "test1@example.com",
                            "type": "changetime",
                            "timestamp": get_current_time(),
                            "secs": 4,
                        },
                        "target": target_name,
                        "secret": "xxx",
                    },
                    expect_content={"host_errors": []},
                )
        r, _ = get_current_rights(target_name)
        check(
            r, 1, False, None, None, None, 0, fourhours + 5 * 60 + 20 + 15 + 5 + 5 + 4
        )
        r = changestarttimegroup("testuser1", twosecs, base_date + halfhour)
        check(
            r,
            1,
            False,
            None,
            None,
            None,
            halfhoursecs,
            halfhoursecs + fourhours + 5 * 60 + 20 + 15 + 5 + 5 + 4,
        )

        self.get(
            "/distRights/changeStartTime",
            query_string={"target": "test", "group": gname, "minutes": 0, "redir": "/"},
            expect_status=400,
            expect_content="DIST_RIGHTS_START_TIME_GROUP not configured.",
        )
        with self.temp_config(
            {
                "DIST_RIGHTS_START_TIME_GROUP": "testuser1",
                "DIST_RIGHTS_SEND_SECRET": "yyy",
                "DIST_RIGHTS_RECEIVE_SECRET": "yyy",
                "DIST_RIGHTS_HOSTS": {
                    target_name: {
                        "hosts": [
                            f'http://{app.config["INTERNAL_PLUGIN_DOMAIN"]}:5001'
                        ],
                        "item": d.path,
                    }
                },
            }
        ):
            self.get(
                "/distRights/changeStartTime",
                query_string={
                    "target": target_name,
                    "group": gname,
                    "minutes": 0,
                    "redir": "view/somedoc",
                },
                expect_status=302,
                expect_content="view/somedoc",
            )

    def test_receive_rights(self):
        target_name = "test3"
        dt = datetime.now()
        fp = Path(app.config["FILES_PATH"]) / f"{target_name}.rights.initial"
        with fp.open("w") as f:
            f.write(
                to_json_str(
                    Right(
                        require_confirm=False,
                        duration_from=None,
                        duration_to=None,
                        duration=None,
                        accessible_from=dt,
                        accessible_to=dt + timedelta(minutes=10),
                    )
                )
                + "\n"
            )

        self.login_test2()
        d = self.create_doc()
        self.login_test1()
        self.get(f"/view/{d.id}", expect_status=403)
        with self.temp_config(
            {
                "DIST_RIGHTS_REGISTER_SECRET": "xxx",
                "DIST_RIGHTS_SEND_SECRET": "yyy",
                "DIST_RIGHTS_RECEIVE_SECRET": "yyy",
                "DIST_RIGHTS_IS_DISTRIBUTOR": True,
                "DIST_RIGHTS_HOSTS": {
                    target_name: {
                        "hosts": [
                            f'http://{app.config["INTERNAL_PLUGIN_DOMAIN"]}:5001'
                        ],
                        "item": d.path,
                    }
                },
            }
        ):
            with self.internal_container_ctx():
                self.json_post(
                    "/distRights/register",
                    {
                        "op": {
                            "email": self.test_user_1.email,
                            "type": "confirm",
                            "timestamp": get_current_time(),
                        },
                        "target": target_name,
                        "secret": "xxx",
                    },
                    expect_content={"host_errors": []},
                )

        self.get(f"/view/{d.id}", expect_status=200)
