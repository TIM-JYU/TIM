import datetime as dt
import zoneinfo
from typing import Optional

import pytz
import sqlalchemy as sa
from celery import schedules
from celery.utils.log import get_logger
from sqlalchemy import func
from sqlalchemy.event import listen
from sqlalchemy.orm import relationship, mapped_column, Mapped
from sqlalchemy.sql import select, insert, update

from .session import ModelBase
from .tzcrontab import TzAwareCrontab
from ..item.block import Block
from ..plugin.taskid import TaskId
from ..timdb.types import datetime_tz
from ..util.utils import cached_property

logger = get_logger("celery_sqlalchemy_scheduler.models")


def cronexp(field):
    """Representation of cron expression."""
    return field and str(field).replace(" ", "") or "*"


class ModelMixin:
    @classmethod
    def create(cls, **kw):
        return cls(**kw)

    def update(self, **kw):
        for attr, value in kw.items():
            setattr(self, attr, value)
        return self


class IntervalSchedule(ModelBase, ModelMixin):
    __tablename__ = "celery_interval_schedule"
    __table_args__ = {"sqlite_autoincrement": True}

    DAYS = "days"
    HOURS = "hours"
    MINUTES = "minutes"
    SECONDS = "seconds"
    MICROSECONDS = "microseconds"

    id: Mapped[int] = mapped_column(primary_key=True, autoincrement=True)

    every: Mapped[int]
    period: Mapped[Optional[str]] = mapped_column(sa.String(24))

    def __repr__(self):
        if self.every == 1:
            return f"every {self.period_singular}"
        return f"every {self.every} {self.period}"

    @property
    def schedule(self):
        return schedules.schedule(
            dt.timedelta(**{self.period: self.every}),
            # nowfun=lambda: make_aware(now())
            # nowfun=dt.datetime.now
        )

    @classmethod
    def from_schedule(cls, session, schedule, period=SECONDS):
        every = max(schedule.run_every.total_seconds(), 0)
        model = (
            session.execute(
                select(IntervalSchedule).filter_by(every=every, period=period).limit(1)
            )
            .scalars()
            .first()
        )
        if not model:
            model = cls(every=every, period=period)
            session.add(model)
            session.commit()
        return model

    @property
    def period_singular(self):
        return self.period[:-1]


class CrontabSchedule(ModelBase, ModelMixin):
    __tablename__ = "celery_crontab_schedule"
    __table_args__ = {"sqlite_autoincrement": True}

    id: Mapped[int] = mapped_column(primary_key=True, autoincrement=True)
    minute: Mapped[Optional[str]] = mapped_column(sa.String(60 * 4), default="*")
    hour: Mapped[Optional[str]] = mapped_column(sa.String(24 * 4), default="*")
    day_of_week: Mapped[Optional[str]] = mapped_column(sa.String(64), default="*")
    day_of_month: Mapped[Optional[str]] = mapped_column(sa.String(31 * 4), default="*")
    month_of_year: Mapped[Optional[str]] = mapped_column(sa.String(64), default="*")
    timezone: Mapped[Optional[str]] = mapped_column(sa.String(64), default="UTC")

    def __repr__(self):
        return "{} {} {} {} {} (m/h/d/dM/MY) {}".format(
            cronexp(self.minute),
            cronexp(self.hour),
            cronexp(self.day_of_week),
            cronexp(self.day_of_month),
            cronexp(self.month_of_year),
            str(self.timezone),
        )

    @property
    def schedule(self):
        return TzAwareCrontab(
            minute=self.minute,
            hour=self.hour,
            day_of_week=self.day_of_week,
            day_of_month=self.day_of_month,
            month_of_year=self.month_of_year,
            tz=pytz.timezone(self.timezone),
        )

    @classmethod
    def from_schedule(cls, session, schedule):
        spec = {
            "minute": schedule._orig_minute,
            "hour": schedule._orig_hour,
            "day_of_week": schedule._orig_day_of_week,
            "day_of_month": schedule._orig_day_of_month,
            "month_of_year": schedule._orig_month_of_year,
        }
        if schedule.tz:
            if isinstance(schedule.tz, zoneinfo.ZoneInfo):
                zone = schedule.tz.key
            elif isinstance(schedule.tz, pytz.BaseTzInfo):
                zone = schedule.tz.zone
            else:
                zone = str(schedule.tz)
            spec.update({"timezone": zone})
        model = (
            session.execute(select(CrontabSchedule).filter_by(**spec).limit(1))
            .scalars()
            .first()
        )
        if not model:
            model = cls(**spec)
            session.add(model)
            session.commit()
        return model


class SolarSchedule(ModelBase, ModelMixin):
    __tablename__ = "celery_solar_schedule"
    __table_args__ = {"sqlite_autoincrement": True}

    id: Mapped[int] = mapped_column(primary_key=True, autoincrement=True)
    event: Mapped[Optional[str]] = mapped_column(sa.String(24))
    latitude: Mapped[Optional[float]]
    longitude: Mapped[Optional[float]]

    @property
    def schedule(self):
        return schedules.solar(
            self.event, self.latitude, self.longitude, nowfun=dt.datetime.now
        )

    @classmethod
    def from_schedule(cls, session, schedule):
        spec = {
            "event": schedule.event,
            "latitude": schedule.lat,
            "longitude": schedule.lon,
        }
        model = (
            session.execute(select(SolarSchedule).filter_by(**spec)).scalars().first()
        )
        if not model:
            model = cls(**spec)
            session.add(model)
            session.commit()
        return model

    def __repr__(self):
        return f"{self.event} ({self.latitude}, {self.longitude})"


class PeriodicTaskChanged(ModelBase, ModelMixin):
    """Helper table for tracking updates to periodic tasks."""

    __tablename__ = "celery_periodic_task_changed"

    id: Mapped[int] = mapped_column(primary_key=True)
    last_update: Mapped[datetime_tz] = mapped_column(default=dt.datetime.now)

    @classmethod
    def changed(cls, mapper, connection, target):
        """
        :param mapper: the Mapper which is the target of this event
        :param connection: the Connection being used
        :param target: the mapped instance being persisted
        """
        if not target.no_changes:
            cls.update_changed(mapper, connection, target)

    @classmethod
    def update_changed(cls, mapper, connection, target):
        """
        :param mapper: the Mapper which is the target of this event
        :param connection: the Connection being used
        :param target: the mapped instance being persisted
        """
        s = connection.execute(
            select(PeriodicTaskChanged).where(PeriodicTaskChanged.id == 1).limit(1)
        )
        if not s:
            s = connection.execute(
                insert(PeriodicTaskChanged), last_update=dt.datetime.now()
            )
        else:
            s = connection.execute(
                update(PeriodicTaskChanged)
                .where(PeriodicTaskChanged.id == 1)
                .values(last_update=dt.datetime.now())
            )

    @classmethod
    def last_change(cls, session):
        periodic_tasks = (
            session.execute(select(PeriodicTaskChanged).filter_by(id=1))
            .scalars()
            .first()
        )
        if periodic_tasks:
            return periodic_tasks.last_update


class PeriodicTask(ModelBase, ModelMixin):
    __tablename__ = "celery_periodic_task"
    __table_args__ = {"sqlite_autoincrement": True}

    id: Mapped[int] = mapped_column(primary_key=True, autoincrement=True)
    block_id: Mapped[Optional[int]] = mapped_column(sa.ForeignKey("block.id"))
    block: Mapped[Optional[Block]] = relationship()
    # name
    name: Mapped[Optional[str]] = mapped_column(sa.String(255), unique=True)
    # task name
    task: Mapped[Optional[str]] = mapped_column(sa.String(255))

    # not use ForeignKey
    interval_id: Mapped[Optional[int]]
    interval: Mapped[Optional[IntervalSchedule]] = relationship(
        primaryjoin="foreign(PeriodicTask.interval_id) == remote(IntervalSchedule.id)",
    )

    crontab_id: Mapped[Optional[int]]
    crontab: Mapped[Optional[CrontabSchedule]] = relationship(
        primaryjoin="foreign(PeriodicTask.crontab_id) == remote(CrontabSchedule.id)",
    )

    solar_id: Mapped[Optional[int]]
    solar: Mapped[Optional[SolarSchedule]] = relationship(
        primaryjoin="foreign(PeriodicTask.solar_id) == remote(SolarSchedule.id)",
    )

    args: Mapped[Optional[str]] = mapped_column(default="[]")
    kwargs: Mapped[Optional[str]] = mapped_column(default="{}")
    # queue for celery
    queue: Mapped[Optional[str]] = mapped_column(sa.String(255))
    # exchange for celery
    exchange: Mapped[Optional[str]] = mapped_column(sa.String(255))
    # routing_key for celery
    routing_key: Mapped[Optional[str]] = mapped_column(sa.String(255))
    priority: Mapped[Optional[int]]
    expires: Mapped[Optional[datetime_tz]]

    # 只执行一次
    one_off: Mapped[Optional[bool]] = mapped_column(default=False)
    start_time: Mapped[Optional[datetime_tz]]
    enabled: Mapped[Optional[bool]] = mapped_column(default=True)
    last_run_at: Mapped[Optional[datetime_tz]]
    total_run_count: Mapped[int] = mapped_column(default=0)
    # 修改时间
    date_changed: Mapped[Optional[datetime_tz]] = mapped_column(
        default=func.now(), onupdate=func.now()
    )
    description: Mapped[Optional[str]] = mapped_column(default="")

    no_changes = False

    def __repr__(self):
        fmt = "{0.name}: {{no schedule}}"
        if self.interval:
            fmt = "{0.name}: {0.interval}"
        elif self.crontab:
            fmt = "{0.name}: {0.crontab}"
        elif self.solar:
            fmt = "{0.name}: {0.solar}"
        return fmt.format(self)

    @property
    def task_name(self):
        return self.task

    @task_name.setter
    def task_name(self, value):
        self.task = value

    @property
    def schedule(self):
        if self.interval:
            return self.interval.schedule
        elif self.crontab:
            return self.crontab.schedule
        elif self.solar:
            return self.solar.schedule
        raise ValueError(f"{self.name} schedule is None!")

    @cached_property
    def task_id(self):
        """Returns the TIM task id. Only valid for tasks added by users."""
        return TaskId.parse(self.name, allow_block_hint=False, allow_type=False)


listen(PeriodicTask, "after_insert", PeriodicTaskChanged.update_changed)
listen(PeriodicTask, "after_delete", PeriodicTaskChanged.update_changed)
listen(PeriodicTask, "after_update", PeriodicTaskChanged.changed)
listen(IntervalSchedule, "after_insert", PeriodicTaskChanged.update_changed)
listen(IntervalSchedule, "after_delete", PeriodicTaskChanged.update_changed)
listen(IntervalSchedule, "after_update", PeriodicTaskChanged.update_changed)
listen(CrontabSchedule, "after_insert", PeriodicTaskChanged.update_changed)
listen(CrontabSchedule, "after_delete", PeriodicTaskChanged.update_changed)
listen(CrontabSchedule, "after_update", PeriodicTaskChanged.update_changed)
listen(SolarSchedule, "after_insert", PeriodicTaskChanged.update_changed)
listen(SolarSchedule, "after_delete", PeriodicTaskChanged.update_changed)
listen(SolarSchedule, "after_update", PeriodicTaskChanged.update_changed)
