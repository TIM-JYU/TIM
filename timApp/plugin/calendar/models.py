from typing import Optional

from timApp.auth.sessioninfo import get_current_user_id
from timApp.timdb.sqa import db
from timApp.user.usergroup import UserGroup


class Eventgroup(db.Model):
    """Table for combining events to specific usergroups"""

    __tablename__ = "eventgroup"
    event_id = db.Column(db.Integer, db.ForeignKey("event.event_id"), primary_key=True)
    usergroup_id = db.Column(
        db.Integer, db.ForeignKey("usergroup.id"), primary_key=True
    )


class Enrollment(db.Model):
    """Table for enrollments; combines event, user group and enrollment type."""

    __tablename__ = "enrollment"
    event_id = db.Column(db.Integer, db.ForeignKey("event.event_id"), primary_key=True)
    usergroup_id = db.Column(
        db.Integer, db.ForeignKey("usergroup.id"), primary_key=True
    )
    enroll_type_id = db.Column(
        db.Integer, db.ForeignKey("enrollmenttype.enroll_type_id"), nullable=False
    )

    def get_enrollment_by_ids(event_id: int, user_group: usergroup_id):
        return Enrollment.query.filter(
            Enrollment.event_id == event_id, Enrollment.usergroup_id == user_group
        ).one_or_none()


class Event(db.Model):
    """Table for event. Contains all information of event"""

    __tablename__ = "event"
    event_id = db.Column(db.Integer, primary_key=True)
    location = db.Column(db.Text)
    max_size = db.Column(db.Integer)
    event_tag = db.Column(db.Text)
    start_time = db.Column(db.DateTime(timezone=True), nullable=False)
    end_time = db.Column(db.DateTime(timezone=True), nullable=False)
    message = db.Column(db.Text)
    title = db.Column(db.Text, nullable=False)
    signup_before = db.Column(db.DateTime(timezone=True))
    creator_user_id = db.Column(
        db.Integer, db.ForeignKey("useraccount.id"), nullable=False
    )

    # def __json__(self):
    #   return ["event_id", "title", "start_time", "end_time"]

    enrolled_users: list[UserGroup] = db.relationship(
        UserGroup,
        Enrollment.__table__,
        primaryjoin=event_id == Enrollment.event_id,
        lazy="select",
    )

    groups_in_event: list[UserGroup] = db.relationship(
        UserGroup,
        Eventgroup.__table__,
        primaryjoin=event_id == Eventgroup.event_id,
        lazy="select",
    )

    def get_event_by_id(event_id: int) -> Optional["Event"]:
        # cur_user = get_current_user_id()
        return Event.query.filter(Event.event_id == event_id).one_or_none()


class Enrollmenttype(db.Model):
    """Table for enrollment type, combines enrollment type ID to specific enrollment type"""

    __tablename__ = "enrollmenttype"
    enroll_type_id = db.Column(db.Integer, primary_key=True)
    enroll_type = db.Column(db.Text, nullable=False)


class ExportedCalendar(db.Model):
    """Table for hash codes used in calendar exporting. Combines user to its hash code"""

    __tablename__ = "exportedcalendar"
    user_id = db.Column(
        db.Integer, db.ForeignKey("useraccount.id"), primary_key=True, nullable=False
    )
    calendar_hash = db.Column(db.Text, nullable=False)
