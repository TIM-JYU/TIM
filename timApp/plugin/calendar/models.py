"""
The database classes for the calendar plugin
"""

__authors__ = [
    "Miika Immonen",
    "Terhi Kamula",
    "Anssi Lepikko",
    "Touko Miettinen",
    "Joose Tikkanen",
]
__license__ = "MIT"
__date__ = "24.5.2022"

from typing import Optional

from timApp.timdb.sqa import db
from timApp.user.user import User
from timApp.user.usergroup import UserGroup


class EventGroup(db.Model):
    """Table for specifying special group information for events."""

    __tablename__ = "eventgroup"
    event_id = db.Column(db.Integer, db.ForeignKey("event.event_id"), primary_key=True)
    """Event the the group belongs to"""

    usergroup_id = db.Column(
        db.Integer, db.ForeignKey("usergroup.id"), primary_key=True
    )
    """The usergroup that belongs to the group"""

    manager = db.Column(db.Boolean)
    """Is the group a manager (i.e. is able to modify event settings)?"""

    user_group = db.relationship(UserGroup, lazy="select")
    """The usergroup that belongs to the group"""


class Enrollment(db.Model):
    """A single enrollment in an event"""

    __tablename__ = "enrollment"
    event_id = db.Column(db.Integer, db.ForeignKey("event.event_id"), primary_key=True)
    """Event the enrollment is for"""

    usergroup_id = db.Column(
        db.Integer, db.ForeignKey("usergroup.id"), primary_key=True
    )
    """The usergroup that is enrolled (i.e. booked) in the event"""

    booker_message = db.Column(db.Text)
    """The message left by the booker"""

    enroll_type_id = db.Column(
        db.Integer, db.ForeignKey("enrollmenttype.enroll_type_id"), nullable=False
    )
    """Type of the enrollment"""

    @staticmethod
    def get_by_event_and_user(
        event_id: int, user_group_id: int
    ) -> Optional["Enrollment"]:
        """Returns a specific enrollment (or none) that match the user group id and event id"""
        return Enrollment.query.filter(
            Enrollment.event_id == event_id, Enrollment.usergroup_id == user_group_id
        ).one_or_none()


class Event(db.Model):
    """A calendar event. Event can have"""

    __tablename__ = "event"
    event_id = db.Column(db.Integer, primary_key=True)
    """Identification number of the event"""

    location = db.Column(db.Text)
    """Location of the event"""

    max_size = db.Column(db.Integer)
    """How many people can attend the event"""

    # TODO: Remove
    event_tag = db.Column(db.Text)

    start_time = db.Column(db.DateTime(timezone=True), nullable=False)
    """Start time of the event"""

    end_time = db.Column(db.DateTime(timezone=True), nullable=False)
    """End time of the event"""

    message = db.Column(db.Text)
    """Message visible to anyone who can see the event"""

    title = db.Column(db.Text, nullable=False)
    """Title of the event"""

    signup_before = db.Column(db.DateTime(timezone=True))
    """Time until signup is closed"""

    creator_user_id = db.Column(
        db.Integer, db.ForeignKey("useraccount.id"), nullable=False
    )
    """User who created the event originally"""

    enrolled_users: list[UserGroup] = db.relationship(
        UserGroup,
        Enrollment.__table__,
        primaryjoin=event_id == Enrollment.event_id,
        lazy="select",
    )
    """List of usergroups that are enrolled in the event"""

    creator: User = db.relationship(User)
    """User who created the event originally"""

    event_groups: list[EventGroup] = db.relationship(
        EventGroup,
        foreign_keys="EventGroup.event_id",
        cascade="all,delete-orphan",
    )

    @staticmethod
    def get_by_id(event_id: int) -> Optional["Event"]:
        return Event.query.filter_by(event_id=event_id).one_or_none()


class EnrollmentType(db.Model):
    """Table for enrollment type, combines enrollment type ID to specific enrollment type"""

    __tablename__ = "enrollmenttype"
    enroll_type_id = db.Column(db.Integer, primary_key=True)
    """Enrollment type"""

    enroll_type = db.Column(db.Text, nullable=False)
    """Name of the enrollment type"""


class ExportedCalendar(db.Model):
    """Information about exported calendars"""

    __tablename__ = "exportedcalendar"
    user_id = db.Column(
        db.Integer, db.ForeignKey("useraccount.id"), primary_key=True, nullable=False
    )
    """User who created the exported calendar"""

    calendar_hash = db.Column(db.Text, nullable=False)
    """Hash of the exported calendar"""

    user = db.relationship(User)
