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

from typing import Optional, Iterable

from sqlalchemy import func

from timApp.timdb.sqa import db
from timApp.user.user import User
from timApp.user.usergroup import UserGroup


class EventGroup(db.Model):
    """Information about a user group participating in an event."""

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

    event = db.relationship("Event", lazy="select")
    """The event the enrollment is related to"""

    usergroup = db.relationship(UserGroup, lazy="select")
    """User group that booked the event"""

    @staticmethod
    def get_by_event_and_user(
        event_id: int, user_group_id: int
    ) -> Optional["Enrollment"]:
        """Returns a specific enrollment (or none) that match the user group id and event id"""
        return Enrollment.query.filter(
            Enrollment.event_id == event_id, Enrollment.usergroup_id == user_group_id
        ).one_or_none()


class EventTagAttachment(db.Model):
    """Attachment information for the event tag"""

    __tablename__ = "eventtagattachment"
    event_id = db.Column(db.Integer, db.ForeignKey("event.event_id"), primary_key=True)
    """Event the tag is attached to"""
    tag_id = db.Column(db.Integer, db.ForeignKey("eventtag.tag_id"), primary_key=True)
    """Tag that is attached to the event"""


class EventTag(db.Model):
    """A string tag that can be attached to an event"""

    __tablename__ = "eventtag"
    tag_id = db.Column(db.Integer, primary_key=True)
    """The id of the tag"""

    tag = db.Column(db.Text, nullable=False)
    """The tag itself"""

    events: list["Event"] = db.relationship(
        "Event",
        secondary=EventTagAttachment.__table__,
        lazy="select",
        back_populates="tags",
    )

    @staticmethod
    def get_or_create(tags: Iterable[str]) -> list["EventTag"]:
        """
        Gets or creates new tags.

        If the tag does not exist, it is added to the session.

        :param tags: List of tags to get or create
        :return: List of already existing or new event tags that match
        """
        result = []
        existing_tags = EventTag.query.filter(EventTag.tag.in_(tags)).all()
        existing_tags_dict = {tag.tag: tag for tag in existing_tags}
        for tag in tags:
            if tag in existing_tags_dict:
                result.append(existing_tags_dict[tag])
            else:
                new_tag = EventTag(tag=tag)
                db.session.add(new_tag)
                result.append(new_tag)
        return result


class Event(db.Model):
    """A calendar event. Event has metadata (title, time, location) and various participating user groups."""

    __tablename__ = "event"
    event_id = db.Column(db.Integer, primary_key=True)
    """Identification number of the event"""

    location = db.Column(db.Text)
    """Location of the event"""

    max_size = db.Column(db.Integer)
    """How many people can attend the event"""

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

    enrollments: list[Enrollment] = db.relationship(
        Enrollment,
        lazy="select",
        back_populates="event",
        cascade="all, delete-orphan",
    )
    """Enrollment information for the event"""

    creator: User = db.relationship(User)
    """User who created the event originally"""

    event_groups: list[EventGroup] = db.relationship(
        EventGroup,
        foreign_keys="EventGroup.event_id",
        cascade="all,delete-orphan",
    )

    tags: list[EventTag] = db.relationship(
        EventTag,
        secondary=EventTagAttachment.__table__,
        lazy="select",
        back_populates="events",
    )
    """Tags attached to the event"""

    @property
    def enrollments_count(self) -> int:
        """Returns the number of enrollments in the event"""
        return (
            db.session.query(func.count(Enrollment.event_id))
            .filter(Enrollment.event_id == self.event_id)
            .scalar()
        )

    @staticmethod
    def get_by_id(event_id: int) -> Optional["Event"]:
        return Event.query.filter_by(event_id=event_id).one_or_none()

    def to_json(self, with_users: bool = False, for_user: User | None = None) -> dict:
        meta = {
            "signup_before": self.signup_before,
            "enrollments": self.enrollments_count,
            "maxSize": self.max_size,
            "location": self.location,
            "description": self.message,
        }

        if with_users:
            meta |= {
                "booker_groups": [
                    {
                        "name": e.usergroup.name,
                        "users": [u.to_json() for u in e.usergroup.users],
                        "message": e.booker_message,
                    }
                    for e in self.enrollments
                ]
            }
        elif for_user:
            user_group_ids = [ug.id for ug in for_user.groups]
            meta |= {
                "booker_groups": [
                    {
                        "name": e.usergroup.name,
                        "users": [for_user.to_json()],
                        "message": e.booker_message,
                    }
                    for e in self.enrollments
                    if e.usergroup_id in user_group_ids
                ]
            }

        return {
            "id": self.event_id,
            "title": self.title,
            "start": self.start_time,
            "end": self.end_time,
            "meta": meta,
        }


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
