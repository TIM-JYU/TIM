from typing import Optional, TYPE_CHECKING

from sqlalchemy import func, select
from sqlalchemy.orm import mapped_column, Mapped, relationship

from timApp.document.version import Version
from timApp.notification.notification import NotificationType
from timApp.timdb.sqa import db
from timApp.timdb.types import datetime_tz

if TYPE_CHECKING:
    from timApp.user.user import User
    from timApp.item.block import Block

GroupingKey = tuple[int, str]


class PendingNotification(db.Model):
    __tablename__ = "pendingnotification"

    id: Mapped[int] = mapped_column(primary_key=True)
    user_id: Mapped[int] = mapped_column(db.ForeignKey("useraccount.id"))
    doc_id: Mapped[int] = mapped_column(db.ForeignKey("block.id"))
    discriminant: Mapped[str]
    par_id: Mapped[Optional[str]]
    text: Mapped[Optional[str]]
    created: Mapped[datetime_tz] = mapped_column(default=func.now())
    processed: Mapped[Optional[datetime_tz]] = mapped_column(index=True)
    kind: Mapped[NotificationType]

    user: Mapped["User"] = relationship(lazy="selectin")
    block: Mapped["Block"] = relationship()

    @property
    def grouping_key(self) -> GroupingKey:
        return NotImplemented

    @property
    def notify_type(self) -> NotificationType:
        return self.kind

    __mapper_args__ = {"polymorphic_on": "discriminant"}


class DocumentNotification(PendingNotification):
    """A notification that a document has changed."""

    version_change = mapped_column(db.Text)  # : str  # like "1,2/1,3"

    @property
    def version_before(self) -> Version:
        z = tuple(int(x) for x in self.version_change.split("/")[0].split(","))
        return z[0], z[1]

    @property
    def version_after(self) -> Version:
        z = tuple(int(x) for x in self.version_change.split("/")[1].split(","))
        return z[0], z[1]

    @property
    def grouping_key(self) -> GroupingKey:
        return self.doc_id, "d"

    __mapper_args__ = {
        "polymorphic_identity": "d",
    }


class CommentNotification(PendingNotification):
    """A notification that a comment has been added, changed or deleted."""

    @property
    def grouping_key(self) -> GroupingKey:
        return self.doc_id, "c"

    __mapper_args__ = {
        "polymorphic_identity": "c",
    }


class AnswerNotification(PendingNotification):
    """A notification that an answer has been added, changed or deleted."""

    answer_number = mapped_column(db.Integer)
    task_id = mapped_column(db.Text)

    @property
    def grouping_key(self) -> GroupingKey:
        return self.doc_id, "a"

    __mapper_args__ = {
        "polymorphic_identity": "a",
    }


def get_pending_notifications() -> list[PendingNotification]:
    return (
        db.session.execute(
            select(PendingNotification)
            .filter(PendingNotification.processed == None)
            .order_by(PendingNotification.created.asc())
        )
        .scalars()
        .all()
    )
