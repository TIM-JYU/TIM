from sqlalchemy import func, select

from timApp.document.version import Version
from timApp.notification.notification import NotificationType
from timApp.timdb.sqa import db
from timApp.user.user import User

GroupingKey = tuple[int, str]


class PendingNotification(db.Model):
    __tablename__ = "pendingnotification"
    __allow_unmapped__ = True

    id = db.Column(db.Integer, primary_key=True)
    user_id = db.Column(db.Integer, db.ForeignKey("useraccount.id"), nullable=False)
    doc_id = db.Column(db.Integer, db.ForeignKey("block.id"), nullable=False)
    discriminant = db.Column(db.Text, nullable=False)
    par_id = db.Column(db.Text, nullable=True)
    text = db.Column(db.Text, nullable=True)
    created = db.Column(db.DateTime(timezone=True), nullable=False, default=func.now())
    processed = db.Column(db.DateTime(timezone=True), nullable=True, index=True)
    kind = db.Column(db.Enum(NotificationType), nullable=False)

    user: User = db.relationship("User", lazy="selectin")
    block = db.relationship("Block")

    @property
    def grouping_key(self) -> GroupingKey:
        return NotImplemented

    @property
    def notify_type(self) -> NotificationType:
        return self.kind

    __mapper_args__ = {"polymorphic_on": discriminant}


class DocumentNotification(PendingNotification):
    """A notification that a document has changed."""

    version_change: str = db.Column(db.Text)  # like "1,2/1,3"

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

    answer_number = db.Column(db.Integer)
    task_id = db.Column(db.Text)

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
