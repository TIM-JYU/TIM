from typing import Any, Optional, TYPE_CHECKING

from sqlalchemy import UniqueConstraint, ForeignKey
from sqlalchemy.orm import mapped_column, Mapped, relationship

from timApp.timdb.sqa import db
from timApp.timdb.types import datetime_tz, DbModel

if TYPE_CHECKING:
    from timApp.user.user import User


class PeerReview(DbModel):
    """A peer review to a task."""

    __tablename__ = "peer_review"

    id: Mapped[int] = mapped_column(primary_key=True)
    """Review identifier."""

    answer_id: Mapped[Optional[int]] = mapped_column(ForeignKey("answer.id"))
    """Answer id."""

    task_name: Mapped[Optional[str]]
    """Task name"""

    block_id: Mapped[int] = mapped_column(ForeignKey("block.id"))
    """Doc id"""

    reviewer_id: Mapped[int] = mapped_column(ForeignKey("useraccount.id"))
    """Reviewer id"""

    reviewable_id: Mapped[int] = mapped_column(ForeignKey("useraccount.id"))
    """Reviewable id"""

    start_time: Mapped[datetime_tz]
    """Review start time"""

    end_time: Mapped[datetime_tz]
    """Review end time"""

    reviewed: Mapped[Optional[bool]] = mapped_column(default=False)
    """Review status"""

    points: Mapped[Optional[float]]
    """Points given by the reviewer"""

    comment: Mapped[Optional[str]]
    """Review comment"""

    __table_args__ = (
        UniqueConstraint("answer_id", "block_id", "reviewer_id", "reviewable_id"),
        UniqueConstraint("task_name", "block_id", "reviewer_id", "reviewable_id"),
    )

    reviewer: Mapped["User"] = relationship(foreign_keys=[reviewer_id])
    reviewable: Mapped["User"] = relationship(foreign_keys=[reviewable_id])

    def to_json(self) -> dict[str, Any]:
        return {
            "id": self.id,
            "answer_id": self.answer_id,
            "task_name": self.task_name,
            "block_id": self.block_id,
            "reviewer": self.reviewer,
            "reviewable": self.reviewable,
            "start_time": self.start_time,
            "end_time": self.end_time,
            "reviewed": self.reviewed,
            "points": self.points,
            "comment": self.comment,
        }
