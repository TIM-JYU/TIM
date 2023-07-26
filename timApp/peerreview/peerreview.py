from typing import Any

from sqlalchemy.orm import mapped_column

from timApp.timdb.sqa import db


class PeerReview(db.Model):
    """A peer review to a task."""

    __tablename__ = "peer_review"
    
    
    id = mapped_column(db.Integer, primary_key=True)
    """Review identifier."""

    answer_id = mapped_column(db.Integer, db.ForeignKey("answer.id"), nullable=True)
    """Answer id."""

    task_name = mapped_column(db.Text, nullable=True)
    """Task name"""

    block_id = mapped_column(db.Integer, db.ForeignKey("block.id"), nullable=False)
    """Doc id"""

    reviewer_id = mapped_column(db.Integer, db.ForeignKey("useraccount.id"), nullable=False)
    """Reviewer id"""

    reviewable_id = mapped_column(
        db.Integer, db.ForeignKey("useraccount.id"), nullable=False
    )
    """Reviewable id"""

    start_time = mapped_column(db.DateTime(timezone=True), nullable=False)
    """Review start time"""

    end_time = mapped_column(db.DateTime(timezone=True), nullable=False)
    """Review end time"""

    reviewed = mapped_column(db.Boolean, default=False)
    """Review status"""

    points = mapped_column(db.Float)
    """Points given by the reviewer"""

    comment = mapped_column(db.Text)
    """Review comment"""

    __table_args__ = (
        db.UniqueConstraint("answer_id", "block_id", "reviewer_id", "reviewable_id"),
        db.UniqueConstraint("task_name", "block_id", "reviewer_id", "reviewable_id"),
    )

    reviewer = db.relationship("User", foreign_keys=[reviewer_id])
    reviewable = db.relationship("User", foreign_keys=[reviewable_id])

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
