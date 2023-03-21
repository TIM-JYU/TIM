from typing import Any

from timApp.timdb.sqa import db


class PeerReview(db.Model):
    """A peer review to a task."""

    __tablename__ = "peer_review"
    id = db.Column(db.Integer, primary_key=True)
    """Review identifier."""

    answer_id = db.Column(db.Integer, db.ForeignKey("answer.id"), nullable=True)
    """Answer id."""

    task_name = db.Column(db.Text, nullable=True)
    """Task name"""

    block_id = db.Column(db.Integer, db.ForeignKey("block.id"), nullable=False)
    """Doc id"""

    reviewer_id = db.Column(db.Integer, db.ForeignKey("useraccount.id"), nullable=False)
    """Reviewer id"""

    reviewable_id = db.Column(
        db.Integer, db.ForeignKey("useraccount.id"), nullable=False
    )
    """Reviewable id"""

    start_time = db.Column(db.DateTime(timezone=True), nullable=False)
    """Review start time"""

    end_time = db.Column(db.DateTime(timezone=True), nullable=False)
    """Review end time"""

    reviewed = db.Column(db.Boolean, default=False)
    """Review status"""

    points = db.Column(db.Float)
    """Points given by the reviewer"""

    comment = db.Column(db.Text)
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
            "reviewer_id": self.reviewer_id,
            "reviewer": self.reviewer,
            "reviewable_id": self.reviewable_id,
            "reviewable": self.reviewable,
            "start_time": self.start_time,
            "end_time": self.end_time,
            "reviewed": self.reviewed,
            "points": self.points,
            "comment": self.comment,
        }
