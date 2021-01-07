from typing import Dict, Any

from timApp.timdb.sqa import db


class PeerReview(db.Model):
    """A peer review to a task."""
    __tablename__ = 'peer_review'
    id = db.Column(db.Integer, primary_key=True)
    """Review identifier."""

    answer_id = db.Column(db.Integer, db.ForeignKey('answer.id'))
    """Answer id."""

    task_name = db.Column(db.Text, nullable=False)
    """Task name"""

    block_id = db.Column(db.Integer, db.ForeignKey('block.id'), nullable=False)
    """Doc id"""

    reviewer_id = db.Column(db.Integer, db.ForeignKey('useraccount.id'), nullable=False)
    """Reviewer id"""

    reviewable_id = db.Column(db.Integer, db.ForeignKey('useraccount.id'), nullable=False)
    """Reviewable id"""

    start_time = db.Column(db.DateTime(timezone=True), nullable=False)
    """Review start time"""

    end_time = db.Column(db.DateTime(timezone=True), nullable=False)
    """Review end time"""

    reviewed = db.Column(db.Boolean, default=False)
    """Review status"""

    __table_args__ = (db.UniqueConstraint('answer_id', 'block_id', 'reviewer_id', 'reviewable_id'),
                      db.UniqueConstraint('task_name', 'block_id', 'reviewer_id', 'reviewable_id'))

    reviewer = db.relationship('User', foreign_keys=[reviewer_id])
    reviewable = db.relationship('User', foreign_keys=[reviewable_id])

    @property
    def to_json(self) -> Dict[str, Any]:
        return {
            'id': self.id,
            'answer_id': self.answer_id,
            'task_name': self.task_name,
            'block_id': self.block_id,
            'reviewer_id': self.reviewer_id,
            'reviewable_id': self.reviewable_id,
            'start_time': self.start_time,
            'end_time': self.end_time,
            'reviewed': self.reviewed
        }
