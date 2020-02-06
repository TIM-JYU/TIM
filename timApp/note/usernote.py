from typing import Optional

from sqlalchemy import func

from timApp.timdb.sqa import db


class UserNote(db.Model):
    """A comment/note that has been posted in a document paragraph."""
    __tablename__ = 'usernotes'
    id = db.Column(db.Integer, primary_key=True)
    """Comment id."""

    usergroup_id = db.Column(db.Integer, db.ForeignKey('usergroup.id'), nullable=False)
    """The UserGroup id who posted the comment."""

    doc_id = db.Column(db.Integer, db.ForeignKey('block.id'), nullable=False)
    """The document id in which this comment was posted."""

    par_id = db.Column(db.Text, nullable=False)
    """The paragraph id in which this comment was posted."""

    par_hash = db.Column(db.Text, nullable=False)
    """The paragraph hash at the time this comment was posted."""

    content = db.Column(db.Text, nullable=False)
    """Comment content."""

    created = db.Column(db.DateTime(timezone=True), nullable=False, default=func.now())
    """Comment creation timestamp."""

    modified = db.Column(db.DateTime(timezone=True))
    """Comment modification timestamp."""

    access = db.Column(db.Text, nullable=False)
    """Who can see this comment. So far valid values are 'everyone' and 'justme'."""

    tags = db.Column(db.Text, nullable=False)
    """Tags for the comment."""

    html = db.Column(db.Text)
    """Comment HTML cache."""

    usergroup = db.relationship('UserGroup', back_populates='notes')

    def to_json(self):
        return {
            'id': self.id,
            'doc_id': self.doc_id,
            'par_id': self.par_id,
            'par_hash': self.par_hash,
            'content': self.content,
            'created': self.created,
            'modified': self.modified,
            'access': self.access,
            'usergroup': self.usergroup,
        }


def get_comment_by_id(c_id: int) -> Optional[UserNote]:
    return UserNote.query.get(c_id)
