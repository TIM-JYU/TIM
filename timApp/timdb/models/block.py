import datetime
from datetime import timezone

from timdb.tim_models import db


class Block(db.Model):
    __bind_key__ = 'tim_main'
    __tablename__ = 'block'
    id = db.Column(db.Integer, primary_key=True)
    latest_revision_id = db.Column(db.Integer)
    type_id = db.Column(db.Integer, nullable=False)
    description = db.Column(db.Text)
    created = db.Column(db.DateTime(timezone=True), nullable=False)
    modified = db.Column(db.DateTime(timezone=True))
    usergroup_id = db.Column(db.Integer, db.ForeignKey('usergroup.id'), nullable=False)

    owner = db.relationship('UserGroup', backref=db.backref('owned_blocks', lazy='dynamic'))

    def __init__(self, type_id, usergroup_id, description=None):
        self.type_id = type_id
        self.usergroup_id = usergroup_id
        self.description = description
        self.created = datetime.datetime.now(timezone.utc)
        self.modified = self.created

    @property
    def parent(self) -> 'Folder':
        if self.type_id == 0:
            from timdb.models.docentry import DocEntry
            return DocEntry.query.filter_by(id=self.id, public=True).first().parent
        elif self.type_id == 6:
            from timdb.models.folder import Folder
            folder = Folder.get_by_id(self.id)
            return folder.parent

    def is_unpublished(self):
        from routes.accesshelper import has_ownership
        return has_ownership(self.id) and not self.owner.is_large() and self.accesses.count() == 0
