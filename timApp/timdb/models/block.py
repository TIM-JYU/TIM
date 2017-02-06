from sqlalchemy import func

from timdb.accesstype import AccessType
from timdb.models.usergroup import UserGroup
from timdb.tim_models import db, BlockAccess


class Block(db.Model):
    __bind_key__ = 'tim_main'
    __tablename__ = 'block'
    id = db.Column(db.Integer, primary_key=True)
    latest_revision_id = db.Column(db.Integer)
    type_id = db.Column(db.Integer, nullable=False)
    description = db.Column(db.Text)
    created = db.Column(db.DateTime(timezone=True), nullable=False, default=func.now())
    modified = db.Column(db.DateTime(timezone=True), default=func.now())

    @property
    def owner(self) -> UserGroup:
        return UserGroup.query.filter(UserGroup.id.in_(
            BlockAccess.query.filter_by(block_id=self.id, type=AccessType.owner.value).with_entities(
                BlockAccess.usergroup_id))).first()

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
        from accesshelper import has_ownership
        return has_ownership(self.id) is not None and (not self.owner or not self.owner.is_large()) and self.accesses.count() <= 1

    @property
    def owner_access(self):
        return BlockAccess.query.filter_by(block_id=self.id, type=AccessType.owner.value).first()
