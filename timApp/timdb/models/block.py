from datetime import datetime, timezone
from typing import Optional

from sqlalchemy import func

from timApp.timdb.accesstype import AccessType
from timApp.timdb.blocktypes import blocktypes
from timApp.timdb.models.usergroup import UserGroup
from timApp.timdb.tim_models import db, BlockAccess
from timApp.types import FolderType


class Block(db.Model):
    __bind_key__ = 'tim_main'
    __tablename__ = 'block'
    id = db.Column(db.Integer, primary_key=True)
    latest_revision_id = db.Column(db.Integer)
    type_id = db.Column(db.Integer, nullable=False)
    description = db.Column(db.Text)
    created = db.Column(db.DateTime(timezone=True), nullable=False, default=func.now())
    modified = db.Column(db.DateTime(timezone=True), default=func.now())

    docentries = db.relationship('DocEntry', back_populates='_block', lazy='dynamic')
    folder = db.relationship('Folder', back_populates='_block', lazy='dynamic')
    answerupload = db.relationship('AnswerUpload', back_populates='block', lazy='dynamic')
    accesses = db.relationship('BlockAccess', back_populates='block', lazy='dynamic')

    def __json__(self):
        return ['id', 'type_id', 'description', 'created', 'modified']

    @property
    def owner(self) -> Optional[UserGroup]:
        owner_access = self.owner_access
        if not owner_access:
            return None
        return owner_access.usergroup

    @property
    def parent(self) -> FolderType:
        if self.type_id == blocktypes.DOCUMENT:
            from timApp.timdb.models.docentry import DocEntry
            return DocEntry.find_by_id(self.id, try_translation=True).parent
        elif self.type_id == blocktypes.FOLDER:
            from timApp.timdb.models.folder import Folder
            folder = Folder.get_by_id(self.id)
            return folder.parent

    def is_unpublished(self):
        from timApp.accesshelper import has_ownership
        return has_ownership(self.id) is not None and (
        not self.owner or not self.owner.is_large()) and self.accesses.count() <= 1

    @property
    def owner_access(self):
        return self.accesses.filter_by(type=AccessType.owner.value).first()

    def set_owner(self, usergroup_id: int):
        """Changes the owner group for a block.

        :param usergroup_id: The id of the new usergroup.

        """
        self.accesses.filter_by(type=AccessType.owner.value).delete()
        self.accesses.append(
            BlockAccess(usergroup_id=usergroup_id,
                        type=AccessType.owner.value,
                        accessible_from=datetime.now(tz=timezone.utc)))
