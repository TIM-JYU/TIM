from datetime import datetime, timezone
from enum import Enum
from typing import Optional

from sqlalchemy import func

from timApp.auth.accesstype import AccessType
from timApp.item.blockassociation import BlockAssociation
from timApp.user.usergroup import UserGroup
from timApp.timdb.sqa import db
from timApp.auth.auth_models import BlockAccess
from timApp.timtypes import FolderType


class Block(db.Model):
    """The "base class" for all database objects that are part of the permission system."""
    __bind_key__ = 'tim_main'
    __tablename__ = 'block'
    id = db.Column(db.Integer, primary_key=True)
    """A unique identifier for the Block."""

    latest_revision_id = db.Column(db.Integer)
    """Old field that is not used anymore."""

    type_id = db.Column(db.Integer, nullable=False)
    """Type of the Block, see BlockType enum for possible types."""

    description = db.Column(db.Text)
    """Additional information about the Block. This is used for different purposes by different BlockTypes,
    so it isn't merely a "description".
    """

    created = db.Column(db.DateTime(timezone=True), nullable=False, default=func.now())
    """When this Block was created."""

    modified = db.Column(db.DateTime(timezone=True), default=func.now())
    """When this Block was last modified."""

    docentries = db.relationship('DocEntry', back_populates='_block')
    folder = db.relationship('Folder', back_populates='_block', uselist=False)
    translation = db.relationship('Translation', back_populates='_block', uselist=False, foreign_keys="Translation.doc_id")
    answerupload = db.relationship('AnswerUpload', back_populates='block', lazy='dynamic')
    accesses = db.relationship('BlockAccess', back_populates='block', lazy='joined')
    tags = db.relationship('Tag', back_populates='block', lazy='select')
    children = db.relationship('Block',
                               secondary=BlockAssociation.__table__,
                               primaryjoin=id == BlockAssociation.__table__.c.parent,
                               secondaryjoin=id == BlockAssociation.__table__.c.child,
                               lazy='select')
    parents = db.relationship('Block',
                              secondary=BlockAssociation.__table__,
                              primaryjoin=id == BlockAssociation.__table__.c.child,
                              secondaryjoin=id == BlockAssociation.__table__.c.parent,
                              lazy='select')
    notifications = db.relationship('Notification', back_populates='block', lazy='dynamic')

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
        if self.type_id == BlockType.Document.value:
            from timApp.document.docentry import DocEntry
            return DocEntry.find_by_id(self.id).parent
        elif self.type_id == BlockType.Folder.value:
            from timApp.folder.folder import Folder
            folder = Folder.get_by_id(self.id)
            return folder.parent

    def is_unpublished(self):
        from timApp.auth.sessioninfo import get_current_user_object
        u = get_current_user_object()
        return u.has_ownership(self) is not None and (
                not self.owner or not self.owner.is_large()) and len(self.accesses) <= 1

    @property
    def owner_access(self):
        for a in self.accesses:
            if a.type == AccessType.owner.value:
                return a
        return None

    def set_owner(self, usergroup: UserGroup):
        """Changes the owner group for a block.

        :param usergroup: The new usergroup.

        """
        for a in self.accesses:
            if a.type == AccessType.owner.value:
                db.session.delete(a)
        self.accesses.append(
            BlockAccess(usergroup_id=usergroup.id,
                        type=AccessType.owner.value,
                        accessible_from=datetime.now(tz=timezone.utc)))


class BlockType(Enum):
    Document = 0
    Comment = 1
    Note = 2
    Answer = 3
    Image = 4
    Reading = 5
    Folder = 6
    File = 7
    Upload = 8
    Velpgroup = 9
    Annotation = 10

    @staticmethod
    def from_str(type_name: str) -> 'BlockType':
        return BlockType[type_name.title()]


def insert_block(block_type: BlockType, description: Optional[str], owner_group_id: Optional[int] = None) -> Block:
    """Inserts a block to database.

    :param description: The name (description) of the block.
    :param owner_group_id: The owner group of the block.
    :param block_type: The type of the block.
    :returns: The id of the block.

    """
    b = Block(description=description, type_id=block_type.value)
    if owner_group_id is not None:
        access = BlockAccess(block=b,
                             usergroup_id=owner_group_id,
                             type=AccessType.owner.value,
                             accessible_from=datetime.now(tz=timezone.utc))
        db.session.add(access)
    db.session.add(b)
    db.session.flush()
    assert b.id != 0
    return b


def copy_default_rights(item_id: int, item_type: BlockType):
    from timApp.timdb.dbaccess import get_timdb
    from timApp.user.userutils import grant_access
    timdb = get_timdb()
    default_rights = []
    folder = Block.query.get(item_id).parent
    while folder is not None:
        default_rights += timdb.users.get_default_rights_holders(folder.id, item_type)
        folder = folder.parent
    for d in default_rights:
        grant_access(d['gid'],
                     item_id,
                     d['access_name'],
                     commit=False,
                     accessible_from=d['accessible_from'],
                     accessible_to=d['accessible_to'],
                     duration_from=d['duration_from'],
                     duration_to=d['duration_to'],
                     duration=d['duration'])
