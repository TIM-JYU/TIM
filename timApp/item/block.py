from __future__ import annotations

from enum import Enum
from typing import TYPE_CHECKING

from sqlalchemy import func
from sqlalchemy.orm.collections import attribute_mapped_collection

from timApp.auth.accesstype import AccessType
from timApp.auth.auth_models import BlockAccess
from timApp.item.blockassociation import BlockAssociation
from timApp.item.tag import Tag
from timApp.messaging.messagelist.messagelist_models import MessageListModel
from timApp.messaging.timMessage.internalmessage_models import (
    InternalMessage,
    InternalMessageDisplay,
)
from timApp.timdb.sqa import db
from timApp.user.usergroup import UserGroup
from timApp.user.usergroupdoc import UserGroupDoc
from timApp.util.utils import get_current_time

if TYPE_CHECKING:
    from timApp.folder.folder import Folder


class Block(db.Model):
    """The "base class" for all database objects that are part of the permission system."""

    __tablename__ = "block"
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

    docentries = db.relationship("DocEntry", back_populates="_block")
    folder = db.relationship("Folder", back_populates="_block", uselist=False)
    translation = db.relationship(
        "Translation",
        back_populates="_block",
        uselist=False,
        foreign_keys="Translation.doc_id",
    )
    answerupload = db.relationship(
        "AnswerUpload", back_populates="block", lazy="dynamic"
    )
    accesses = db.relationship(
        "BlockAccess",
        back_populates="block",
        lazy="joined",
        cascade="all, delete-orphan",
        collection_class=attribute_mapped_collection("block_collection_key"),
    )
    tags: list[Tag] = db.relationship("Tag", back_populates="block", lazy="select")
    children = db.relationship(
        "Block",
        secondary=BlockAssociation.__table__,
        primaryjoin=id == BlockAssociation.__table__.c.parent,
        secondaryjoin=id == BlockAssociation.__table__.c.child,
        lazy="select",
    )
    parents = db.relationship(
        "Block",
        secondary=BlockAssociation.__table__,
        primaryjoin=id == BlockAssociation.__table__.c.child,
        secondaryjoin=id == BlockAssociation.__table__.c.parent,
        lazy="select",
        overlaps="children",
    )
    notifications = db.relationship(
        "Notification", back_populates="block", lazy="dynamic"
    )

    relevance = db.relationship(
        "BlockRelevance", back_populates="_block", uselist=False
    )

    # If this Block corresponds to a group's manage document, indicates the group being managed.
    managed_usergroup: UserGroup | None = db.relationship(
        "UserGroup",
        secondary=UserGroupDoc.__table__,
        lazy="select",
        uselist=False,
        overlaps="admin_doc",
    )

    #  If this Block corresponds to a message list's manage document, indicates the message list
    #  being managed.
    managed_messagelist: MessageListModel | None = db.relationship(
        "MessageListModel", back_populates="block", lazy="select"
    )

    internalmessage: InternalMessage | None = db.relationship(
        "InternalMessage", back_populates="block"
    )
    internalmessage_display: InternalMessageDisplay | None = db.relationship(
        "InternalMessageDisplay", back_populates="display_block"
    )

    def __json__(self):
        return ["id", "type_id", "description", "created", "modified"]

    @property
    def owners(self) -> list[UserGroup]:
        return [o.usergroup for o in self.owner_accesses]

    @property
    def parent(self) -> Folder:
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
        return (
            u.has_ownership(self) is not None
            and all(not o.is_large() for o in self.owners)
            and len(self.accesses) <= 1
        )

    @property
    def owner_accesses(self):
        return [a for a in self.accesses.values() if a.type == AccessType.owner.value]

    def set_owner(self, usergroup: UserGroup):
        """Changes the owner group for a block.

        :param usergroup: The new usergroup.

        """
        self.accesses = {
            (usergroup.id, AccessType.owner.value): BlockAccess(
                usergroup_id=usergroup.id,
                type=AccessType.owner.value,
                accessible_from=get_current_time(),
            )
        }

    def add_rights(self, groups, access_type: AccessType):
        for gr in groups:
            key = (gr.id, access_type.value)
            self.accesses[key] = BlockAccess(
                usergroup_id=gr.id,
                type=access_type.value,
                accessible_from=get_current_time(),
            )


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
    ScheduledFunction = 9
    Task = 10

    @staticmethod
    def from_str(type_name: str) -> BlockType:
        return BlockType[type_name.title()]


def insert_block(
    block_type: BlockType,
    description: str | None,
    owner_groups: list[UserGroup] | None = None,
) -> Block:
    """Inserts a block to database.

    :param description: The name (description) of the block.
    :param owner_groups: The owner groups of the block.
    :param block_type: The type of the block.
    :returns: The id of the block.

    """
    b = Block(description=description, type_id=block_type.value)
    db.session.add(b)
    if owner_groups:
        for owner_group in owner_groups:
            db.session.flush()
            access = BlockAccess(
                block=b,
                usergroup=owner_group,
                type=AccessType.owner.value,
                accessible_from=get_current_time(),
            )
            b.accesses[(owner_group.id, AccessType.owner.value)] = access
            # Also register to accesses_alt because it may be used by other methods in the same session
            owner_group.accesses_alt[(b.id, AccessType.owner.value)] = access
        db.session.flush()
    return b


def copy_default_rights(
    item, item_type: BlockType, owners_to_skip: list[UserGroup] | None = None
):
    from timApp.user.userutils import grant_access
    from timApp.user.users import get_default_rights_holders

    default_rights: list[BlockAccess] = []
    folder = item.parent
    while folder is not None:
        default_rights += get_default_rights_holders(folder, item_type)
        folder = folder.parent
    refreshed_groups = set()
    for d in default_rights:
        if (
            owners_to_skip
            and d.usergroup in owners_to_skip
            and d.access_type == AccessType.owner
        ):
            continue
        # Because copy_default_rights is usually applied to a new item with new permissions, the usergroup.accesses_alt
        # might not be updated yet. Because of this, we need to force the update of the accesses_alt at least once.
        if d.usergroup.id not in refreshed_groups:
            db.session.expire(d.usergroup, ["accesses_alt"])
            refreshed_groups.add(d.usergroup.id)
        grant_access(
            d.usergroup,
            item,
            d.atype.to_enum(),
            accessible_from=d.accessible_from,
            accessible_to=d.accessible_to,
            duration_from=d.duration_from,
            duration_to=d.duration_to,
            duration=d.duration,
        )
