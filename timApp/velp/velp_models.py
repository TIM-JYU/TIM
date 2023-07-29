"""Defines all data models related to velps."""
from datetime import datetime
from typing import Optional, TYPE_CHECKING, Dict, List

from sqlalchemy import ForeignKey
from sqlalchemy.orm import mapped_column, Mapped, relationship, attribute_keyed_dict
from sqlalchemy.orm.collections import attribute_mapped_collection  # type: ignore

from timApp.item.block import Block
from timApp.timdb.types import datetime_tz, DbModel

if TYPE_CHECKING:
    from timApp.user.user import User


class VelpContent(DbModel):
    """The actual content of a Velp."""

    version_id: Mapped[int] = mapped_column(
        ForeignKey("velpversion.id"), primary_key=True
    )
    language_id: Mapped[str] = mapped_column(primary_key=True)
    content: Mapped[Optional[str]]
    default_comment: Mapped[Optional[str]]

    velp_version: Mapped["VelpVersion"] = relationship()


class AnnotationComment(DbModel):
    """A comment in an Annotation."""

    id: Mapped[int] = mapped_column(primary_key=True)
    """Comment identifier."""

    annotation_id: Mapped[int] = mapped_column(ForeignKey("annotation.id"))
    """Annotation id."""

    comment_time: Mapped[datetime_tz] = mapped_column(default=datetime.utcnow)
    """Comment timestamp."""

    commenter_id: Mapped[int] = mapped_column(ForeignKey("useraccount.id"))
    """Commenter user id."""

    content: Mapped[Optional[str]]
    """Comment text."""

    commenter: Mapped["User"] = relationship()

    def to_json(self) -> dict:
        return {
            "annotation_id": self.annotation_id,
            "comment_time": self.comment_time,
            "commenter": self.commenter,
            "content": self.content,
            "id": self.id,
        }


class LabelInVelp(DbModel):
    """Associates VelpLabels with Velps."""

    label_id: Mapped[int] = mapped_column(ForeignKey("velplabel.id"), primary_key=True)
    velp_id: Mapped[int] = mapped_column(ForeignKey("velp.id"), primary_key=True)


class VelpInGroup(DbModel):
    velp_group_id: Mapped[int] = mapped_column(
        ForeignKey("velpgroup.id"), primary_key=True
    )
    velp_id: Mapped[int] = mapped_column(ForeignKey("velp.id"), primary_key=True)


class Velp(DbModel):
    """A Velp is a kind of category for Annotations and is visually represented by a Post-it note."""

    id: Mapped[int] = mapped_column(primary_key=True)
    creator_id: Mapped[int] = mapped_column(ForeignKey("useraccount.id"))
    creation_time: Mapped[datetime_tz] = mapped_column(default=datetime.utcnow)
    default_points: Mapped[Optional[float]]
    valid_from: Mapped[Optional[datetime_tz]] = mapped_column(default=datetime.utcnow)
    valid_until: Mapped[Optional[datetime_tz]]
    color: Mapped[Optional[str]]
    visible_to: Mapped[int]
    style: Mapped[Optional[int]]

    creator: Mapped["User"] = relationship(back_populates="velps")
    labels: Mapped[Dict[int, "VelpLabel"]] = relationship(
        back_populates="velps",
        secondary=LabelInVelp.__table__,
        collection_class=attribute_keyed_dict("id"),
    )
    groups: Mapped[Dict[int, "VelpGroup"]] = relationship(
        back_populates="velps",
        secondary=VelpInGroup.__table__,
        collection_class=attribute_mapped_collection("id"),
        cascade="all",
    )
    velp_versions: Mapped[List["VelpVersion"]] = relationship(
        order_by="VelpVersion.id.desc()"
    )

    def to_json(self) -> dict:
        vv = self.velp_versions[0]
        vc = vv.content[0]
        return {
            "color": self.color,
            "content": vc.content,
            "default_comment": vc.default_comment,
            "id": self.id,
            "labels": [lbl.id for lbl in self.labels.values()],
            "language_id": vc.language_id,
            "points": self.default_points,
            "valid_until": self.valid_until,
            "velp_groups": [vg.id for vg in self.groups.values()],
            "visible_to": self.visible_to,
            "style": self.style,
        }


class VelpGroup(DbModel):
    """Represents a group of Velps."""

    id: Mapped[int] = mapped_column(ForeignKey("block.id"), primary_key=True)
    name: Mapped[Optional[str]]
    creation_time: Mapped[datetime_tz] = mapped_column(default=datetime.utcnow)
    valid_from: Mapped[Optional[datetime_tz]] = mapped_column(default=datetime.utcnow)
    valid_until: Mapped[Optional[datetime_tz]]
    default_group: Mapped[Optional[bool]] = mapped_column(default=False)

    velps: Mapped[Dict[int, "Velp"]] = relationship(
        back_populates="groups",
        secondary=VelpInGroup.__table__,
        collection_class=attribute_keyed_dict("id"),
        cascade="all",
    )
    block: Mapped["Block"] = relationship(lazy="joined")

    def to_json(self) -> dict:
        return {
            "id": self.id,
            "name": self.name,
            "location": self.block.docentries[0].name,
        }


class VelpGroupDefaults(DbModel):
    doc_id: Mapped[int] = mapped_column(ForeignKey("block.id"), primary_key=True)
    target_id: Mapped[str] = mapped_column(primary_key=True)
    velp_group_id: Mapped[int] = mapped_column(
        ForeignKey("velpgroup.id"), primary_key=True
    )
    target_type: Mapped[int]  # 0 = document, 1 = paragraph, 2 = area
    selected: Mapped[Optional[bool]] = mapped_column(default=False)


class VelpGroupLabel(DbModel):
    """Currently not used (0 rows in production DB as of 5th July 2018)."""

    id: Mapped[int] = mapped_column(primary_key=True)
    content: Mapped[str]


class VelpGroupSelection(DbModel):
    user_id: Mapped[int] = mapped_column(ForeignKey("useraccount.id"), primary_key=True)
    doc_id: Mapped[int] = mapped_column(ForeignKey("block.id"), primary_key=True)
    target_id: Mapped[str] = mapped_column(primary_key=True)
    target_type: Mapped[int]  # 0 = document, 1 = paragraph, 2 = area
    selected: Mapped[Optional[bool]] = mapped_column(default=False)
    velp_group_id: Mapped[int] = mapped_column(
        ForeignKey("velpgroup.id"), primary_key=True
    )


class VelpGroupsInDocument(DbModel):
    """

    TODO: This table contains lots of rows in production DB (about 19000 as of 5th July 2018).
    TODO: Possibly needs some optimizations.
    """

    user_id: Mapped[int] = mapped_column(ForeignKey("useraccount.id"), primary_key=True)
    doc_id: Mapped[int] = mapped_column(ForeignKey("block.id"), primary_key=True)
    velp_group_id: Mapped[int] = mapped_column(
        ForeignKey("velpgroup.id"), primary_key=True
    )


class VelpLabel(DbModel):
    """A label that can be assigned to a Velp."""

    id: Mapped[int] = mapped_column(primary_key=True)
    # TODO make not optional
    creator_id: Mapped[Optional[int]] = mapped_column(ForeignKey("useraccount.id"))

    creator: Mapped[Optional["User"]] = relationship()
    velps: Mapped[Dict[int, "Velp"]] = relationship(
        back_populates="labels",
        secondary=LabelInVelp.__table__,
        collection_class=attribute_keyed_dict("id"),
    )


class VelpLabelContent(DbModel):
    velplabel_id: Mapped[int] = mapped_column(
        ForeignKey("velplabel.id"), primary_key=True
    )
    language_id: Mapped[str] = mapped_column(primary_key=True)
    content: Mapped[Optional[str]]

    velplabel: Mapped["VelpLabel"] = relationship()

    def to_json(self) -> dict:
        return {
            "id": self.velplabel_id,
            "language_id": self.language_id,
            "content": self.content,
        }


class VelpVersion(DbModel):
    id: Mapped[int] = mapped_column(primary_key=True)
    velp_id: Mapped[int] = mapped_column(ForeignKey("velp.id"))
    modify_time: Mapped[datetime_tz] = mapped_column(default=datetime.utcnow)

    velp: Mapped["Velp"] = relationship("Velp", overlaps="velp_versions")
    content: Mapped[List["VelpContent"]] = relationship(overlaps="velp_version")
