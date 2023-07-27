"""Defines all data models related to velps."""
from datetime import datetime
from typing import Optional, TYPE_CHECKING, Dict, List

from sqlalchemy.orm import mapped_column, Mapped, relationship, attribute_keyed_dict
from sqlalchemy.orm.collections import attribute_mapped_collection  # type: ignore

from timApp.item.block import Block
from timApp.timdb.sqa import db
from timApp.timdb.types import datetime_tz

if TYPE_CHECKING:
    from timApp.user.user import User


class VelpContent(db.Model):
    """The actual content of a Velp."""

    __tablename__ = "velpcontent"

    version_id: Mapped[int] = mapped_column(
        db.ForeignKey("velpversion.id"), primary_key=True
    )
    language_id: Mapped[str] = mapped_column(primary_key=True)
    content: Mapped[Optional[str]]
    default_comment: Mapped[Optional[str]]

    velp_version: Mapped["VelpVersion"] = relationship()


class AnnotationComment(db.Model):
    """A comment in an Annotation."""

    __tablename__ = "annotationcomment"

    id: Mapped[int] = mapped_column(primary_key=True)
    """Comment identifier."""

    annotation_id: Mapped[int] = mapped_column(db.ForeignKey("annotation.id"))
    """Annotation id."""

    comment_time: Mapped[datetime_tz] = mapped_column(default=datetime.utcnow)
    """Comment timestamp."""

    commenter_id: Mapped[int] = mapped_column(db.ForeignKey("useraccount.id"))
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


class LabelInVelp(db.Model):
    """Associates VelpLabels with Velps."""

    __tablename__ = "labelinvelp"

    label_id: Mapped[int] = mapped_column(
        db.ForeignKey("velplabel.id"), primary_key=True
    )
    velp_id: Mapped[int] = mapped_column(db.ForeignKey("velp.id"), primary_key=True)


class VelpInGroup(db.Model):
    __tablename__ = "velpingroup"

    velp_group_id: Mapped[int] = mapped_column(
        db.ForeignKey("velpgroup.id"), primary_key=True
    )
    velp_id: Mapped[int] = mapped_column(db.ForeignKey("velp.id"), primary_key=True)


class Velp(db.Model):
    """A Velp is a kind of category for Annotations and is visually represented by a Post-it note."""

    __tablename__ = "velp"

    id: Mapped[int] = mapped_column(primary_key=True)
    creator_id: Mapped[int] = mapped_column(db.ForeignKey("useraccount.id"))
    creation_time: Mapped[datetime_tz] = mapped_column(default=datetime.utcnow)
    default_points: Mapped[Optional[float]]
    valid_from: Mapped[datetime_tz] = mapped_column(default=datetime.utcnow)
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


class VelpGroup(db.Model):
    """Represents a group of Velps."""

    __tablename__ = "velpgroup"

    id: Mapped[int] = mapped_column(db.ForeignKey("block.id"), primary_key=True)
    name: Mapped[Optional[str]]
    creation_time: Mapped[datetime_tz] = mapped_column(default=datetime.utcnow)
    valid_from: Mapped[datetime_tz] = mapped_column(default=datetime.utcnow)
    valid_until: Mapped[Optional[datetime_tz]]
    default_group: Mapped[bool] = mapped_column(default=False)

    velps: Mapped[Dict[int, "Velp"]] = db.relationship(
        back_populates="groups",
        secondary=VelpInGroup.__table__,
        collection_class=attribute_keyed_dict("id"),
        cascade="all",
    )
    block: Mapped["Block"] = db.relationship(lazy="joined")

    def to_json(self) -> dict:
        return {
            "id": self.id,
            "name": self.name,
            "location": self.block.docentries[0].name,
        }


class VelpGroupDefaults(db.Model):
    __tablename__ = "velpgroupdefaults"

    doc_id: Mapped[int] = mapped_column(db.ForeignKey("block.id"), primary_key=True)
    target_id: Mapped[str] = mapped_column(primary_key=True)
    velp_group_id: Mapped[int] = mapped_column(
        db.ForeignKey("velpgroup.id"), primary_key=True
    )
    target_type: Mapped[int]  # 0 = document, 1 = paragraph, 2 = area
    selected: Mapped[bool] = mapped_column(default=False)


class VelpGroupLabel(db.Model):
    """Currently not used (0 rows in production DB as of 5th July 2018)."""

    __tablename__ = "velpgrouplabel"

    id: Mapped[int] = mapped_column(primary_key=True)
    content: Mapped[str]


class VelpGroupSelection(db.Model):
    __tablename__ = "velpgroupselection"

    user_id: Mapped[int] = mapped_column(
        db.ForeignKey("useraccount.id"), primary_key=True
    )
    doc_id: Mapped[int] = mapped_column(db.ForeignKey("block.id"), primary_key=True)
    target_id: Mapped[str] = mapped_column(primary_key=True)
    target_type: Mapped[int]  # 0 = document, 1 = paragraph, 2 = area
    selected: Mapped[bool] = mapped_column(default=False)
    velp_group_id: Mapped[int] = mapped_column(
        db.ForeignKey("velpgroup.id"), primary_key=True
    )


class VelpGroupsInDocument(db.Model):
    """

    TODO: This table contains lots of rows in production DB (about 19000 as of 5th July 2018).
    TODO: Possibly needs some optimizations.
    """

    __tablename__ = "velpgroupsindocument"

    user_id: Mapped[int] = mapped_column(
        db.ForeignKey("useraccount.id"), primary_key=True
    )
    doc_id: Mapped[int] = mapped_column(db.ForeignKey("block.id"), primary_key=True)
    velp_group_id: Mapped[int] = mapped_column(
        db.ForeignKey("velpgroup.id"), primary_key=True
    )


class VelpLabel(db.Model):
    """A label that can be assigned to a Velp."""

    __tablename__ = "velplabel"

    id: Mapped[int] = mapped_column(primary_key=True)
    # TODO make not optional
    creator_id: Mapped[Optional[int]] = mapped_column(db.ForeignKey("useraccount.id"))

    creator: Mapped[Optional["User"]] = relationship()
    velps: Mapped[Dict[int, "Velp"]] = relationship(
        back_populates="labels",
        secondary=LabelInVelp.__table__,
        collection_class=attribute_keyed_dict("id"),
    )


class VelpLabelContent(db.Model):
    __tablename__ = "velplabelcontent"

    velplabel_id: Mapped[int] = mapped_column(
        db.ForeignKey("velplabel.id"), primary_key=True
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


class VelpVersion(db.Model):
    __tablename__ = "velpversion"

    id: Mapped[int] = mapped_column(primary_key=True)
    velp_id: Mapped[int] = mapped_column(db.ForeignKey("velp.id"))
    modify_time: Mapped[datetime_tz] = mapped_column(default=datetime.utcnow)

    velp: Mapped["Velp"] = db.relationship("Velp", overlaps="velp_versions")
    content: Mapped[List["VelpContent"]] = db.relationship(overlaps="velp_version")
