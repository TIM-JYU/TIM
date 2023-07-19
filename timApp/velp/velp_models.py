"""Defines all data models related to velps."""
from datetime import datetime

from sqlalchemy.orm.collections import attribute_mapped_collection  # type: ignore

from timApp.item.block import Block
from timApp.timdb.sqa import db


class VelpContent(db.Model):
    """The actual content of a Velp."""

    __tablename__ = "velpcontent"
    version_id = db.Column(
        db.Integer, db.ForeignKey("velpversion.id"), primary_key=True
    )
    language_id = db.Column(db.Text, primary_key=True)
    content = db.Column(db.Text)
    default_comment = db.Column(db.Text)

    velp_version = db.relationship("VelpVersion")


class AnnotationComment(db.Model):
    """A comment in an Annotation."""

    __tablename__ = "annotationcomment"
    id = db.Column(db.Integer, primary_key=True)
    """Comment identifier."""

    annotation_id = db.Column(
        db.Integer, db.ForeignKey("annotation.id"), nullable=False
    )
    """Annotation id."""

    comment_time = db.Column(
        db.DateTime(timezone=True), nullable=False, default=datetime.utcnow
    )
    """Comment timestamp."""

    commenter_id = db.Column(
        db.Integer, db.ForeignKey("useraccount.id"), nullable=False
    )
    """Commenter user id."""

    content = db.Column(db.Text)
    """Comment text."""

    commenter = db.relationship("User")

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
    label_id = db.Column(db.Integer, db.ForeignKey("velplabel.id"), primary_key=True)
    velp_id = db.Column(db.Integer, db.ForeignKey("velp.id"), primary_key=True)


class VelpInGroup(db.Model):
    __tablename__ = "velpingroup"
    velp_group_id = db.Column(
        db.Integer, db.ForeignKey("velpgroup.id"), primary_key=True
    )
    velp_id = db.Column(db.Integer, db.ForeignKey("velp.id"), primary_key=True)


class Velp(db.Model):
    """A Velp is a kind of category for Annotations and is visually represented by a Post-it note."""

    __tablename__ = "velp"
    id = db.Column(db.Integer, primary_key=True)
    creator_id = db.Column(db.Integer, db.ForeignKey("useraccount.id"), nullable=False)
    creation_time = db.Column(
        db.DateTime(timezone=True), nullable=False, default=datetime.utcnow
    )
    default_points = db.Column(db.Float)
    valid_from = db.Column(db.DateTime(timezone=True), default=datetime.utcnow)
    valid_until = db.Column(db.DateTime(timezone=True))
    color = db.Column(db.Text)
    visible_to = db.Column(db.Integer, nullable=False)
    style = db.Column(db.Integer)

    creator = db.relationship("User", back_populates="velps")
    labels = db.relationship(
        "VelpLabel",
        back_populates="velps",
        secondary=LabelInVelp.__table__,
        collection_class=attribute_mapped_collection("id"),
    )
    groups = db.relationship(
        "VelpGroup",
        back_populates="velps",
        secondary=VelpInGroup.__table__,
        collection_class=attribute_mapped_collection("id"),
        cascade="all",
    )
    velp_versions: list["VelpVersion"] = db.relationship(
        "VelpVersion", order_by="VelpVersion.id.desc()"
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
    id = db.Column(db.Integer, db.ForeignKey("block.id"), primary_key=True)
    name = db.Column(db.Text)
    creation_time = db.Column(
        db.DateTime(timezone=True), nullable=False, default=datetime.utcnow
    )
    valid_from = db.Column(db.DateTime(timezone=True), default=datetime.utcnow)
    valid_until = db.Column(db.DateTime(timezone=True))
    default_group = db.Column(db.Boolean, default=False)

    velps = db.relationship(
        "Velp",
        back_populates="groups",
        secondary=VelpInGroup.__table__,
        collection_class=attribute_mapped_collection("id"),
        cascade="all",
    )
    block: Block = db.relationship(
        "Block",
        lazy="selectin",
    )
    # docentry = db.relationship(
    #     'DocEntry',
    # )

    def to_json(self) -> dict:
        return {
            "id": self.id,
            "name": self.name,
            "location": self.block.docentries[0].name,
        }


class VelpGroupDefaults(db.Model):
    __tablename__ = "velpgroupdefaults"
    doc_id = db.Column(db.Integer, db.ForeignKey("block.id"), primary_key=True)
    target_type = db.Column(
        db.Integer, nullable=False
    )  # 0 = document, 1 = paragraph, 2 = area
    target_id = db.Column(db.Text, primary_key=True)
    velp_group_id = db.Column(
        db.Integer, db.ForeignKey("velpgroup.id"), primary_key=True
    )
    selected = db.Column(db.Boolean, default=False)


class VelpGroupLabel(db.Model):
    """Currently not used (0 rows in production DB as of 5th July 2018)."""

    __tablename__ = "velpgrouplabel"
    id = db.Column(db.Integer, primary_key=True)
    content = db.Column(db.Text, nullable=False)


class VelpGroupSelection(db.Model):
    __tablename__ = "velpgroupselection"
    user_id = db.Column(db.Integer, db.ForeignKey("useraccount.id"), primary_key=True)
    doc_id = db.Column(db.Integer, db.ForeignKey("block.id"), primary_key=True)
    target_type = db.Column(
        db.Integer, nullable=False
    )  # 0 = document, 1 = paragraph, 2 = area
    target_id = db.Column(db.Text, primary_key=True)
    selected = db.Column(db.Boolean, default=False)
    velp_group_id = db.Column(
        db.Integer, db.ForeignKey("velpgroup.id"), primary_key=True
    )


class VelpGroupsInDocument(db.Model):
    """

    TODO: This table contains lots of rows in production DB (about 19000 as of 5th July 2018).
    TODO: Possibly needs some optimizations.
    """

    __tablename__ = "velpgroupsindocument"
    user_id = db.Column(db.Integer, db.ForeignKey("useraccount.id"), primary_key=True)
    doc_id = db.Column(db.Integer, db.ForeignKey("block.id"), primary_key=True)
    velp_group_id = db.Column(
        db.Integer, db.ForeignKey("velpgroup.id"), primary_key=True
    )


class VelpLabel(db.Model):
    """A label that can be assigned to a Velp."""

    __tablename__ = "velplabel"
    id = db.Column(db.Integer, primary_key=True)
    # TODO make not nullable
    creator_id = db.Column(db.Integer, db.ForeignKey("useraccount.id"), nullable=True)

    creator = db.relationship("User")
    velps = db.relationship(
        "Velp",
        back_populates="labels",
        secondary=LabelInVelp.__table__,
        collection_class=attribute_mapped_collection("id"),
    )


class VelpLabelContent(db.Model):
    __tablename__ = "velplabelcontent"
    velplabel_id = db.Column(
        db.Integer, db.ForeignKey("velplabel.id"), primary_key=True
    )
    language_id = db.Column(db.Text, primary_key=True)
    content = db.Column(db.Text)

    velplabel = db.relationship("VelpLabel")

    def to_json(self) -> dict:
        return {
            "id": self.velplabel_id,
            "language_id": self.language_id,
            "content": self.content,
        }


class VelpVersion(db.Model):
    __tablename__ = "velpversion"
    id = db.Column(db.Integer, primary_key=True)
    velp_id = db.Column(db.Integer, db.ForeignKey("velp.id"), nullable=False)
    modify_time = db.Column(
        db.DateTime(timezone=True), nullable=False, default=datetime.utcnow
    )

    velp: Velp = db.relationship("Velp", overlaps="velp_versions")
    content: list[VelpContent] = db.relationship("VelpContent", overlaps="velp_version")
