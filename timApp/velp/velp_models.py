"""Defines all data models related to velps."""
from datetime import datetime

from sqlalchemy.orm import mapped_column
from sqlalchemy.orm.collections import attribute_mapped_collection  # type: ignore

from timApp.timdb.sqa import db


class VelpContent(db.Model):
    """The actual content of a Velp."""

    __tablename__ = "velpcontent"
    

    version_id = mapped_column(
        db.Integer, db.ForeignKey("velpversion.id"), primary_key=True
    )
    language_id = mapped_column(db.Text, primary_key=True)
    content = mapped_column(db.Text)
    default_comment = mapped_column(db.Text)

    velp_version = db.relationship("VelpVersion")


class AnnotationComment(db.Model):
    """A comment in an Annotation."""

    __tablename__ = "annotationcomment"
    

    id = mapped_column(db.Integer, primary_key=True)
    """Comment identifier."""

    annotation_id = mapped_column(
        db.Integer, db.ForeignKey("annotation.id"), nullable=False
    )
    """Annotation id."""

    comment_time = mapped_column(
        db.DateTime(timezone=True), nullable=False, default=datetime.utcnow
    )
    """Comment timestamp."""

    commenter_id = mapped_column(
        db.Integer, db.ForeignKey("useraccount.id"), nullable=False
    )
    """Commenter user id."""

    content = mapped_column(db.Text)
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
    

    label_id = mapped_column(
        db.Integer, db.ForeignKey("velplabel.id"), primary_key=True
    )
    velp_id = mapped_column(db.Integer, db.ForeignKey("velp.id"), primary_key=True)


class VelpInGroup(db.Model):
    __tablename__ = "velpingroup"
    

    velp_group_id = mapped_column(
        db.Integer, db.ForeignKey("velpgroup.id"), primary_key=True
    )
    velp_id = mapped_column(db.Integer, db.ForeignKey("velp.id"), primary_key=True)


class Velp(db.Model):
    """A Velp is a kind of category for Annotations and is visually represented by a Post-it note."""

    __tablename__ = "velp"
    

    id = mapped_column(db.Integer, primary_key=True)
    creator_id = mapped_column(
        db.Integer, db.ForeignKey("useraccount.id"), nullable=False
    )
    creation_time = mapped_column(
        db.DateTime(timezone=True), nullable=False, default=datetime.utcnow
    )
    default_points = mapped_column(db.Float)
    valid_from = mapped_column(db.DateTime(timezone=True), default=datetime.utcnow)
    valid_until = mapped_column(db.DateTime(timezone=True))
    color = mapped_column(db.Text)
    visible_to = mapped_column(db.Integer, nullable=False)
    style = mapped_column(db.Integer)

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
    velp_versions = db.relationship(
        "VelpVersion", order_by="VelpVersion.id.desc()"
    ) # : list["VelpVersion"]

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
    

    id = mapped_column(db.Integer, db.ForeignKey("block.id"), primary_key=True)
    name = mapped_column(db.Text)
    creation_time = mapped_column(
        db.DateTime(timezone=True), nullable=False, default=datetime.utcnow
    )
    valid_from = mapped_column(db.DateTime(timezone=True), default=datetime.utcnow)
    valid_until = mapped_column(db.DateTime(timezone=True))
    default_group = mapped_column(db.Boolean, default=False)

    velps = db.relationship(
        "Velp",
        back_populates="groups",
        secondary=VelpInGroup.__table__,
        collection_class=attribute_mapped_collection("id"),
        cascade="all",
    )
    block = db.relationship(
        "Block",
        lazy="selectin",
    ) # : Block
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
    

    doc_id = mapped_column(db.Integer, db.ForeignKey("block.id"), primary_key=True)
    target_type = mapped_column(
        db.Integer, nullable=False
    )  # 0 = document, 1 = paragraph, 2 = area
    target_id = mapped_column(db.Text, primary_key=True)
    velp_group_id = mapped_column(
        db.Integer, db.ForeignKey("velpgroup.id"), primary_key=True
    )
    selected = mapped_column(db.Boolean, default=False)


class VelpGroupLabel(db.Model):
    """Currently not used (0 rows in production DB as of 5th July 2018)."""

    __tablename__ = "velpgrouplabel"
    

    id = mapped_column(db.Integer, primary_key=True)
    content = mapped_column(db.Text, nullable=False)


class VelpGroupSelection(db.Model):
    __tablename__ = "velpgroupselection"
    

    user_id = mapped_column(
        db.Integer, db.ForeignKey("useraccount.id"), primary_key=True
    )
    doc_id = mapped_column(db.Integer, db.ForeignKey("block.id"), primary_key=True)
    target_type = mapped_column(
        db.Integer, nullable=False
    )  # 0 = document, 1 = paragraph, 2 = area
    target_id = mapped_column(db.Text, primary_key=True)
    selected = mapped_column(db.Boolean, default=False)
    velp_group_id = mapped_column(
        db.Integer, db.ForeignKey("velpgroup.id"), primary_key=True
    )


class VelpGroupsInDocument(db.Model):
    """

    TODO: This table contains lots of rows in production DB (about 19000 as of 5th July 2018).
    TODO: Possibly needs some optimizations.
    """

    __tablename__ = "velpgroupsindocument"
    

    user_id = mapped_column(
        db.Integer, db.ForeignKey("useraccount.id"), primary_key=True
    )
    doc_id = mapped_column(db.Integer, db.ForeignKey("block.id"), primary_key=True)
    velp_group_id = mapped_column(
        db.Integer, db.ForeignKey("velpgroup.id"), primary_key=True
    )


class VelpLabel(db.Model):
    """A label that can be assigned to a Velp."""

    __tablename__ = "velplabel"
    

    id = mapped_column(db.Integer, primary_key=True)
    # TODO make not nullable
    creator_id = mapped_column(
        db.Integer, db.ForeignKey("useraccount.id"), nullable=True
    )

    creator = db.relationship("User")
    velps = db.relationship(
        "Velp",
        back_populates="labels",
        secondary=LabelInVelp.__table__,
        collection_class=attribute_mapped_collection("id"),
    )


class VelpLabelContent(db.Model):
    __tablename__ = "velplabelcontent"
    

    velplabel_id = mapped_column(
        db.Integer, db.ForeignKey("velplabel.id"), primary_key=True
    )
    language_id = mapped_column(db.Text, primary_key=True)
    content = mapped_column(db.Text)

    velplabel = db.relationship("VelpLabel")

    def to_json(self) -> dict:
        return {
            "id": self.velplabel_id,
            "language_id": self.language_id,
            "content": self.content,
        }


class VelpVersion(db.Model):
    __tablename__ = "velpversion"
    

    id = mapped_column(db.Integer, primary_key=True)
    velp_id = mapped_column(db.Integer, db.ForeignKey("velp.id"), nullable=False)
    modify_time = mapped_column(
        db.DateTime(timezone=True), nullable=False, default=datetime.utcnow
    )

    velp = db.relationship("Velp", overlaps="velp_versions") # : Velp
    content = db.relationship("VelpContent", overlaps="velp_version") # : list[VelpContent]
