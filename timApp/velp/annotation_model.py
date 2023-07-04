import json
from dataclasses import dataclass
from datetime import datetime

from timApp.timdb.sqa import db


@dataclass
class AnnotationCoordinate:
    par_id: str
    offset: int | None = None
    depth: int | None = None
    node: int | None = None
    el_path: list[int] | None = None
    t: str | None = None


@dataclass
class AnnotationPosition:
    """
    start.par_id: ID of paragraph where annotation starts.
    end.par_id: ID of paragraph where annotation ends.
    start.offset: Character location where annotation starts.
    start.node:
    start.depth: depth of the element path
    end.offset: Character location where annotation ends.
    end.node:
    end.depth: depth of the element path
    start.t: Hash code of paragraph where annotation starts.
    end.t: Hash code of paragraph where annotation ends.
    start.el_path: List of elements as text (parsed in interface) connected to annotation start.
    end.el_path: List of elements as text (parsed in interface) connected to annotation end.
    """

    start: AnnotationCoordinate
    end: AnnotationCoordinate


class Annotation(db.Model):
    """An annotation that can be associated with an Answer or with a DocParagraph in a Document.

    The annotation can start and end in specific positions, in which case the annotation is supposed to be displayed
    as highlighted text in the corresponding location.
    """

    __tablename__ = "annotation"
    id = db.Column(db.Integer, primary_key=True)
    """Annotation identifier."""

    velp_version_id = db.Column(
        db.Integer, db.ForeignKey("velpversion.id"), nullable=False
    )
    """Id of the velp that has been used for this annotation."""

    annotator_id = db.Column(
        db.Integer, db.ForeignKey("useraccount.id"), nullable=False
    )
    """Id of the User who created the annotation."""

    points = db.Column(db.Float)
    """Points associated with the annotation."""

    creation_time = db.Column(
        db.DateTime(timezone=True), nullable=False, default=datetime.utcnow
    )
    """Creation time."""

    valid_from = db.Column(db.DateTime(timezone=True), default=datetime.utcnow)
    """Since when should this annotation be valid."""

    valid_until = db.Column(db.DateTime(timezone=True))
    """Until when should this annotation be valid."""

    visible_to = db.Column(db.Integer)
    """Who should this annotation be visible to.
    
    Possible values are denoted by AnnotationVisibility enum:
    
    myself = 1
    owner = 2
    teacher = 3
    everyone = 4
    
    """

    document_id = db.Column(db.Integer, db.ForeignKey("block.id"))
    """Id of the document in case this is a paragraph annotation."""

    answer_id = db.Column(db.Integer, db.ForeignKey("answer.id"))
    """Id of the Answer in case this is an answer annotation."""

    paragraph_id_start = db.Column(db.Text)
    """The id of the paragraph where this annotation starts from (in case this is a paragraph annotation)."""

    paragraph_id_end = db.Column(db.Text)
    """The id of the paragraph where this annotation ends (in case this is a paragraph annotation)."""

    offset_start = db.Column(db.Integer)
    """Positional information about the annotation."""

    node_start = db.Column(db.Integer)
    """Positional information about the annotation."""

    depth_start = db.Column(db.Integer)
    """Positional information about the annotation."""

    offset_end = db.Column(db.Integer)
    """Positional information about the annotation."""

    node_end = db.Column(db.Integer)
    """Positional information about the annotation."""

    depth_end = db.Column(db.Integer)
    """Positional information about the annotation."""

    hash_start = db.Column(db.Text)
    """Positional information about the annotation."""

    hash_end = db.Column(db.Text)
    """Positional information about the annotation."""

    color = db.Column(db.Text)
    """Color for the annotation."""

    element_path_start = db.Column(db.Text)
    """Positional information about the annotation."""

    element_path_end = db.Column(db.Text)
    """Positional information about the annotation."""

    draw_data = db.Column(db.Text)
    """Drawing information about the annotation (for annotations on images)."""

    style = db.Column(db.Integer)
    """Appearance of the annotation"""

    annotator = db.relationship("User", back_populates="annotations")
    answer = db.relationship("Answer", back_populates="annotations")
    comments = db.relationship("AnnotationComment", order_by="AnnotationComment.id")
    velp_version = db.relationship("VelpVersion")
    velp_content = db.relationship(
        "VelpContent",
        primaryjoin="VelpContent.version_id == foreign(Annotation.velp_version_id)",
        overlaps="velp_version",
    )

    def set_position_info(self, coordinates: AnnotationPosition) -> None:
        start = coordinates.start
        end = coordinates.end
        self.offset_start = start.offset
        self.depth_start = start.depth
        self.node_start = start.node
        self.offset_end = end.offset
        self.depth_end = end.depth
        self.node_end = end.node
        element_path_start = start.el_path
        if element_path_start is not None:
            self.element_path_start = str(element_path_start)
        element_path_end = end.el_path
        if element_path_end is not None:
            self.element_path_end = str(element_path_end)
        self.paragraph_id_start = start.par_id
        self.hash_start = start.t
        self.paragraph_id_end = end.par_id
        self.hash_end = end.t

    def to_json(
        self,
    ) -> dict:
        if self.element_path_start is not None and self.element_path_end is not None:
            try:
                start_path = [int(i) for i in self.element_path_start[1:-1].split(",")]
                end_path = [int(i) for i in self.element_path_end[1:-1].split(",")]
            except ValueError:
                start_path = None
                end_path = None
        else:
            start_path = None
            end_path = None

        start = {
            "par_id": self.paragraph_id_end,
            "offset": self.offset_start,
            "node": self.node_start,
            "depth": self.depth_start,
            "t": self.hash_start,
            "el_path": start_path,
        }
        end = {
            "par_id": self.paragraph_id_end,
            "offset": self.offset_end,
            "node": self.node_end,
            "depth": self.depth_end,
            "t": self.hash_end,
            "el_path": end_path,
        }
        ret = {
            "id": self.id,
            "annotator": self.annotator,
            "answer": self.answer,
            "color": self.color or self.velp_version.velp.color,
            "comments": self.comments,
            "content": self.velp_content.content,
            "coord": {"start": start, "end": end},
            "creation_time": self.creation_time,
            "points": self.points,
            "valid_until": self.valid_until,
            "velp": self.velp_version.velp_id,
            "visible_to": self.visible_to,
            "style": self.style,
        }
        if self.draw_data:
            try:
                ret["draw_data"] = json.loads(self.draw_data)
            except json.decoder.JSONDecodeError:
                pass

        return ret
