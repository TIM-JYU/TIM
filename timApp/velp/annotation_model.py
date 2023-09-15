import json
from dataclasses import dataclass
from datetime import datetime
from typing import Optional, TYPE_CHECKING, List

from sqlalchemy import ForeignKey
from sqlalchemy.orm import mapped_column, Mapped, relationship

from timApp.timdb.sqa import db
from timApp.timdb.types import datetime_tz

if TYPE_CHECKING:
    from timApp.user.user import User
    from timApp.answer.answer import Answer
    from timApp.velp.velp_models import AnnotationComment, VelpVersion, VelpContent


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

    id: Mapped[int] = mapped_column(primary_key=True)
    """Annotation identifier."""

    velp_version_id: Mapped[int] = mapped_column(ForeignKey("velpversion.id"))
    """Id of the velp that has been used for this annotation."""

    annotator_id: Mapped[int] = mapped_column(ForeignKey("useraccount.id"))
    """Id of the User who created the annotation."""

    points: Mapped[Optional[float]]
    """Points associated with the annotation."""

    creation_time: Mapped[datetime_tz] = mapped_column(default=datetime.utcnow)
    """Creation time."""

    valid_from: Mapped[Optional[datetime_tz]] = mapped_column(default=datetime.utcnow)
    """Since when should this annotation be valid."""

    valid_until: Mapped[Optional[datetime_tz]]
    """Until when should this annotation be valid."""

    visible_to: Mapped[Optional[int]]
    """Who should this annotation be visible to.
    
    Possible values are denoted by AnnotationVisibility enum:
    
    myself = 1
    owner = 2
    teacher = 3
    everyone = 4
    
    """

    document_id: Mapped[Optional[int]] = mapped_column(ForeignKey("block.id"))
    """Id of the document in case this is a paragraph annotation."""

    answer_id: Mapped[Optional[int]] = mapped_column(ForeignKey("answer.id"))
    """Id of the Answer in case this is an answer annotation."""

    paragraph_id_start: Mapped[Optional[str]]
    """The id of the paragraph where this annotation starts from (in case this is a paragraph annotation)."""

    paragraph_id_end: Mapped[Optional[str]]
    """The id of the paragraph where this annotation ends (in case this is a paragraph annotation)."""

    offset_start: Mapped[Optional[int]]
    """Positional information about the annotation."""

    node_start: Mapped[Optional[int]]
    """Positional information about the annotation."""

    depth_start: Mapped[Optional[int]]
    """Positional information about the annotation."""

    offset_end: Mapped[Optional[int]]
    """Positional information about the annotation."""

    node_end: Mapped[Optional[int]]
    """Positional information about the annotation."""

    depth_end: Mapped[Optional[int]]
    """Positional information about the annotation."""

    hash_start: Mapped[Optional[str]]
    """Positional information about the annotation."""

    hash_end: Mapped[Optional[str]]
    """Positional information about the annotation."""

    color: Mapped[Optional[str]]
    """Color for the annotation."""

    element_path_start: Mapped[Optional[str]]
    """Positional information about the annotation."""

    element_path_end: Mapped[Optional[str]]
    """Positional information about the annotation."""

    draw_data: Mapped[Optional[str]]
    """Drawing information about the annotation (for annotations on images)."""

    style: Mapped[Optional[int]]
    """Appearance of the annotation"""

    annotator: Mapped["User"] = relationship(back_populates="annotations")
    answer: Mapped[Optional["Answer"]] = relationship(back_populates="annotations")
    comments: Mapped[List["AnnotationComment"]] = relationship(
        order_by="AnnotationComment.id"
    )
    velp_version: Mapped["VelpVersion"] = relationship()
    velp_content: Mapped["VelpContent"] = relationship(
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
