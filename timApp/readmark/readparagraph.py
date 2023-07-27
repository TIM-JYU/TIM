from typing import TYPE_CHECKING

from sqlalchemy import func
from sqlalchemy.orm import mapped_column, Mapped, relationship

from timApp.readmark.readparagraphtype import ReadParagraphType
from timApp.timdb.sqa import db
from timApp.timdb.types import datetime_tz

if TYPE_CHECKING:
    from timApp.user.usergroup import UserGroup


class ReadParagraph(db.Model):
    """Denotes that a User(Group) has read a specific paragraph in some way."""

    __tablename__ = "readparagraph"

    id: Mapped[int] = mapped_column(primary_key=True)
    """Readmark id."""

    usergroup_id: Mapped[int] = mapped_column(db.ForeignKey("usergroup.id"))
    """UserGroup id."""

    doc_id: Mapped[int] = mapped_column(db.ForeignKey("block.id"))
    """Document id."""

    par_id: Mapped[str]
    """Paragraph id."""

    type: Mapped[ReadParagraphType]
    """Readmark type."""

    par_hash: Mapped[str]
    """Paragraph hash at the time the readmark was registered."""

    timestamp: Mapped[datetime_tz] = mapped_column(default=func.now())
    """The time the readmark was registered."""

    __table_args__ = (
        db.Index("readparagraph_doc_id_par_id_idx", "doc_id", "par_id"),
        db.Index("readparagraph_doc_id_usergroup_id_idx", "doc_id", "usergroup_id"),
    )

    usergroup: Mapped["UserGroup"] = relationship(back_populates="readparagraphs")
