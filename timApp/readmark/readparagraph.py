from sqlalchemy import func

from timApp.readmark.readparagraphtype import ReadParagraphType
from timApp.timdb.sqa import db


class ReadParagraph(db.Model):
    """Denotes that a User(Group) has read a specific paragraph in some way."""

    __tablename__ = "readparagraph"
    __allow_unmapped__ = True
    
    id = db.Column(db.Integer, primary_key=True)
    """Readmark id."""

    usergroup_id = db.Column(db.Integer, db.ForeignKey("usergroup.id"), nullable=False)
    """UserGroup id."""

    doc_id = db.Column(db.Integer, db.ForeignKey("block.id"))
    """Document id."""

    par_id = db.Column(db.Text, nullable=False)
    """Paragraph id."""

    type = db.Column(db.Enum(ReadParagraphType), nullable=False)
    """Readmark type."""

    par_hash = db.Column(db.Text, nullable=False)
    """Paragraph hash at the time the readmark was registered."""

    timestamp = db.Column(
        db.DateTime(timezone=True), nullable=False, default=func.now()
    )
    """The time the readmark was registered."""

    __table_args__ = (
        db.Index("readparagraph_doc_id_par_id_idx", "doc_id", "par_id"),
        db.Index("readparagraph_doc_id_usergroup_id_idx", "doc_id", "usergroup_id"),
    )

    usergroup = db.relationship("UserGroup", back_populates="readparagraphs")


