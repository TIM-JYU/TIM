from sqlalchemy.orm import mapped_column

from timApp.timdb.sqa import db


class BlockRelevance(db.Model):
    """A relevance value of a block (used in search)."""

    __tablename__ = "blockrelevance"
    

    block_id = mapped_column(db.Integer, db.ForeignKey("block.id"), primary_key=True)
    relevance = mapped_column(db.Integer, nullable=False)

    _block = db.relationship("Block", back_populates="relevance")
