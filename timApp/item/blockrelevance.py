from timApp.timdb.sqa import db


class BlockRelevance(db.Model):
    """A relevance value of a block (used in search)."""

    __tablename__ = "blockrelevance"
    __allow_unmapped__ = True
    
    block_id = db.Column(db.Integer, db.ForeignKey("block.id"), primary_key=True)
    relevance = db.Column(db.Integer, nullable=False)

    _block = db.relationship("Block", back_populates="relevance")
