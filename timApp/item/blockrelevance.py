from timApp.timdb.sqa import db


class BlockRelevance(db.Model):
    """A relevance value of a block."""
    __tablename__ = 'blockrelevance'
    block_id = db.Column(db.Integer, db.ForeignKey('block.id'), primary_key=True)
    relevance = db.Column(db.Integer, nullable=False)

    _block = db.relationship('Block', back_populates='relevance')
