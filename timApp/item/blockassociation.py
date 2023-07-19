from timApp.timdb.sqa import db


class BlockAssociation(db.Model):
    """Associates blocks with other blocks. Currently only used for associating uploaded files with documents."""

    __tablename__ = "blockassociation"
    __allow_unmapped__ = True

    parent = db.Column(db.Integer, db.ForeignKey("block.id"), primary_key=True)
    """The parent Block."""

    child = db.Column(db.Integer, db.ForeignKey("block.id"), primary_key=True)
    """The child Block."""
