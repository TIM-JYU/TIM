from sqlalchemy.orm import mapped_column, Mapped

from timApp.timdb.sqa import db


class BlockAssociation(db.Model):
    """Associates blocks with other blocks. Currently only used for associating uploaded files with documents."""

    __tablename__ = "blockassociation"

    parent: Mapped[int] = mapped_column(db.ForeignKey("block.id"), primary_key=True)
    """The parent Block."""

    child: Mapped[int] = mapped_column(db.ForeignKey("block.id"), primary_key=True)
    """The child Block."""
