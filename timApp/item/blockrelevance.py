from typing import TYPE_CHECKING

from sqlalchemy.orm import mapped_column, Mapped

from timApp.timdb.sqa import db

if TYPE_CHECKING:
    from timApp.item.block import Block

class BlockRelevance(db.Model):
    """A relevance value of a block (used in search)."""

    __tablename__ = "blockrelevance"

    block_id: Mapped[int] = mapped_column(db.ForeignKey("block.id"), primary_key=True)
    relevance: Mapped[int]

    _block: Mapped["Block"] = db.relationship(back_populates="relevance")
