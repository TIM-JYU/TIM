from typing import TYPE_CHECKING

from sqlalchemy import ForeignKey
from sqlalchemy.orm import mapped_column, Mapped, relationship

from timApp.timdb.types import DbModel

if TYPE_CHECKING:
    from timApp.item.block import Block


class BlockRelevance(DbModel):
    """A relevance value of a block (used in search)."""

    block_id: Mapped[int] = mapped_column(ForeignKey("block.id"), primary_key=True)
    relevance: Mapped[int]

    _block: Mapped["Block"] = relationship(back_populates="relevance")
