from sqlalchemy import ForeignKey
from sqlalchemy.orm import mapped_column, Mapped

from timApp.timdb.types import DbModel


class SlideStatus(DbModel):
    __tablename__ = "slide_status"

    doc_id: Mapped[int] = mapped_column(ForeignKey("block.id"), primary_key=True)
    status: Mapped[str]
