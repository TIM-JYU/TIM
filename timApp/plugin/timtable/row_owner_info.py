from typing import Optional

from sqlalchemy import ForeignKey
from sqlalchemy.orm import mapped_column, Mapped

from timApp.timdb.types import DbModel


class RowOwnerInfo(DbModel):
    """
    Information about the owner of a TimTable row. Includes document and paragraph
    id for determining the TimTable instance.
    """

    doc_id: Mapped[int] = mapped_column(primary_key=True)
    par_id: Mapped[str] = mapped_column(primary_key=True)
    unique_row_id: Mapped[int] = mapped_column(primary_key=True)
    usergroup_id: Mapped[Optional[int]] = mapped_column(ForeignKey("usergroup.id"))

    # usergroup = relationship('UserGroup', back_populates='rowOwnerInfo')
    # block = relationship('Block', back_populates='tags')

    def __json__(self):
        return ["doc_id", "par_id", "unique_row_id", "usergroup_id"]
