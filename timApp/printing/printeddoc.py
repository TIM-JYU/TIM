from typing import Optional

from sqlalchemy import func, ForeignKey
from sqlalchemy.orm import mapped_column, Mapped

from timApp.timdb.sqa import db
from timApp.timdb.types import datetime_tz, DbModel


class PrintedDoc(DbModel):
    """A printed document. A PrintedDoc is created each time a document is printed
    (CSS printing does not count because it happens entirely in browser)."""

    __tablename__ = "printed_doc"

    id: Mapped[int] = mapped_column(primary_key=True)
    doc_id: Mapped[int] = mapped_column(ForeignKey("block.id"))
    """Id of the printed document."""

    template_doc_id: Mapped[Optional[int]] = mapped_column(ForeignKey("block.id"))
    """Id of the template document."""

    file_type: Mapped[str]
    """The filetype of the print."""

    path_to_file: Mapped[Optional[str]]
    """Path to the printed document in the filesystem."""

    version: Mapped[str]
    """Version (in practice, a hash) for identifying whether a document has already been printed and can be
    fetched from cache.
    """

    temp: Mapped[bool] = mapped_column(default=True)
    """Whether the printed document is stored only temporarily (gets deleted after some time)."""

    created: Mapped[datetime_tz] = mapped_column(default=func.now())
    """Timestamp of printing."""
