from sqlalchemy import func
from sqlalchemy.orm import mapped_column

from timApp.timdb.sqa import db


class PrintedDoc(db.Model):
    """A printed document. A PrintedDoc is created each time a document is printed
    (CSS printing does not count because it happens entirely in browser)."""

    __tablename__ = "printed_doc"
    

    id = mapped_column(db.Integer, primary_key=True)
    doc_id = mapped_column(db.Integer, db.ForeignKey("block.id"), nullable=False)
    """Id of the printed document."""

    template_doc_id = mapped_column(
        db.Integer, db.ForeignKey("block.id"), nullable=True
    )
    """Id of the template document."""

    file_type = mapped_column(db.Text, nullable=False)
    """The filetype of the print."""

    path_to_file = mapped_column(db.Text, nullable=True)
    """Path to the printed document in the filesystem."""

    version = mapped_column(db.Text, nullable=False)
    """Version (in practice, a hash) for identifying whether a document has already been printed and can be
    fetched from cache.
    """

    temp = mapped_column(db.Boolean, default=True, nullable=False)
    """Whether the printed document is stored only temporarily (gets deleted after some time)."""

    created = mapped_column(
        db.DateTime(timezone=True), default=func.now(), nullable=False
    )
    """Timestamp of printing."""
