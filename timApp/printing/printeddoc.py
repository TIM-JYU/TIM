from sqlalchemy import func

from timApp.timdb.sqa import db


class PrintedDoc(db.Model):
    """A printed document. A PrintedDoc is created each time a document is printed
    (CSS printing does not count because it happens entirely in browser)."""

    __tablename__ = "printed_doc"
    __allow_unmapped__ = True
    
    id = db.Column(db.Integer, primary_key=True)
    doc_id = db.Column(db.Integer, db.ForeignKey("block.id"), nullable=False)
    """Id of the printed document."""

    template_doc_id = db.Column(db.Integer, db.ForeignKey("block.id"), nullable=True)
    """Id of the template document."""

    file_type = db.Column(db.Text, nullable=False)
    """The filetype of the print."""

    path_to_file = db.Column(db.Text, nullable=True)
    """Path to the printed document in the filesystem."""

    version = db.Column(db.Text, nullable=False)
    """Version (in practice, a hash) for identifying whether a document has already been printed and can be
    fetched from cache.
    """

    temp = db.Column(db.Boolean, default=True, nullable=False)
    """Whether the printed document is stored only temporarily (gets deleted after some time)."""

    created = db.Column(db.DateTime(timezone=True), default=func.now(), nullable=False)
    """Timestamp of printing."""
