from sqlalchemy import func

from timdb.tim_models import db


class PrintedDoc(db.Model):
    """
    Model for printed_docs table
    """

    __bind_key__ = 'tim_main'
    __tablename__ = 'printed_docs'
    id = db.Column(db.Integer, primary_key=True)
    doc_id = db.Column(db.Integer, db.ForeignKey('block.id'), nullable=False)  # The id of the printed document
    doc_version = db.Column(db.Float, nullable=False) # Stored the version number of the printed document

    template_doc_id = db.Column(db.Integer, db.ForeignKey('block.id'), nullable=False)  # The id of the template document
    template_doc_version = db.Column(db.Float, nullable=False) # Stored the version number of the template document

    file_type = db.Column(db.Text, nullable=False) # The filetype of the print
    path_to_file = db.Column(db.Text, nullable=True) # Path to the printed document in the filesystem
    temp = db.Column(db.Boolean, default=True, nullable=False)
    created = db.Column(db.DateTime(timezone=True), default=func.now(), nullable=False)

    # the following column is present solely for future use,
    # as the settings are not yet implemented as such
    # TODO: is this even beneficial?
    settings_hash = db.Column(db.Text, nullable=True) # Stores hash calculated from used print settings

    @staticmethod
    def get_by_id(printed_doc_id: int):
        """
        Returns the PrintedDoc elements that have been produced from the DocEntry with the with the given id
        :param printed_doc_id:
        :return: PrintedDocs with the given source doc
        """
        return db.session.query(PrintedDoc).filter_by(id=printed_doc_id).first()