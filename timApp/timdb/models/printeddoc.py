from sqlalchemy import func

from timdb.tim_models import db


class PrintedDoc(db.Model):
    """
    Model for printed_docs table
    """

    __bind_key__ = 'tim_main'
    __tablename__ = 'printed_docs'
    id = db.Column(db.Integer, primary_key=True)
    doc_id = db.Column(db.Integer, db.ForeignKey('block.id'), nullable=False)  # NOTE Added foreign key
    path_to_file = db.Column(db.Text, nullable=False) # path to the printed document in the filesystem
    settings_hash = db.Column(db.Text, nullable=False) # stores hash calculated from used print settings
    temp = db.Column(db.Boolean, default=True, nullable=False)
    created = db.Column(db.DateTime(timezone=True), default=func.now(), nullable=False)


    def get_by_id(printed_doc_id: int):
        return db.session.query(PrintedDoc).filter_by(id=printed_doc_id).first()