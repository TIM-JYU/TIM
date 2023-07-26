from sqlalchemy.orm import mapped_column

from timApp.timdb.sqa import db


class SlideStatus(db.Model):
    __tablename__ = "slide_status"
    

    doc_id = mapped_column(db.Integer, db.ForeignKey("block.id"), primary_key=True)
    status = mapped_column(db.Text, nullable=False)

    def __init__(self, doc_id, status):
        self.doc_id = doc_id
        self.status = status
