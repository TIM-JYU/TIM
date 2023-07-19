from timApp.timdb.sqa import db


class SlideStatus(db.Model):
    __tablename__ = "slide_status"
    __allow_unmapped__ = True
    
    doc_id = db.Column(db.Integer, db.ForeignKey("block.id"), primary_key=True)
    status = db.Column(db.Text, nullable=False)

    def __init__(self, doc_id, status):
        self.doc_id = doc_id
        self.status = status
