from timdb.tim_models import db
from timdb.gamification_models import gamificationdocument
from timdb.gamification_models import gamificationdocumenttype
from timdb.models import block
from timdb.timdbexception import TimDbException


class DocGamified(db.Model):
    """
    Created by TIMG
    This class represents the DocGamified database table, that connects regular documents to gamified ones.
    """
    __bind_key__ = 'tim_main'
    __tablename__ = 'docgamified'
    gamification_doc_id = db.Column(db.Integer, db.ForeignKey('block.id'))
    doc_id = db.Column(db.Integer, db.ForeignKey('block.id'))
    doc_type_id = db.Column(db.Integer, db.ForeignKey('gamificationdocumenttype.document_type_id'))


    @staticmethod
    def create(docGamID: int, documID: int, docTypeID: int) -> 'DocGamified':
        """Creates a new entry into DocGamified table"""

        dgamified = DocGamified(docGamID, documID, docTypeID)
        db.session.add(dgamified)
        db.session.commit()

        return dgamified
