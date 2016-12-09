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

    # #Kokeillaan etsiä block selffin avulla.
    # def get_docgamified_block(self):
    #     g = block.query.filter_by(id=self.doc_id).first()
    #     if g:
    #         return g
    #     raise TimDbException('docgamified_block not found')
    #
    # #Kokeillaan etsiä gamificationdocumenttype selffin avulla.
    # def get_gamificationdocumenttype(self):
    #     g = gamificationdocumenttype.query.filter_by(document_type_id=self.doc_type_id).first()
    #     if g:
    #         return g
    #     raise TimDbException('gamificationdocumenttype not found')
    #
    # #Revitään gamificationdocument selffin avulla.
    # def get_gamificationdocument(self):
    #     g = gamificationdocument.query.filter_by(id=self.gamification_doc_id).first()
    #     if g:
    #         return g
    #     raise TimDbException('gamificationdocumenttype not found')
    #
    #
    # @staticmethod
    # def create(gamification_doc_id,doc_id,doc_type_id) -> 'DocGamified':
    #     """Creates a new entry into DocGamified table"""
    #     docgamified = DocGamified(gamification_doc_id=gamification_doc_id,doc_id=doc_id,doc_type_id=doc_type_id)
    #     db.session.add(docgamified)
    #     db.session.commit()
    #     return docgamified
