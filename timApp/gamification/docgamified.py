from timApp.timdb.sqa import db


class DocGamified(db.Model):
    """Created by TIMG This class represents the DocGamified database table, that connects regular documents to gamified
    ones."""
    __tablename__ = 'docgamified'
    gamification_doc_id = db.Column(db.Integer, db.ForeignKey('block.id'), primary_key=True)
    doc_id = db.Column(db.Integer, db.ForeignKey('block.id'), primary_key=True)
    doc_type_id = db.Column(db.Integer, db.ForeignKey('gamificationdocumenttype.document_type_id'))

    @staticmethod
    def create(doc_game_id: int, document_id: int, doc_type_id: int) -> 'DocGamified':
        """Creates a new entry into DocGamified table."""

        dgamified = DocGamified(doc_game_id, document_id, doc_type_id)
        db.session.add(dgamified)
        return dgamified
