from timApp.timdb.tim_models import db


class DocumentGamificationPoint(db.Model):
    """Created by TIMG This class represents the DocumentGamificationPoint database table, that stores gamification
    point information regarding each document that is gamified."""
    __bind_key__ = 'tim_main'
    __tablename__ = 'documentgamificationpoint'
    doc_id = db.Column(db.Integer, db.ForeignKey('block.id'), primary_key=True)
    point_type_id = db.Column(db.Integer, db.ForeignKey('gamificationpointtype.point_type_id'), primary_key=True)
    amount = db.Column(db.Integer)
    multiplier = db.Column(db.Integer)
    is_active = db.Column(db.Boolean)

    @staticmethod
    def create(doc_id: int, point_type_id: int, amount: int, multip: int, is_active: bool) -> 'DocumentGamificationPoint':
        """Creates a new entry into DocGamified table."""

        doc_gamif_point = DocumentGamificationPoint(doc_id, point_type_id, amount, multip, is_active)
        db.session.add(doc_gamif_point)
        return doc_gamif_point
