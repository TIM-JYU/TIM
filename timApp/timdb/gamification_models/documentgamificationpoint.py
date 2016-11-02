from timdb.tim_models import db

class DocumentGamificationPoint(db.Model):
    """
    Created by TIMG
    This class represents the DocumentGamificationPoint database table, that stores gamification point information
    regarding each document that is gamified.
    """
    __bind_key__ = 'tim_main'
    __tablename__ = 'documentgamificationpoint'
    doc_id = db.Column(db.Integer, db.ForeignKey('block.id'))
    point_type_id = db.Column(db.Integer, db.ForeignKey('gamificationpointtype.point_type_id'))
    amount = db.Column(db.Integer)
    multiplier = db.Column(db.Integer)
    is_active = db.Column(db.Boolean)


    @staticmethod
    def create(docID: int, pointtypeID: int, amount: int, multip: int, isActive: bool) -> 'DocumentGamificationPoint':
        """Creates a new entry into DocGamified table"""

        DocGamiPoint = DocumentGamificationPoint(docID, pointtypeID, amount, multip, isActive)
        db.session.add(DocGamiPoint)
        db.session.commit()

        return DocGamiPoint
