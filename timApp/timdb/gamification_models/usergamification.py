from timdb.tim_models import db


class UserGamification(db.Model):
    """
    Created by TIMG
    This class represents the UserGamification database table, used connecting users to gamification documents.
    """
    __bind_key__ = 'tim_main'
    __tablename__ = 'usergamification'
    gamification_doc_id = db.Column(db.Integer, db.ForeignKey('gamificationdocument.id'))
    user_id = db.Column(db.Integer, db.ForeignKey('user.id'))
    is_gamified = db.Column(db.Boolean)


    @staticmethod
    def create(gamDocID: int, userID: int, isGamified: bool) -> 'UserGamification':
        """Creates a new entry into DocGamified table"""

        uGamification = UserGamification(gamDocID, userID, isGamified)
        db.session.add(uGamification)
        db.session.commit()

        return uGamification
