from timdb.tim_models import db


class UserGamification(db.Model):
    """
    Created by TIMG
    This class represents the UserGamification database table, used connecting users to gamification documents.
    """
    __bind_key__ = 'tim_main'
    __tablename__ = 'usergamification'
    gamification_doc_id = db.Column(db.Integer, db.ForeignKey('gamificationdocument.id'), primary_key=True)
    user_id = db.Column(db.Integer, db.ForeignKey('user.id'), primary_key=True)
    is_gamified = db.Column(db.Boolean)


    @staticmethod
    def create(game_doc_id: int, user_id: int, is_gamified: bool) -> 'UserGamification':
        """Creates a new entry into DocGamified table"""

        uGamification = UserGamification(game_doc_id, user_id, is_gamified)
        db.session.add(uGamification)
        db.session.commit()

        return uGamification
