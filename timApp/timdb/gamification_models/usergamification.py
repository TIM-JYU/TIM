from tim_app import db
from timdb.dbutils import insert_block
from timdb.blocktypes import blocktypes
from timdb.models import docentry


class UserGamification(db.Model):
    __bind_key__ = 'tim_main'
    __tablename__ = 'usergamification'
    #En ole ihan varma, miten toimii jos pitää setviä kahden avaimen avulla. -iltapeur
    gamification_doc_id = db.Column (db.Integer, primary_key=True)
    user_id = db.Column(db.Integer, primary_key=True)
    #En ole testannut, miten tuo boolean pitää heittää.
    is_gamified = db.Column(db.Boolean)

    @staticmethod
    def create(gamification_doc_id,user_id,is_gamified) -> 'UserGamification':
        usergamification = UserGamification(gamification_doc_id=gamification_doc_id,user_id=user_id,is_gamified=is_gamified)
        db.session.add(usergamification)
        db.session.commit()
        return usergamification