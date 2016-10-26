from tim_app import db
from timdb.dbutils import insert_block
from timdb.blocktypes import blocktypes
from timdb.models import docentry


class DocumentGamificationPoint(db.Model):
    __bind_key__ = 'tim_main'
    __tablename__ = 'documentgamificationpoint'
    #En taaskaan oikein tiedä, miten käy kahden primary keyn kanssa etsimisessä.
    doc_id = db.Column (db.Integer, primary_key=True)
    point_type_id = db.Column (db.Integer, primary_key=True)
    amount = db.Column(db.Integer)
    multiplier = db.Column(db.Double)
    is_active = db.Column(db.Boolean)

    @staticmethod
    def create(doc_id,point_type_id,amount,multiplier,is_active) -> 'DocumentGamificationPoint':
        """Creates a new entry into DocumentGamificationPoint table"""
        documentgamificationpoint = DocumentGamificationPoint(doc_id=doc_id,point_type_id=point_type_id,amount=amount,
                                                              multiplier=multiplier,is_active=is_active)
        db.session.add(documentgamificationpoint)
        db.session.commit()
        return documentgamificationpoint