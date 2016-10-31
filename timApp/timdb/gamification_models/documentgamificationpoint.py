from tim_app import db
<<<<<<< HEAD


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

=======
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
>>>>>>> origin/timg-new
