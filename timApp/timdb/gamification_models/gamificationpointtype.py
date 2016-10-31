from tim_app import db
<<<<<<< HEAD

class GamificationPointType(db.model):
    """
    Created by TIMG
    This class represents the GamificationPointType database table, that contains different gamification point types.
    Currently only two types exist (Demo points = 1, Clicks from read paragraphs = 2).
    """
    __bind_key__ = 'tim_main'
    __tablename__ = 'gamificationpointtype'
    point_type_id = db.Column (db.Integer, primary_key = True)
    point_type_name = db.Column (db.Text)


=======
from timdb.dbutils import insert_block
from timdb.blocktypes import blocktypes
from timdb.models import docentry


class GamificationPointType(db.Model):
    __bind_key__ = 'tim_main'
    __tablename__ = 'gamificationpointtype'
    point_type_id = db.Column (db.Integer, primary_key=True)
    point_type_name = db.Column(db.text)

    @staticmethod
    def create(id,text) -> 'GamificationPointType':
        """Creates a new entry into GamificationPointType table"""
        gamificationpointtype = GamificationPointType(point_type_id=id,point_type_name=text)
        db.session.add(gamificationpointtype)
        db.session.commit()
        return gamificationpointtype
>>>>>>> origin/timg-new
