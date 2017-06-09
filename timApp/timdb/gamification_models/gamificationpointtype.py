from timApp.timdb.tim_models import db


class GamificationPointType(db.Model):
    """Created by TIMG This class represents the GamificationPointType database table, that contains different
    gamification point types.

    Currently only two types exist (Demo points = 1, Clicks from read paragraphs = 2).

    """
    __bind_key__ = 'tim_main'
    __tablename__ = 'gamificationpointtype'
    point_type_id = db.Column(db.Integer, primary_key=True)
    point_type_name = db.Column(db.Text)
