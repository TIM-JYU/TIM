from tim_app import db

class GamificationDocument(db.Model):
    """
    Created by TIMG
    This class represents the GamificationDocument database table. If a line exists in this table, the document with
    the indicated ID is gamified.
    """
    __bind_key__ = 'tim_main'
    __tablename__ = 'gamificationdocument'
    id = db.Column (db.Integer, db.ForeignKey('block.id'), primary_key=True)

    @staticmethod
    def create(id) -> 'GamificationDocument':
        """Creates a new entry into GamificationDocument table"""

        gamificationdocument = GamificationDocument(id=id)
        db.session.add(gamificationdocument)
        db.session.commit()
        return gamificationdocument

    @staticmethod
    def doc_is_gamified(id):
        return GamificationDocument.query(id).filter_by(id=id).scalar() is not None
