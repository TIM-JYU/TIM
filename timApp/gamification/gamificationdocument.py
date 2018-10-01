from timApp.timdb.sqa import db


class GamificationDocument(db.Model):
    """Created by TIMG This class represents the GamificationDocument database table.

    If a line exists in this table, the document with the indicated ID is gamified.

    """
    __tablename__ = 'gamificationdocument'
    id = db.Column(db.Integer, db.ForeignKey('block.id'), primary_key=True)

    @staticmethod
    def create(document_id) -> 'GamificationDocument':
        """Creates a new entry into GamificationDocument table."""

        gamificationdocument = GamificationDocument(id=document_id)
        db.session.add(gamificationdocument)
        return gamificationdocument

    @staticmethod
    def doc_is_gamified(doc_id):
        return GamificationDocument.query(doc_id).filter_by(id=doc_id).scalar() is not None
