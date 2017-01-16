from timdb.tim_models import db


class GamificationDocumentType(db.model):
    """
    Created by TIMG
    This class represents the GamificationDocumentType database table, that differentiates documents for gamification
    point purposes. Currently only two types exist (Lectures = 1, Demos = 2).
    """
    __bind_key__ = 'tim_main'
    __tablename__ = 'gamificationdocumenttype'
    document_type_id = db.Column(db.Integer, primary_key=True)
    document_type_name = db.Column(db.Text)
