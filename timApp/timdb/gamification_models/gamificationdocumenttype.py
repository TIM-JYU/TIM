from tim_app import db


class GamificationDocumentType(db.model):
    __bind_key__ = 'tim_main'
    __tablename__ = 'gamificationdocumenttype'
    document_type_id = db.Column (db.Integer, primary_key = True)
    document_type_name = db.Column (db.Text)

    @staticmethod
    def create() -> 'GamificationDocumentType':
        """Creates a new entry into GamificationDocumentType table"""

