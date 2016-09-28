from tim_app import db

class GamificationDocument(db.Model):
    __bind_key__ = 'tim_main'
    __tablename__ = 'gamificationdocument'
    id = db.Column (db.Integer, db.ForeignKey('block.id'), primary_key=True)