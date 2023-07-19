from sqlalchemy import func

from timApp.timdb.sqa import db
from timApp.user.user import Consent


class ConsentChange(db.Model):
    __tablename__ = "consentchange"
    __allow_unmapped__ = True
    
    id = db.Column(db.Integer, primary_key=True)
    user_id = db.Column(db.Integer, db.ForeignKey("useraccount.id"), nullable=False)
    time = db.Column(db.DateTime(timezone=True), nullable=False, default=func.now())
    consent = db.Column(db.Enum(Consent), nullable=False)

    user = db.relationship("User", back_populates="consents", lazy="select")
