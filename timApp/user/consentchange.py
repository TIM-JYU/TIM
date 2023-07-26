from sqlalchemy import func
from sqlalchemy.orm import mapped_column

from timApp.timdb.sqa import db
from timApp.user.user import Consent


class ConsentChange(db.Model):
    __tablename__ = "consentchange"
    

    id = mapped_column(db.Integer, primary_key=True)
    user_id = mapped_column(db.Integer, db.ForeignKey("useraccount.id"), nullable=False)
    time = mapped_column(db.DateTime(timezone=True), nullable=False, default=func.now())
    consent = mapped_column(db.Enum(Consent), nullable=False)

    user = db.relationship("User", back_populates="consents", lazy="select")
