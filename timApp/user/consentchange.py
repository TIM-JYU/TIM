from sqlalchemy import func
from sqlalchemy.orm import mapped_column, Mapped, relationship

from timApp.timdb.sqa import db
from timApp.timdb.types import datetime_tz
from timApp.user.user import Consent, User


class ConsentChange(db.Model):
    __tablename__ = "consentchange"

    id: Mapped[int] = mapped_column(primary_key=True)
    user_id: Mapped[int] = mapped_column(db.ForeignKey("useraccount.id"))
    time: Mapped[datetime_tz] = mapped_column(default=func.now())
    consent: Mapped[Consent]

    user: Mapped["User"] = relationship(back_populates="consents")
