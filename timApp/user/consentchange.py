from sqlalchemy import func, ForeignKey
from sqlalchemy.orm import mapped_column, Mapped, relationship

from timApp.timdb.types import datetime_tz, DbModel
from timApp.user.user import Consent, User


class ConsentChange(DbModel):
    id: Mapped[int] = mapped_column(primary_key=True)
    user_id: Mapped[int] = mapped_column(ForeignKey("useraccount.id"))
    time: Mapped[datetime_tz] = mapped_column(default=func.now())
    consent: Mapped[Consent]

    user: Mapped["User"] = relationship(back_populates="consents")
