from functools import lru_cache
from typing import TYPE_CHECKING

from flask import current_app
from sqlalchemy import select
from sqlalchemy.orm import mapped_column, Mapped, relationship

from timApp.timdb.sqa import db, run_sql

if TYPE_CHECKING:
    from timApp.user.personaluniquecode import PersonalUniqueCode


class HakaOrganization(db.Model):
    __tablename__ = "haka_organization"

    id: Mapped[int] = mapped_column(primary_key=True)
    name: Mapped[str] = mapped_column(unique=True)

    uniquecodes: Mapped["PersonalUniqueCode"] = relationship(
        back_populates="organization"
    )

    @staticmethod
    def get_or_create(name: str):
        found = (
            run_sql(select(HakaOrganization).filter_by(name=name).limit(1))
            .scalars()
            .first()
        )
        if not found:
            found = HakaOrganization(name=name)
            db.session.add(found)
        return found


@lru_cache
def get_home_organization_id():
    org = HakaOrganization.get_or_create(name=current_app.config["HOME_ORGANIZATION"])
    if org.id is None:
        db.session.flush()
    return org.id
