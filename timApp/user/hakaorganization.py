from functools import lru_cache

from flask import current_app
from sqlalchemy import select

from timApp.timdb.sqa import db


class HakaOrganization(db.Model):
    __allow_unmapped__ = True
    
    id = db.Column(db.Integer, primary_key=True)
    name = db.Column(db.Text, nullable=False, unique=True)

    uniquecodes = db.relationship("PersonalUniqueCode", back_populates="organization")

    @staticmethod
    def get_or_create(name: str):
        found = (
            db.session.execute(select(HakaOrganization).filter_by(name=name).limit(1))
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
