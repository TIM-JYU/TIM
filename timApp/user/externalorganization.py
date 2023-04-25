from functools import lru_cache

from flask import current_app

from timApp.timdb.sqa import db


class ExternalOrganization(db.Model):
    id = db.Column(db.Integer, primary_key=True)
    name = db.Column(db.Text, nullable=False, unique=True)

    uniquecodes = db.relationship("PersonalUniqueCode", back_populates="organization")

    @staticmethod
    def get_or_create(name: str):
        found = ExternalOrganization.query.filter_by(name=name).first()
        if not found:
            found = ExternalOrganization(name=name)
            db.session.add(found)
        return found


@lru_cache
def get_home_organization_id():
    org = ExternalOrganization.get_or_create(
        name=current_app.config["HOME_ORGANIZATION"]
    )
    if org.id is None:
        db.session.flush()
    return org.id
