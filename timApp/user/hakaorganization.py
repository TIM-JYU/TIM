from timApp.timdb.sqa import db

from flask import current_app

class HakaOrganization(db.Model):
    id = db.Column(db.Integer, primary_key=True)
    name = db.Column(db.Text, nullable=False, unique=True)

    uniquecodes = db.relationship('PersonalUniqueCode', back_populates='organization')

    @staticmethod
    def get_or_create(name: str):
        found = HakaOrganization.query.filter_by(name=name).first()
        if not found:
            found = HakaOrganization(name=name)
            db.session.add(found)
        return found

_home_organization_id = None
def get_home_organisation_id():
    global _home_organization_id
    if _home_organization_id is None:
        org = HakaOrganization.get_or_create(name=current_app.config['HOME_ORGANIZATION'])
        if org.id is None:
            db.session.flush()
        _home_organization_id = org.id
    return _home_organization_id