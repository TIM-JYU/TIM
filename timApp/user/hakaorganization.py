from timApp.timdb.sqa import db


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
