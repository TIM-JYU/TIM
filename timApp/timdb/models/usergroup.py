from tim_app import db
from timdb.special_group_names import ANONYMOUS_GROUPNAME, LARGE_GROUPS


class UserGroup(db.Model):
    __bind_key__ = 'tim_main'
    __tablename__ = 'usergroup'
    id = db.Column(db.Integer, primary_key=True)
    name = db.Column(db.Text, nullable=False, unique=True)

    def is_anonymous(self):
        return self.name == ANONYMOUS_GROUPNAME

    def is_large(self):
        return self.name in LARGE_GROUPS

    def __json__(self):
        return ['id', 'name']