import json

from typing import Dict

from documentmodel.timjsonencoder import TimJsonEncoder
from timdb.tim_models import db
from timdb.models.folder import Folder
from timdb.models.usergroup import UserGroup
from timdb.special_group_names import ANONYMOUS_GROUPNAME, ANONYMOUS_USERNAME
from timdb.timdbexception import TimDbException


class User(db.Model):
    __bind_key__ = 'tim_main'
    __tablename__ = 'useraccount'
    id = db.Column(db.Integer, primary_key=True)
    name = db.Column(db.Text, nullable=False, unique=True)
    real_name = db.Column(db.Text)
    email = db.Column(db.Text)
    prefs = db.Column(db.Text)
    pass_ = db.Column('pass', db.Text)
    yubikey = db.Column(db.Text)

    def get_personal_group(self) -> UserGroup:
        if self.id < 0 or self.name == ANONYMOUS_USERNAME:
            return UserGroup.query.filter_by(name=ANONYMOUS_GROUPNAME).first()
        g = UserGroup.query.filter_by(name=self.name).first()
        if g:
            return g
        g = UserGroup.query.filter_by(name='group of user ' + self.name).first()
        if g:
            return g
        raise TimDbException('Personal usergroup for user {} was not found!'.format(self.name))

    def get_personal_folder(self):
        path = 'users/' + self.name
        f = Folder.find_by_path(path)
        if f is None:
            return Folder.create(path, self.get_personal_group().id, apply_default_rights=True)
        return f

    def get_prefs(self) -> Dict:
        return json.loads(self.prefs)

    def set_prefs(self, prefs):
        self.prefs = json.dumps(prefs, cls=TimJsonEncoder)

    def to_json(self):
        return {'id': self.id,
                'name': self.name,
                'real_name': self.real_name,
                'email': self.email,
                'group': self.get_personal_group()
                }
