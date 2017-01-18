import json

from typing import Dict

from sqlalchemy.orm import Query

from documentmodel.timjsonencoder import TimJsonEncoder
from timdb.tim_models import db, UserGroupMember
from timdb.models.folder import Folder
from timdb.models.usergroup import UserGroup
from timdb.special_group_names import ANONYMOUS_GROUPNAME, ANONYMOUS_USERNAME, LOGGED_IN_GROUPNAME
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

    @property
    def logged_in(self):
        return self.id > 0

    @property
    def is_admin(self):
        if not hasattr(self, '_is_admin'):
            q = UserGroupMember.query.join(UserGroup, UserGroupMember.usergroup_id == UserGroup.id).filter(
                (UserGroupMember.user_id == self.id) & (UserGroup.name == 'Administrators'))
            self._is_admin = db.session.query(q.exists()).scalar()
        return self._is_admin

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

    def get_groups(self) -> Query:
        special_groups = [ANONYMOUS_GROUPNAME]
        if self.logged_in:
            special_groups.append(LOGGED_IN_GROUPNAME)
        return UserGroup.query.filter(
            UserGroup.id.in_(db.session.query(UserGroupMember.usergroup_id).filter_by(user_id=self.id))
        ).union(
            UserGroup.query.filter(UserGroup.name.in_(special_groups))
        )

    def to_json(self):
        return {'id': self.id,
                'name': self.name,
                'real_name': self.real_name,
                'email': self.email,
                'group': self.get_personal_group()
                }
