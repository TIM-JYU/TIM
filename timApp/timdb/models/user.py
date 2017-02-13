import json

from typing import Dict
from typing import List

from sqlalchemy.orm import Query

from documentmodel.timjsonencoder import TimJsonEncoder
from timdb.accesstype import AccessType
from timdb.tim_models import db, UserGroupMember, BlockAccess
from timdb.models.folder import Folder
from timdb.models.usergroup import UserGroup
from timdb.special_group_names import ANONYMOUS_GROUPNAME, ANONYMOUS_USERNAME, LOGGED_IN_GROUPNAME
from timdb.timdbexception import TimDbException
from utils import remove_path_special_chars


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

    def derive_personal_folder_name(self):
        real_name = self.real_name
        if not real_name:
            real_name = "anonymous"
        basename = remove_path_special_chars(real_name).lower()
        index = ''
        while Folder.find_by_path('users/' + basename + index):
            index = str(int(index or 1) + 1)
        return basename + index

    def get_personal_folder(self) -> Folder:
        if self.logged_in:
            group_condition = UserGroup.name == self.name
        else:
            group_condition = UserGroup.name == ANONYMOUS_GROUPNAME
        folders = Folder.query.join(
            BlockAccess, BlockAccess.block_id == Folder.id
        ).join(
            UserGroup, UserGroup.id == BlockAccess.usergroup_id
        ).filter(
            (Folder.location == 'users') &
            group_condition &
            (BlockAccess.type == AccessType.owner.value)
        ).with_entities(Folder).all()  # type: List[Folder]
        if len(folders) >= 2:
            raise TimDbException('Found multiple personal folders for user {}: {}'.format(
                self.name, [f.name for f in folders]))
        if not folders:
            return Folder.create('users/' + self.derive_personal_folder_name(),
                                 self.get_personal_group().id,
                                 title="{}".format(self.real_name),
                                 apply_default_rights=True)
        return folders[0]

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
                'group': self.get_personal_group(),
                'folder': self.get_personal_folder()
                }
