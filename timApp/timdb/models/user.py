import json
import os
from datetime import datetime, timedelta, timezone
from typing import Dict, Optional, Union, Set
from typing import List

from flask import current_app
from sqlalchemy.orm import Query

from timApp.documentmodel.timjsonencoder import TimJsonEncoder
from timApp.theme import Theme
from timApp.timdb.accesstype import AccessType
from timApp.timdb.docinfo import DocInfo
from timApp.timdb.exceptions import TimDbException
from timApp.timdb.item import Item
from timApp.timdb.models.block import Block
from timApp.timdb.models.folder import Folder
from timApp.timdb.models.notification import Notification
from timApp.timdb.models.usergroup import UserGroup
from timApp.timdb.special_group_names import ANONYMOUS_GROUPNAME, ANONYMOUS_USERNAME, LOGGED_IN_GROUPNAME
from timApp.timdb.tim_models import db, UserGroupMember, BlockAccess
from timApp.timdb.userutils import get_viewable_blocks, get_accessible_blocks, grant_access, get_access_type_id, \
    create_password_hash, check_password_hash
from timApp.utils import remove_path_special_chars, generate_theme_scss, ThemeNotFoundException, \
    get_combined_css_filename, cached_property

ItemOrBlock = Union[Item, Block]
maxdate = datetime.max.replace(tzinfo=timezone.utc)

view_access_set = {t.value for t in [
    AccessType.view,
    AccessType.edit,
    AccessType.owner,
    AccessType.teacher,
    AccessType.see_answers,
    AccessType.manage,
]}

edit_access_set = {t.value for t in [
    AccessType.edit,
    AccessType.owner,
    AccessType.manage,
]}

manage_access_set = {t.value for t in [
    AccessType.owner,
    AccessType.manage,
]}

owner_access_set = {t.value for t in [
    AccessType.owner,
]}

teacher_access_set = {t.value for t in [
    AccessType.owner,
    AccessType.manage,
    AccessType.teacher,
]}

seeanswers_access_set = {t.value for t in [
    AccessType.owner,
    AccessType.teacher,
    AccessType.see_answers,
    AccessType.manage,
]}


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

    notifications = db.relationship('Notification', back_populates='user', lazy='dynamic')
    groups = db.relationship('UserGroup', secondary=UserGroupMember.__table__,
                             back_populates='users', lazy='joined')

    @property
    def logged_in(self):
        return self.id > 0

    @cached_property
    def is_admin(self):
        for g in self.groups:
            if g.name == 'Administrators':
                return True
        return False

    @property
    def is_email_user(self):
        """Returns whether the user signed up via email."""
        return '@' in self.name or self.name.startswith('testuser')

    @property
    def pretty_full_name(self):
        """Returns the user's full name in the form "Firstname Lastname"."""

        # Email users can type their name themselves and usually the first name is typed first
        # and no secondary names are given, so we return the real name as is.
        if self.is_email_user:
            return self.real_name

        # In case of a Korppi user, the last name is always the first.
        names = self.real_name.split(' ')
        if len(names) > 1:
            return names[1] + ' ' + names[0]

        # In case there is only one name for a Korppi user, return the name as is.
        return self.real_name

    @staticmethod
    def create(name: str, real_name: str, email: str, password: str = '',
               commit: bool = True) -> 'User':
        """Creates a new user with the specified name.

        :param email: The email address of the user.
        :param name: The name of the user to be created.
        :param real_name: The real name of the user.
        :param password: The password for the user (not used on Korppi login).
        :returns: The id of the newly created user.

        """

        p_hash = create_password_hash(password) if password != '' else ''
        user = User(name=name, real_name=real_name, email=email, pass_=p_hash)
        db.session.add(user)
        db.session.flush()
        if commit:
            db.session.commit()
        assert user.id != 0
        return user

    @staticmethod
    def create_with_group(name: str,
                          real_name: Optional[str] = None,
                          email: Optional[str] = None,
                          password: Optional[str] = None,
                          is_admin: bool = False):
        user = User.create(name, real_name or name, email or name + '@example.com', password=password or '',
                           commit=False)
        group = UserGroup.create(name, commit=False)
        user.groups.append(group)
        if is_admin:
            user.groups.append(UserGroup.get_admin_group())
        return user, group

    @staticmethod
    def get_by_name(name: str) -> Optional['User']:
        return User.query.filter_by(name=name).first()

    @staticmethod
    def get_by_email(email: str) -> Optional['User']:
        return User.query.filter_by(email=email).first()

    def check_password(self, password: str, allow_old=False, update_if_old=True) -> bool:
        is_ok = check_password_hash(password, self.pass_, allow_old=False)
        if is_ok:
            return True
        if not allow_old:
            return False
        is_ok = check_password_hash(password, self.pass_, allow_old=True)
        if is_ok and update_if_old:
            self.pass_ = create_password_hash(password)
        return is_ok

    def make_admin(self):
        self.groups.append(UserGroup.get_admin_group())

    def get_personal_group(self) -> UserGroup:
        return self.personal_group_prop

    @cached_property
    def personal_group_prop(self) -> UserGroup:
        if self.id < 0 or self.name == ANONYMOUS_USERNAME:
            return UserGroup.get_anonymous_group()
        for g in self.groups:
            if g.name == self.name or g.name == 'group of user ' + self.name:
                return g
        raise TimDbException(f'Personal usergroup for user {self.name} was not found!')

    def derive_personal_folder_name(self):
        real_name = self.real_name
        if not real_name:
            real_name = "anonymous"
        basename = remove_path_special_chars(real_name).lower()
        index = ''
        while Folder.find_by_path('users/' + basename + index):
            index = str(int(index or 1) + 1)
        return basename + index

    def get_personal_folder(self):
        return self.personal_folder_prop

    @cached_property
    def personal_folder_prop(self) -> Folder:
        if self.logged_in:
            group_condition = UserGroup.name == self.name
        else:
            group_condition = UserGroup.name == ANONYMOUS_GROUPNAME
        folders: List[Folder] = Folder.query.join(
            BlockAccess, BlockAccess.block_id == Folder.id
        ).join(
            UserGroup, UserGroup.id == BlockAccess.usergroup_id
        ).filter(
            (Folder.location == 'users') &
            group_condition &
            (BlockAccess.type == AccessType.owner.value)
        ).with_entities(Folder).all()
        if len(folders) >= 2:
            raise TimDbException(f'Found multiple personal folders for user {self.name}: {[f.name for f in folders]}')
        if not folders:
            f = Folder.create('users/' + self.derive_personal_folder_name(),
                              self.get_personal_group().id,
                              title=f"{self.real_name}",
                              apply_default_rights=True)
            db.session.commit()
            return f
        return folders[0]

    def get_prefs(self) -> Dict:
        prefs = json.loads(self.prefs or '{}')
        if not prefs.get('css_files'):
            prefs['css_files'] = {}
        if not prefs.get('custom_css'):
            prefs['custom_css'] = ''
        css_file_list = [css for css, v in prefs['css_files'].items() if v]
        css_file_list.sort()
        theme_list = [Theme(f) for f in css_file_list]
        try:
            generate_theme_scss(theme_list, os.path.join('static', current_app.config['SASS_GEN_PATH']))
        except ThemeNotFoundException:
            self.set_prefs(prefs)
            return self.get_prefs()
        prefs['css_combined'] = get_combined_css_filename(theme_list)
        return prefs

    def set_prefs(self, prefs: Dict):
        css_files = prefs.get('css_files', {})
        existing_css_files = {}
        for k, v in css_files.items():
            t = Theme(k)
            if t.exists() and v:
                existing_css_files[t.filename] = True
        prefs['css_files'] = existing_css_files
        self.prefs = json.dumps(prefs, cls=TimJsonEncoder)

    def get_groups(self, include_special=True) -> Query:
        special_groups = [ANONYMOUS_GROUPNAME]
        if self.logged_in:
            special_groups.append(LOGGED_IN_GROUPNAME)
        q = UserGroup.query.filter(
            UserGroup.id.in_(db.session.query(UserGroupMember.usergroup_id).filter_by(user_id=self.id))
        )
        if include_special:
            q = q.union(
                UserGroup.query.filter(UserGroup.name.in_(special_groups))
            )
        return q

    def update_info(self, name: str, real_name: str, email: str, password: Optional[str] = None):
        group = self.get_personal_group()
        self.name = name
        group.name = name
        self.real_name = real_name
        self.email = email
        if password:
            self.pass_ = create_password_hash(password)

    def has_some_access(self, i: ItemOrBlock, vals: Set[int]) -> Optional[BlockAccess]:
        if self.is_admin:
            return BlockAccess(block_id=i.id,
                               accessible_from=datetime.min.replace(tzinfo=timezone.utc),
                               type=AccessType.owner.value,
                               usergroup_id=self.get_personal_group().id)
        if isinstance(i, Item):
            b = i.block
        else:
            b = i
        if not b:
            return None
        now = datetime.now(tz=timezone.utc)
        for a in b.accesses:  # type: BlockAccess
            if a.usergroup not in self.groups:
                name = a.usergroup.name
                if self.logged_in and name == LOGGED_IN_GROUPNAME:
                    pass
                elif name == ANONYMOUS_GROUPNAME:
                    pass
                else:
                    continue
            if a.type not in vals:
                continue
            if (a.accessible_from or maxdate) <= now < (a.accessible_to or maxdate):
                return a
        return None

    def has_view_access(self, i: ItemOrBlock) -> Optional[BlockAccess]:
        return self.has_some_access(i, view_access_set)

    def has_edit_access(self, i: ItemOrBlock) -> Optional[BlockAccess]:
        return self.has_some_access(i, edit_access_set)

    def has_manage_access(self, i: ItemOrBlock) -> Optional[BlockAccess]:
        return self.has_some_access(i, manage_access_set)

    def has_teacher_access(self, i: ItemOrBlock) -> Optional[BlockAccess]:
        return self.has_some_access(i, teacher_access_set)

    def has_seeanswers_access(self, i: ItemOrBlock) -> Optional[BlockAccess]:
        return self.has_some_access(i, seeanswers_access_set)

    def has_ownership(self, i: ItemOrBlock) -> Optional[BlockAccess]:
        return self.has_some_access(i, owner_access_set)

    def can_write_to_folder(self, f: Folder):
        # not even admins are allowed to create new items in 'users' folder
        if f.path == 'users':
            return False
        return self.has_edit_access(f)

    def grant_access(self, block_id: int,
                     access_type: str,
                     accessible_from: Optional[datetime] = None,
                     accessible_to: Optional[datetime] = None,
                     duration_from: Optional[datetime] = None,
                     duration_to: Optional[datetime] = None,
                     duration: Optional[timedelta] = None,
                     commit: bool = True):
        return grant_access(group_id=self.get_personal_group().id,
                            block_id=block_id,
                            access_type=access_type,
                            accessible_from=accessible_from,
                            accessible_to=accessible_to,
                            duration_from=duration_from,
                            duration_to=duration_to,
                            duration=duration,
                            commit=commit)

    def remove_access(self, block_id: int, access_type: str):
        BlockAccess.query.filter_by(block_id=block_id,
                                    usergroup_id=self.get_personal_group().id,
                                    type=get_access_type_id(access_type)).delete()

    def get_viewable_blocks(self) -> Dict[int, BlockAccess]:
        return get_viewable_blocks(self.id)

    def get_accessible_blocks(self, access_types: List[int]) -> Dict[int, BlockAccess]:
        return get_accessible_blocks(self.id, access_types)

    def get_notify_settings(self, doc: DocInfo):
        n = self.notifications.filter_by(doc_id=doc.id).first()
        if not n:
            n = Notification(doc_id=doc.id,
                             user_id=self.id,
                             email_doc_modify=False,
                             email_comment_add=False,
                             email_comment_modify=False
                             )
            db.session.add(n)
        return n

    def set_notify_settings(self, doc: DocInfo, doc_modify: bool, comment_add: bool, comment_modify: bool):
        n = self.get_notify_settings(doc)
        n.email_comment_add = comment_add
        n.email_doc_modify = doc_modify
        n.email_comment_modify = comment_modify

    @property
    def basic_info_dict(self):
        return {'id': self.id,
                'name': self.name,
                'real_name': self.real_name,
                'email': self.email}

    def to_json(self):
        return {**self.basic_info_dict,
                'group': self.get_personal_group(),
                'folder': self.get_personal_folder()
                }
