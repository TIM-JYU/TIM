import json
from datetime import datetime, timedelta, timezone
from enum import Enum
from typing import List
from typing import Optional, Union, Set

from sqlalchemy.orm import Query, joinedload
from sqlalchemy.orm.collections import attribute_mapped_collection

from timApp.answer.answer import Answer
from timApp.answer.answer_models import UserAnswer
from timApp.auth.accesstype import AccessType
from timApp.auth.auth_models import BlockAccess
from timApp.document.docinfo import DocInfo
from timApp.document.timjsonencoder import TimJsonEncoder
from timApp.folder.createopts import FolderCreationOptions
from timApp.folder.folder import Folder
from timApp.item.block import Block
from timApp.item.item import ItemBase
from timApp.lecture.lectureusers import LectureUsers
from timApp.notification.notification import Notification
from timApp.timdb.exceptions import TimDbException
from timApp.timdb.sqa import db, TimeStampMixin
from timApp.user.preferences import Preferences
from timApp.user.scimentity import SCIMEntity
from timApp.user.settings.theme import Theme
from timApp.user.special_group_names import ANONYMOUS_GROUPNAME, ANONYMOUS_USERNAME, LOGGED_IN_GROUPNAME, \
    SPECIAL_USERNAMES
from timApp.user.usergroup import UserGroup
from timApp.user.usergroupmember import UserGroupMember, membership_current, membership_deleted
from timApp.user.userutils import grant_access, get_access_type_id, \
    create_password_hash, check_password_hash, check_password_hash_old
from timApp.util.utils import remove_path_special_chars, cached_property, get_current_time

ItemOrBlock = Union[ItemBase, Block]
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

SCIM_USER_NAME = ':scimuser'

class Consent(Enum):
    CookieOnly = 1
    CookieAndData = 2


class UserOrigin(Enum):
    """Indicates how the user originally registered to TIM.

    Only Email, Korppi and Sisu are used so far; the others are speculative.
    """
    Email = 1
    Korppi = 2
    Sisu = 3
    Haka = 4
    OpenID = 5
    OpenIDConnect = 6
    Facebook = 7
    Google = 8
    Twitter = 9


def last_name_to_first(full_name: Optional[str]):
    """Converts a name of the form "Firstname Middlenames Lastname" to "Lastname Firstname Middlenames".
    """
    if full_name is None:
        return None
    names = full_name.split(' ')
    if len(names) > 1:
        return f'{names[-1]} {" ".join(names[:-1])}'
    return full_name


def last_name_to_last(full_name: Optional[str]):
    """Converts a name of the form "Lastname Firstname Middlenames" to "Firstname Middlenames Lastname".
    """
    if full_name is None:
        return None
    names = full_name.split(' ')
    if len(names) > 1:
        return f'{" ".join(names[1:])} {names[0]}'
    return full_name


class User(db.Model, TimeStampMixin, SCIMEntity):
    """A user account.

    A special user 'Anonymous user' denotes a user that is not logged in. Its id is 0.
    """
    __tablename__ = 'useraccount'
    id = db.Column(db.Integer, primary_key=True)
    """User identifier."""

    name = db.Column(db.Text, nullable=False, unique=True)
    """User name (not full name)."""

    given_name = db.Column(db.Text)
    last_name = db.Column(db.Text)

    real_name = db.Column(db.Text)
    """Real (full) name. This may be in the form "Lastname Firstname" or "Firstname Lastname"."""

    email = db.Column(db.Text, unique=True)
    """Email address."""

    prefs = db.Column(db.Text)
    """Preferences as a JSON string."""

    pass_ = db.Column('pass', db.Text)
    """Password hashed with bcrypt."""

    consent = db.Column(db.Enum(Consent), nullable=True)
    """Current consent for cookie/data collection."""

    origin = db.Column(db.Enum(UserOrigin), nullable=True)
    """How the user registered to TIM."""

    @property
    def scim_display_name(self):
        return last_name_to_last(self.real_name)

    @property
    def scim_created(self):
        return self.created

    @property
    def scim_modified(self):
        return self.modified

    @property
    def scim_id(self):
        return self.name

    @property
    def scim_resource_type(self):
        return 'User'

    @property
    def scim_extra_data(self):
        return {'emails': [{'value': self.email}] if self.email else []}

    consents = db.relationship('ConsentChange', back_populates='user', lazy='select')
    notifications = db.relationship('Notification', back_populates='user', lazy='dynamic')
    notifications_alt = db.relationship('Notification')

    groups = db.relationship(
        UserGroup,
        UserGroupMember.__table__,
        primaryjoin=(id == UserGroupMember.user_id) & membership_current,
        back_populates='users',
        lazy='joined',
    )
    groups_dyn = db.relationship(
        UserGroup,
        UserGroupMember.__table__,
        primaryjoin=id == UserGroupMember.user_id,
        lazy='dynamic',
    )
    groups_inactive = db.relationship(
        UserGroup,
        UserGroupMember.__table__,
        primaryjoin=(id == UserGroupMember.user_id) & membership_deleted,
        lazy='dynamic',
    )
    memberships_dyn = db.relationship(
        UserGroupMember,
        foreign_keys="UserGroupMember.user_id",
        lazy='dynamic',
    )
    memberships: List[UserGroupMember] = db.relationship(
        UserGroupMember,
        foreign_keys="UserGroupMember.user_id",
    )
    active_memberships = db.relationship(
        UserGroupMember,
        primaryjoin=(id == UserGroupMember.user_id) & membership_current,
        collection_class=attribute_mapped_collection("UserGroupMember.usergroup_id"),
        # back_populates="group",
    )
    lectures = db.relationship('Lecture', secondary=LectureUsers.__table__,
                               back_populates='users', lazy='dynamic')
    owned_lectures = db.relationship('Lecture', back_populates='owner', lazy='dynamic')
    owned_lectures_alt = db.relationship('Lecture')
    lectureanswers = db.relationship('LectureAnswer', back_populates='user', lazy='dynamic')
    lectureanswers_alt = db.relationship('LectureAnswer')
    messages = db.relationship('Message', back_populates='user', lazy='dynamic')
    messages_alt = db.relationship('Message')
    questionactivity = db.relationship('QuestionActivity', back_populates='user', lazy='select')
    useractivity = db.relationship('Useractivity', back_populates='user', lazy='select')
    answers = db.relationship('Answer', secondary=UserAnswer.__table__,
                              back_populates='users', lazy='dynamic')
    answers_alt = db.relationship('Answer', secondary=UserAnswer.__table__)
    annotations = db.relationship('Annotation', back_populates='annotator', lazy='dynamic')
    annotations_alt = db.relationship('Annotation')
    velps = db.relationship('Velp', back_populates='creator', lazy='dynamic')
    velps_alt = db.relationship('Velp')

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
        """Returns whether the user signed up via email and has not been "upgraded" to Korppi or Sisu user."""
        return '@' in self.name or self.name.startswith('testuser')

    @property
    def pretty_full_name(self):
        """Returns the user's full name."""

        if self.given_name and self.last_name:
            return f'{self.given_name} {self.last_name}'
        return self.real_name if self.real_name is not None else '(real_name is null)'

    @staticmethod
    def create(
            name: str,
            real_name: str,
            email: str,
            password: str = '',
            uid: Optional[int] = None,
            origin: Optional[UserOrigin] = None,
            given_name: Optional[str] = None,
            last_name: Optional[str] = None,
    ) -> 'User':
        """Creates a new user with the specified name.

        :param email: The email address of the user.
        :param name: The name of the user to be created.
        :param real_name: The real name of the user.
        :param password: The password for the user (not used on Korppi login).
        :returns: The id of the newly created user.

        """

        p_hash = create_password_hash(password) if password != '' else ''
        # noinspection PyArgumentList
        user = User(
            id=uid,
            name=name,
            real_name=real_name,
            last_name=last_name,
            given_name=given_name,
            email=email,
            pass_=p_hash,
            origin=origin,
        )
        db.session.add(user)
        return user

    @staticmethod
    def create_with_group(name: str,
                          real_name: Optional[str] = None,
                          email: Optional[str] = None,
                          password: Optional[str] = None,
                          is_admin: bool = False,
                          origin: UserOrigin = None,
                          uid: Optional[int] = None,
                          given_name = None,
                          last_name=None,
                          ):
        user = User.create(
            name,
            real_name,
            email,
            password=password or '',
            given_name=given_name,
            last_name=last_name,
            uid=uid,
            origin=origin,
        )
        group = UserGroup.create(name)
        user.groups.append(group)
        if is_admin:
            user.groups.append(UserGroup.get_admin_group())
        return user, group

    @staticmethod
    def get_by_name(name: str) -> Optional['User']:
        return User.query.filter_by(name=name).first()

    @staticmethod
    def get_by_id(uid: int) -> Optional['User']:
        return User.query.get(uid)

    @staticmethod
    def get_by_email(email: str) -> Optional['User']:
        return User.query.filter_by(email=email).first()

    @staticmethod
    def get_by_email_or_username(email_or_username: str) -> Optional['User']:
        u = User.get_by_email(email_or_username)
        if u:
            return u
        return User.get_by_name(email_or_username)

    @property
    def email_name_part(self):
        parts = self.email.split('@')
        return parts[0]

    @property
    def is_special(self):
        return self.name in SPECIAL_USERNAMES

    def check_password(self, password: str, allow_old=False, update_if_old=True) -> bool:
        if not self.pass_:
            return False
        is_ok = check_password_hash(password, self.pass_)
        if is_ok:
            return True
        if not allow_old:
            return False
        is_ok = check_password_hash_old(password, self.pass_)
        if is_ok and update_if_old:
            self.pass_ = create_password_hash(password)
        return is_ok

    def make_admin(self):
        self.groups.append(UserGroup.get_admin_group())

    def get_personal_group(self) -> UserGroup:
        return self.personal_group_prop

    @cached_property
    def personal_group_prop(self) -> UserGroup:
        if self.name == ANONYMOUS_USERNAME:
            return UserGroup.get_anonymous_group()
        for g in self.groups:
            if g.name == self.name:
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
                              self.get_personal_group(),
                              title=f"{self.real_name}",
                              creation_opts=FolderCreationOptions(apply_default_rights=True))
            db.session.commit()
            return f
        return folders[0]

    def get_prefs(self) -> Preferences:
        prefs = json.loads(self.prefs or '{}')
        try:
            return Preferences.from_json(prefs)
        except TypeError:
            return Preferences()

    def set_prefs(self, prefs: Preferences):
        css_files = prefs.css_files
        existing_css_files = {}
        for k, v in css_files.items():
            t = Theme(k)
            if t.exists() and v:
                existing_css_files[t.filename] = True
        prefs.css_files = existing_css_files
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

    def add_to_group(self, ug: UserGroup, added_by: 'User'):
        existing: UserGroupMember = self.id is not None and self.memberships_dyn.filter_by(group=ug).first()
        if existing:
            existing.membership_end = None
            existing.adder = added_by
            return False
        else:
            self.memberships.append(UserGroupMember(group=ug, adder=added_by))
            return True

    @staticmethod
    def get_scimuser():
        u = User.get_by_name(SCIM_USER_NAME)
        if not u:
            u, _ = User.create_with_group(name=SCIM_USER_NAME, real_name='Scim User', email='scimuser@example.com')
        return u

    def update_info(
            self,
            name: str,
            real_name: str,
            email: str,
            password: Optional[str] = None,
            given_name: Optional[str]=None,
            last_name: Optional[str]=None,
    ):
        if self.name != name:
            group = self.get_personal_group()
            self.name = name
            group.name = name
        if given_name and last_name:
            self.given_name = given_name
            self.last_name = last_name
        self.real_name = real_name
        self.email = email
        if password:
            self.pass_ = create_password_hash(password)

    def has_some_access(self, i: ItemOrBlock, vals: Set[int], allow_admin: bool = True) -> Optional[BlockAccess]:
        if allow_admin and self.is_admin:
            return BlockAccess(block_id=i.id,
                               accessible_from=datetime.min.replace(tzinfo=timezone.utc),
                               type=AccessType.owner.value,
                               usergroup_id=self.get_personal_group().id)
        if isinstance(i, ItemBase):
            b = i.block
        else:
            b = i
        if not b:
            return None
        now = get_current_time()
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

    def has_ownership(self, i: ItemOrBlock, allow_admin: bool = True) -> Optional[BlockAccess]:
        return self.has_some_access(i, owner_access_set, allow_admin)

    def can_write_to_folder(self, f: Folder):
        # not even admins are allowed to create new items in 'users' folder
        if f.path == 'users':
            return False
        return self.has_edit_access(f)

    def grant_access(self, block: ItemOrBlock,
                     access_type: str,
                     accessible_from: Optional[datetime] = None,
                     accessible_to: Optional[datetime] = None,
                     duration_from: Optional[datetime] = None,
                     duration_to: Optional[datetime] = None,
                     duration: Optional[timedelta] = None,
                     commit: bool = True):
        return grant_access(group=self.get_personal_group(),
                            block=block,
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
        if not any((doc_modify, comment_add, comment_modify)):
            db.session.delete(n)

    def get_answers_for_task(self, task_id: str):
        return self.answers.options(joinedload(Answer.users_all)).order_by(Answer.id.desc()).filter_by(task_id=task_id)

    @property
    def basic_info_dict(self):
        if not getattr(self, 'hide_name', False):
            return {
                'id': self.id,
                'name': self.name,
                'real_name': self.real_name,
                'email': self.email,
            }
        else:
            return {
                'id': self.id,
                'name': f'user{self.id}',
                'real_name': f'User {self.id}',
                'email': f'user{self.id}@example.com',
            }

    def to_json(self, full=False):
        return {**self.basic_info_dict,
                'group': self.get_personal_group(),
                'groups': self.groups,
                'folder': self.get_personal_folder(),
                'consent': self.consent,
                } if full else self.basic_info_dict


def get_membership_end(u: User, group_ids: Set[int]):
    relevant_memberships: List[UserGroupMember] = [m for m in u.memberships if m.usergroup_id in group_ids]
    membership_end = None
    # If the user is not active in any of the groups, we'll show the lastly-ended membership.
    # TODO: It might be possible in the future that the membership_end is in the future.
    if all(m.membership_end is not None for m in relevant_memberships):
        membership_end = (
            max(m.membership_end for m in relevant_memberships)
        )
    return membership_end
