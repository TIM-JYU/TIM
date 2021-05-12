import json
from dataclasses import dataclass, field
from datetime import datetime, timedelta, timezone
from enum import Enum
from typing import List, Tuple, Dict
from typing import Optional, Union, Set

from sqlalchemy import func
from sqlalchemy.orm import Query, joinedload, defaultload
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
from timApp.messaging.timMessage.internalmessage_models import InternalMessageReadReceipt
from timApp.notification.notification import Notification
from timApp.timdb.exceptions import TimDbException
from timApp.timdb.sqa import db, TimeStampMixin
from timApp.user.hakaorganization import HakaOrganization
from timApp.user.personaluniquecode import SchacPersonalUniqueCode, PersonalUniqueCode
from timApp.user.preferences import Preferences
from timApp.user.scimentity import SCIMEntity
from timApp.user.settings.theme import Theme
from timApp.user.special_group_names import ANONYMOUS_GROUPNAME, ANONYMOUS_USERNAME, LOGGED_IN_GROUPNAME, \
    SPECIAL_USERNAMES
from timApp.user.usergroup import UserGroup, get_logged_in_group_id, get_anonymous_group_id
from timApp.user.usergroupmember import UserGroupMember, membership_current, membership_deleted
from timApp.user.userutils import grant_access, get_access_type_id, \
    create_password_hash, check_password_hash, check_password_hash_old
from timApp.util.utils import remove_path_special_chars, cached_property, get_current_time

ItemOrBlock = Union[ItemBase, Block]
maxdate = datetime.max.replace(tzinfo=timezone.utc)

view_access_set = {t.value for t in [
    AccessType.view,
    AccessType.copy,
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

copy_access_set = {t.value for t in [
    AccessType.copy,
    AccessType.edit,
    AccessType.owner,
    AccessType.manage,
]}

access_sets = {
    AccessType.copy: copy_access_set,
    AccessType.edit: edit_access_set,
    AccessType.manage: manage_access_set,
    AccessType.owner: owner_access_set,
    AccessType.see_answers: seeanswers_access_set,
    AccessType.teacher: teacher_access_set,
    AccessType.view: view_access_set,
}

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


@dataclass
class UserInfo:
    username: Optional[str] = None
    email: Optional[str] = None
    full_name: Optional[str] = None
    given_name: Optional[str] = None
    last_name: Optional[str] = None
    origin: Optional[UserOrigin] = None
    password: Optional[str] = None
    password_hash: Optional[str] = None
    unique_codes: List[SchacPersonalUniqueCode] = field(default_factory=list)

    def __post_init__(self):
        assert self.password is None or self.password_hash is None, 'Cannot pass both password and password_hash to UserInfo'


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


deleted_user_suffix = '_deleted'


def user_query_with_joined_groups() -> Query:
    return User.query.options(joinedload(User.groups))


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

    uniquecodes = db.relationship(
        'PersonalUniqueCode',
        back_populates='user',
        collection_class=attribute_mapped_collection('user_collection_key'),
    )

    internalmessage_readreceipt: Optional[InternalMessageReadReceipt] = db.relationship('InternalMessageReadReceipt',
                                                                                        back_populates='user')

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

    groups: List[UserGroup] = db.relationship(
        UserGroup,
        UserGroupMember.__table__,
        primaryjoin=(id == UserGroupMember.user_id) & membership_current,
        back_populates='users',
        lazy='select',
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
                               back_populates='users', lazy='select')
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

    def __repr__(self):
        return f'<User(id={self.id}, name={self.name}, email={self.email}, real_name={self.real_name})>'

    @property
    def logged_in(self):
        return self.id > 0

    @property
    def is_deleted(self):
        return self.name.endswith(deleted_user_suffix) and self.email.endswith(deleted_user_suffix)

    @property
    def group_ids(self):
        return set(g.id for g in self.groups)

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
        if self.is_name_hidden:
            return f'User {self.id}'
        if self.given_name and self.last_name:
            return f'{self.given_name} {self.last_name}'
        if self.real_name is None:
            return '(real_name is null)'
        parts = self.real_name.split(' ')
        if len(parts) == 1:
            return self.real_name
        return ' '.join(parts[1:]) + ' ' + parts[0]

    @staticmethod
    def create_with_group(
            info: UserInfo,
            is_admin: bool = False,
            uid: Optional[int] = None,
    ) -> Tuple['User', UserGroup]:
        p_hash = create_password_hash(info.password) if info.password is not None else info.password_hash
        user = User(
            id=uid,
            name=info.username,
            real_name=info.full_name,
            last_name=info.last_name,
            given_name=info.given_name,
            email=info.email,
            pass_=p_hash,
            origin=info.origin,
        )
        db.session.add(user)
        user.set_unique_codes(info.unique_codes)
        group = UserGroup.create(info.username)
        user.groups.append(group)
        if is_admin:
            user.make_admin()
        return user, group

    @staticmethod
    def get_by_name(name: str) -> Optional['User']:
        return user_query_with_joined_groups().filter_by(name=name).first()

    @staticmethod
    def get_by_id(uid: int) -> Optional['User']:
        return user_query_with_joined_groups().get(uid)

    @staticmethod
    def get_by_email(email: str) -> Optional['User']:
        if email is None:
            raise Exception('Tried to find an user by null email')
        return user_query_with_joined_groups().filter_by(email=email).first()

    @staticmethod
    def get_by_email_case_insensitive(email: str) -> List['User']:
        return user_query_with_joined_groups().filter(func.lower(User.email).in_([email])).all()

    @staticmethod
    def get_by_email_case_insensitive_or_username(email_or_username: str) -> List['User']:
        users = User.get_by_email_case_insensitive(email_or_username)
        if users:
            return users
        u = User.get_by_name(email_or_username)
        if u:
            return [u]
        return []

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
        ag = UserGroup.get_admin_group()
        if ag not in self.groups:
            self.groups.append(ag)

    def get_personal_group(self) -> UserGroup:
        return self.personal_group_prop

    @cached_property
    def personal_group_prop(self) -> UserGroup:
        group_to_find = self.name
        if self.name == ANONYMOUS_USERNAME:
            group_to_find = ANONYMOUS_GROUPNAME
        for g in self.groups:
            if g.name == group_to_find:
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

    def get_personal_folder(self) -> Folder:
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
        ).with_entities(Folder).options(
            defaultload(Folder._block)
                .joinedload(Block.accesses)
                .joinedload(BlockAccess.usergroup)
        ).all()
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

    def add_to_group(self, ug: UserGroup, added_by: Optional['User']) -> bool:
        # Local import to avoid cyclical importing.
        from timApp.messaging.messagelist.messagelist_utils import sync_message_list_on_add
        existing: UserGroupMember = self.id is not None and self.memberships_dyn.filter_by(group=ug).first()
        if existing:
            existing.membership_end = None
            existing.adder = added_by
            new_add = False
        else:
            self.memberships.append(UserGroupMember(group=ug, adder=added_by))
            new_add = True

        # TODO: Enable syncing when the syncing is fixed.
        # On changing of group, sync this person to the user goup's message lists.
        # sync_message_list_on_add(self, ug)
        return new_add

    @staticmethod
    def get_scimuser() -> 'User':
        u = User.get_by_name(SCIM_USER_NAME)
        if not u:
            u, _ = User.create_with_group(UserInfo(
                username=SCIM_USER_NAME,
                full_name='Scim User',
                email='scimuser@example.com',
            ))
        return u

    @staticmethod
    def get_anon() -> 'User':
        return User.get_by_id(0)

    def update_info(
            self,
            info: UserInfo,
    ):
        if info.username and self.name != info.username:
            group = self.get_personal_group()
            self.name = info.username
            group.name = info.username
        if info.given_name:
            self.given_name = info.given_name
        if info.last_name:
            self.last_name = info.last_name
        if info.full_name:
            self.real_name = info.full_name
        if info.email:
            self.email = info.email
        if info.password:
            self.pass_ = create_password_hash(info.password)
        elif info.password_hash:
            self.pass_ = info.password_hash
        self.set_unique_codes(info.unique_codes)

    def set_unique_codes(self, codes: List[SchacPersonalUniqueCode]):
        for c in codes:
            ho = HakaOrganization.get_or_create(name=c.org)
            if ho.id is None or self.id is None:
                db.session.flush()
            puc = PersonalUniqueCode(
                code=c.code,
                type=c.codetype,
                org_id=ho.id,
            )
            if puc.user_collection_key not in self.uniquecodes:
                self.uniquecodes[puc.user_collection_key] = puc

    def has_some_access(
            self,
            i: ItemOrBlock,
            vals: Set[int],
            allow_admin: bool = True,
            grace_period: timedelta = timedelta(seconds=0),
    ) -> Optional[BlockAccess]:
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
        best_access = None
        curr_group_ids = self.group_ids
        for a in b.accesses.values():  # type: BlockAccess
            if a.usergroup_id not in curr_group_ids:
                if self.logged_in and a.usergroup_id == get_logged_in_group_id():
                    pass
                elif a.usergroup_id == get_anonymous_group_id():
                    pass
                else:
                    continue
            if a.type not in vals:
                continue
            to_time = a.accessible_to
            if to_time is not None:
                to_time += grace_period
            if (a.accessible_from or maxdate) <= now < (to_time or maxdate):
                # If the end time of the access is unrestricted, there is no better access.
                if to_time is None:
                    return a
                # If the end time of the access is restricted, there might be a better access,
                # so we'll continue looping.
                if best_access is None or best_access.accessible_to < a.accessible_to:
                    best_access = a
        return best_access

    def has_access(
            self,
            i: ItemOrBlock,
            access: AccessType,
            grace_period=timedelta(seconds=0),
    ) -> Optional[BlockAccess]:
        return self.has_some_access(i, access_sets[access], grace_period=grace_period)

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

    def has_copy_access(self, i: ItemOrBlock) -> Optional[BlockAccess]:
        return self.has_some_access(i, copy_access_set)

    def has_ownership(self, i: ItemOrBlock, allow_admin: bool = True) -> Optional[BlockAccess]:
        return self.has_some_access(i, owner_access_set, allow_admin)

    def can_write_to_folder(self, f: Folder):
        # not even admins are allowed to create new items in 'users' folder
        if f.path == 'users':
            return False
        return self.has_edit_access(f)

    def grant_access(self, block: ItemOrBlock,
                     access_type: AccessType,
                     accessible_from: Optional[datetime] = None,
                     accessible_to: Optional[datetime] = None,
                     duration_from: Optional[datetime] = None,
                     duration_to: Optional[datetime] = None,
                     duration: Optional[timedelta] = None,
                     require_confirm: Optional[bool] = None):
        return grant_access(group=self.get_personal_group(),
                            block=block,
                            access_type=access_type,
                            accessible_from=accessible_from,
                            accessible_to=accessible_to,
                            duration_from=duration_from,
                            duration_to=duration_to,
                            duration=duration,
                            require_confirm=require_confirm)

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
    def is_name_hidden(self):
        return getattr(self, 'hide_name', False)

    @property
    def basic_info_dict(self):
        if not self.is_name_hidden:
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

    def to_json(self, full: bool=False) -> Dict:
        return {**self.basic_info_dict,
                'group': self.get_personal_group(),
                'groups': self.groups,
                'folder': self.get_personal_folder() if self.logged_in else None,
                'consent': self.consent,
                'last_name': self.last_name,
                } if full else self.basic_info_dict

    @cached_property
    def bookmarks(self):
        from timApp.bookmark.bookmarks import Bookmarks
        return Bookmarks(self)

    def belongs_to_any_of(self, *groups: UserGroup):
        return bool(set(groups) & set(self.groups))


def get_membership_end(u: User, group_ids: Set[int]):
    relevant_memberships: List[UserGroupMember] = [m for m in u.memberships if m.usergroup_id in group_ids]
    membership_end = None
    # If the user is not active in any of the groups, we'll show the lastly-ended membership.
    # TODO: It might be possible in the future that the membership_end is in the future.
    if relevant_memberships and all(m.membership_end is not None for m in relevant_memberships):
        membership_end = (
            max(m.membership_end for m in relevant_memberships)
        )
    return membership_end


def check_rights(hide_type: str, rights: dict):
    """
    Checks whether the user has the correct rights rights not to hide links or the buttons in the top of the
    page from them.

    :param hide_type What elements to hide in the document.
    :param rights Which user roles the elements should be hidden from.
    :return Should the elements be hidden from the user.
    """
    return {'view': not rights['editable'] and not rights['see_answers'],
            'edit': not rights['see_answers'],
            'see_answers': not rights['teacher'],
            'teacher': not rights['manage']}.get(hide_type, False)


def get_owned_objects_query(u: User):
    return u.get_personal_group().accesses.filter_by(type=AccessType.owner.value).with_entities(
        BlockAccess.block_id)
