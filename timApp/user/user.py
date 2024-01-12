import json
import re
from dataclasses import dataclass, field
from datetime import datetime, timedelta, timezone
from enum import Enum
from typing import Optional, Union, Dict, Tuple, TYPE_CHECKING, List

import filelock
from flask import current_app, has_request_context
from sqlalchemy import func, select, delete
from sqlalchemy.ext.hybrid import hybrid_property
from sqlalchemy.orm import (
    mapped_column,
    Mapped,
    attribute_keyed_dict,
    relationship,
    DynamicMapped,
)
from sqlalchemy.orm import (
    selectinload,
    defaultload,
)
from sqlalchemy.orm.interfaces import LoaderOption
from sqlalchemy.sql import Select

from timApp.answer.answer import Answer
from timApp.answer.answer_models import UserAnswer
from timApp.auth.access.util import get_locked_access_type, get_locked_active_groups
from timApp.auth.accesstype import AccessType
from timApp.auth.auth_models import BlockAccess
from timApp.auth.get_user_rights_for_item import UserItemRights
from timApp.auth.session.model import UserSession
from timApp.document.docinfo import DocInfo
from timApp.folder.createopts import FolderCreationOptions
from timApp.folder.folder import Folder
from timApp.item.block import Block
from timApp.item.item import ItemBase
from timApp.lecture.lectureusers import LectureUsers
from timApp.messaging.messagelist.listinfo import Channel
from timApp.notification.notification import Notification, NotificationType
from timApp.sisu.scimusergroup import ScimUserGroup
from timApp.timdb.exceptions import TimDbException
from timApp.timdb.sqa import db, TimeStampMixin, is_attribute_loaded, run_sql
from timApp.user.hakaorganization import HakaOrganization, get_home_organization_id
from timApp.user.personaluniquecode import SchacPersonalUniqueCode, PersonalUniqueCode
from timApp.user.preferences import Preferences
from timApp.user.scimentity import SCIMEntity
from timApp.user.special_group_names import (
    ANONYMOUS_GROUPNAME,
    ANONYMOUS_USERNAME,
    LOGGED_IN_GROUPNAME,
    SPECIAL_USERNAMES,
    LOGGED_IN_USERNAME,
)
from timApp.user.usercontact import (
    UserContact,
    PrimaryContact,
    ContactOrigin,
    NO_AUTO_VERIFY_ORIGINS,
)
from timApp.user.usergroup import (
    UserGroup,
    get_admin_group_id,
    get_anonymous_group_id,
    get_logged_in_group_id,
)
from timApp.user.usergroupmember import (
    UserGroupMember,
    membership_current,
    membership_deleted,
)
from timApp.user.userutils import (
    grant_access,
    get_access_type_id,
    create_password_hash,
    check_password_hash,
    check_password_hash_old,
)
from timApp.util.utils import (
    remove_path_special_chars,
    cached_property,
    get_current_time,
)
from tim_common.timjsonencoder import TimJsonEncoder

if TYPE_CHECKING:
    from timApp.messaging.timMessage.internalmessage_models import (
        InternalMessageReadReceipt,
    )
    from timApp.user.consentchange import ConsentChange
    from timApp.lecture.lecture import Lecture
    from timApp.lecture.lectureanswer import LectureAnswer
    from timApp.lecture.message import Message
    from timApp.lecture.questionactivity import QuestionActivity
    from timApp.lecture.useractivity import UserActivity
    from timApp.velp.annotation_model import Annotation
    from timApp.velp.velp_models import Velp


ItemOrBlock = Union[ItemBase, Block]
maxdate = datetime.max.replace(tzinfo=timezone.utc)

view_access_set = {
    t.value
    for t in [
        AccessType.view,
        AccessType.copy,
        AccessType.edit,
        AccessType.owner,
        AccessType.teacher,
        AccessType.see_answers,
        AccessType.manage,
    ]
}

edit_access_set = {
    t.value
    for t in [
        AccessType.edit,
        AccessType.owner,
        AccessType.manage,
    ]
}

manage_access_set = {
    t.value
    for t in [
        AccessType.owner,
        AccessType.manage,
    ]
}

owner_access_set = {
    t.value
    for t in [
        AccessType.owner,
    ]
}

teacher_access_set = {
    t.value
    for t in [
        AccessType.owner,
        AccessType.manage,
        AccessType.teacher,
    ]
}

seeanswers_access_set = {
    t.value
    for t in [
        AccessType.owner,
        AccessType.teacher,
        AccessType.see_answers,
        AccessType.manage,
    ]
}

copy_access_set = {
    t.value
    for t in [
        AccessType.copy,
        AccessType.edit,
        AccessType.owner,
        AccessType.manage,
    ]
}

access_sets = {
    AccessType.copy: copy_access_set,
    AccessType.edit: edit_access_set,
    AccessType.manage: manage_access_set,
    AccessType.owner: owner_access_set,
    AccessType.see_answers: seeanswers_access_set,
    AccessType.teacher: teacher_access_set,
    AccessType.view: view_access_set,
}

SCIM_USER_NAME = ":scimuser"


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
    JSRunner = 10  # JSRunner can create new users that can be managed by JSRunner

    def to_contact_origin(self):
        if self == UserOrigin.Email:
            return ContactOrigin.Custom
        elif self == UserOrigin.Korppi or self == UserOrigin.Haka:
            return ContactOrigin.Haka
        elif self == UserOrigin.Sisu:
            return ContactOrigin.Sisu
        return ContactOrigin.Custom


@dataclass
class UserInfo:
    username: str | None = None
    email: str | None = None
    full_name: str | None = None
    given_name: str | None = None
    last_name: str | None = None
    origin: UserOrigin | None = None
    password: str | None = None
    password_hash: str | None = None
    unique_codes: list[SchacPersonalUniqueCode] = field(default_factory=list)

    def __post_init__(self):
        assert (
            self.password is None or self.password_hash is None
        ), "Cannot pass both password and password_hash to UserInfo"


def last_name_to_first(full_name: str | None):
    """Converts a name of the form "Firstname Middlenames Lastname" to "Lastname Firstname Middlenames"."""
    if full_name is None:
        return None
    names = full_name.split(" ")
    if len(names) > 1:
        return f'{names[-1]} {" ".join(names[:-1])}'
    return full_name


def last_name_to_last(full_name: str | None):
    """Converts a name of the form "Lastname Firstname Middlenames" to "Firstname Middlenames Lastname"."""
    if full_name is None:
        return None
    names = full_name.split(" ")
    if len(names) > 1:
        return f'{" ".join(names[1:])} {names[0]}'
    return full_name


deleted_user_suffix = "_deleted"
deleted_user_pattern = re.compile(rf".*{deleted_user_suffix}(_\d+)?$")


def user_query_with_joined_groups() -> Select:
    return select(User).options(selectinload(User.groups))


class User(db.Model, TimeStampMixin, SCIMEntity):
    """A user account. Used to identify users.

    .. note:: Some user IDs are reserved for internal use:

              * ID `0` is used to denote all "Anonymous users"
    """

    __tablename__ = "useraccount"

    id: Mapped[int] = mapped_column(primary_key=True)
    """User identifier."""

    name: Mapped[str] = mapped_column(unique=True)
    """User name (not full name). Used to identify the user and during log-in."""

    given_name: Mapped[Optional[str]]
    """User's given name."""

    last_name: Mapped[Optional[str]]
    """User's last name."""

    real_name: Mapped[Optional[str]]
    """Real (full) name. This may be in the form "Lastname Firstname" or "Firstname Lastname"."""

    _email: Mapped[Optional[str]] = mapped_column("email", unique=True)
    """Email address."""

    prefs: Mapped[Optional[str]]
    """Preferences as a JSON string."""

    pass_: Mapped[Optional[str]] = mapped_column("pass")
    """Password hashed with bcrypt."""

    consent: Mapped[Optional[Consent]]
    """Current consent for cookie/data collection."""

    origin: Mapped[Optional[UserOrigin]]
    """How the user registered to TIM."""

    uniquecodes: Mapped[Dict[Tuple[int, str], "PersonalUniqueCode"]] = relationship(
        back_populates="user",
        collection_class=attribute_keyed_dict("user_collection_key"),
    )
    """Personal unique codes used to identify the user via Haka Identity Provider."""

    internalmessage_readreceipt: Mapped[
        List["InternalMessageReadReceipt"]
    ] = relationship(back_populates="user")
    """User's read receipts for internal messages."""

    primary_email_contact: Mapped["UserContact"] = relationship(
        primaryjoin=(id == UserContact.user_id)
        & (UserContact.primary == PrimaryContact.true)
        & (UserContact.channel == Channel.EMAIL),
        overlaps="user, contacts",
    )
    """
    The primary email contact for the user.
    
    The primary contact is the preferred email address that the user wants to receive notifications from TIM.
    """

    @hybrid_property
    def email(self) -> str:
        """
        User's primary email address.

        This is the address the user can log in with and receive notifications from TIM.
        """
        return self._email

    @email.inplace.setter
    def _set_email(self, value: str) -> None:
        self.update_email(value)

    consents: Mapped[List["ConsentChange"]] = relationship(back_populates="user")
    """User's consent changes."""

    contacts: Mapped[List["UserContact"]] = relationship(
        back_populates="user",
        overlaps="primary_email_contact",
        cascade_backrefs=False,
    )
    """User's contacts."""

    notifications: DynamicMapped["Notification"] = relationship(
        "Notification",
        back_populates="user",
        lazy="dynamic",
        cascade_backrefs=False,
    )
    """Notification settings for the user. Represents what notifications the user wants to receive."""

    groups: Mapped[List["UserGroup"]] = relationship(
        secondary=UserGroupMember.__table__,
        primaryjoin=(id == UserGroupMember.user_id) & membership_current,
        back_populates="users",
        overlaps="user, current_memberships, group, memberships, memberships_sel",
    )
    """Current groups of the user is a member of."""

    groups_dyn: DynamicMapped["UserGroup"] = relationship(
        secondary=UserGroupMember.__table__,
        primaryjoin=id == UserGroupMember.user_id,
        lazy="dynamic",
        overlaps="group, groups, user, users, current_memberships, memberships, memberships_sel",
    )
    """All groups of the user as a dynamic query."""

    groups_inactive: DynamicMapped["UserGroup"] = relationship(
        secondary=UserGroupMember.__table__,
        primaryjoin=(id == UserGroupMember.user_id) & membership_deleted,
        lazy="dynamic",
        overlaps="group, groups, groups_dyn, user, users, current_memberships, memberships, memberships_sel",
    )
    """All groups the user is no longer a member of as a dynamic query."""

    memberships_dyn: DynamicMapped["UserGroupMember"] = relationship(
        foreign_keys="UserGroupMember.user_id",
        lazy="dynamic",
        overlaps="groups, groups_dyn, groups_inactive, user, users",
    )
    """User's group memberships as a dynamic query."""

    memberships: Mapped[List["UserGroupMember"]] = relationship(
        foreign_keys="UserGroupMember.user_id",
        overlaps="groups_inactive, memberships_dyn, user, users",
    )
    """All user's group memberships."""

    active_memberships: Mapped[Dict[int, "UserGroupMember"]] = relationship(
        primaryjoin=(id == UserGroupMember.user_id) & membership_current,
        collection_class=attribute_keyed_dict("usergroup_id"),
        overlaps="groups, groups_dyn, groups_inactive, memberships, memberships_dyn, user, users",
    )
    """Active group memberships mapped by user group ID."""

    lectures: Mapped[List["Lecture"]] = relationship(
        secondary=LectureUsers.__table__,
        back_populates="users",
    )
    """Lectures that the user is attending at the moment."""

    owned_lectures: DynamicMapped["Lecture"] = relationship(
        back_populates="owner", lazy="dynamic"
    )
    """Lectures that the user has created."""

    lectureanswers: DynamicMapped["LectureAnswer"] = relationship(
        back_populates="user", lazy="dynamic"
    )
    """Lecture answers that the user sent to lectures as a dynamic query."""

    messages: DynamicMapped["Message"] = relationship(
        back_populates="user", lazy="dynamic"
    )
    """Lecture messages that the user sent to lectures as a dynamic query."""

    questionactivity: Mapped[List["QuestionActivity"]] = relationship(
        back_populates="user"
    )
    """User's activity on lecture questions."""

    useractivity: Mapped[List["UserActivity"]] = relationship(back_populates="user")
    """User's activity during lectures."""

    answers: DynamicMapped["Answer"] = relationship(
        secondary=UserAnswer.__table__,
        back_populates="users",
        lazy="dynamic",
        overlaps="users_all",
    )
    """User's answers to tasks as a dynamic query."""

    annotations: DynamicMapped["Annotation"] = relationship(
        back_populates="annotator", lazy="dynamic"
    )
    """User's task annotations as a dynamic query."""

    velps: DynamicMapped["Velp"] = relationship(
        back_populates="creator", lazy="dynamic"
    )
    """Velps created by the user as a dynamic query."""

    sessions: Mapped[List["UserSession"]] = relationship(back_populates="user")
    """All user's sessions as a dynamic query."""

    active_sessions: Mapped[Dict[str, "UserSession"]] = relationship(
        primaryjoin=(id == UserSession.user_id) & ~UserSession.expired,
        collection_class=attribute_keyed_dict("session_id"),
        overlaps="sessions, user",
    )
    """Active sessions mapped by the session ID."""

    # Used for copying
    notifications_alt: Mapped[List["Notification"]] = relationship(
        overlaps="notifications, user"
    )
    owned_lectures_alt: Mapped[List["Lecture"]] = relationship(
        overlaps="owned_lectures, owner"
    )
    lectureanswers_alt: Mapped[List["LectureAnswer"]] = relationship(
        overlaps="lectureanswers, user"
    )
    messages_alt: Mapped[List["Message"]] = relationship(overlaps="messages, user")
    answers_alt: Mapped[List["Answer"]] = relationship(
        secondary=UserAnswer.__table__,
        overlaps="answers, users",
    )
    annotations_alt: Mapped[List["Annotation"]] = relationship(
        overlaps="annotations, annotator"
    )
    velps_alt: Mapped[List["Velp"]] = relationship(overlaps="velps, creator")

    def update_email(
        self,
        new_email: str,
        create_contact: bool = True,
        notify_message_lists: bool = True,
    ):
        """
        Updates the user's primary email address.

        .. note:: Prefer setting :attr:`email` instead of using this method. For example:

                    >>> user = User.get_by_name("testuser1")
                    >>> user.email = "newemail@example.com"
                    >>> db.session.commit()
                    None

        :param new_email: New email address.
        :param create_contact: Whether to create a new contact for the new email address. Defaults to `True`.
                                If `False`, updates only the user's email address info without updating the primary contact.
        :param notify_message_lists: If `True`, send a notification to all message lists that the user is subscribed to.
                                     Defaults to `True`.
        """
        from timApp.messaging.messagelist.emaillist import update_mailing_list_address

        prev_email = self._email
        self._email = new_email
        if prev_email != new_email:
            if create_contact:
                new_primary = (
                    run_sql(
                        select(UserContact)
                        .filter_by(
                            user_id=self.id, channel=Channel.EMAIL, contact=new_email
                        )
                        .limit(1)
                    )
                    .scalars()
                    .first()
                )
                if not new_primary:
                    # If new primary contact does not exist for the email, create it
                    # This is used mainly for CLI operations where email of the user is changed directly
                    new_primary = UserContact(
                        user=self,
                        verified=True,
                        channel=Channel.EMAIL,
                        contact=new_email,
                        contact_origin=ContactOrigin.Custom,
                    )
                    db.session.add(new_primary)
                self.primary_email_contact.primary = None
                new_primary.primary = PrimaryContact.true

            if notify_message_lists:
                update_mailing_list_address(prev_email, new_email)

    @property
    def scim_display_name(self):
        """User's display name in format used by SCIM API."""
        return last_name_to_last(self.real_name)

    @property
    def scim_created(self):
        """User's creation date in format used by SCIM API."""
        return self.created

    @property
    def scim_modified(self):
        """User's last modification date in format used by SCIM API."""
        return self.modified

    @property
    def scim_id(self):
        """User's identifier in format used by SCIM API."""
        return self.name

    @property
    def scim_resource_type(self):
        """The resource type of the user in format used by SCIM API."""
        return "User"

    @property
    def scim_extra_data(self):
        """Any extra data that should be returned in the SCIM API response."""
        email_contacts_stmt = select(UserContact).filter_by(
            user_id=self.id, channel=Channel.EMAIL, verified=True
        )
        return {
            "emails": [
                {"value": uc.contact} for uc in run_sql(email_contacts_stmt).scalars()
            ]
        }

    def __repr__(self):
        return f"<User(id={self.id}, name={self.name}, email={self.email}, real_name={self.real_name})>"

    @property
    def logged_in(self):
        """Whether the user is an authenticated user (i.e. not unauthenticated anonymous)."""
        return self.id != 0

    @property
    def is_real_user(self) -> bool:
        """
        Whether the user is a real user (i.e. not autogenerated anonymous) and is currently logged in.
        """
        # TODO: This should be used instead of logged_in in places where we need to ensure non-temporary users
        return self.id > 0

    @property
    def is_deleted(self) -> bool:
        return (self.email and deleted_user_pattern.match(self.email) is not None) or (
            self.name and deleted_user_pattern.match(self.name) is not None
        )

    @property
    def effective_groups(self) -> list[UserGroup]:
        """
        Returns the groups that the user effectively belongs to.

        Unlike :attr:`groups`, this list includes all metagroups that the user belongs to.
        Additionally, the list may be affected by user's current session settings.
        """

        def effective_real_groups():
            res = list(self.groups)
            res.append(UserGroup.get_anonymous_group())
            if self.logged_in:
                res.append(UserGroup.get_logged_in_group())
            return res

        if not self.is_current_user:
            return effective_real_groups()
        if self.skip_access_lock:
            return effective_real_groups()
        locked_groups = get_locked_active_groups()
        if locked_groups is None:
            return effective_real_groups()
        return (
            run_sql(select(UserGroup).filter(UserGroup.id.in_(locked_groups)))
            .scalars()
            .all()
        )

    @property
    def effective_group_ids(self):
        """Returns the group IDs of the user's currently active groups.

        Unlike :attr:`groups`, this property includes metagroups (e.g. anonymous, logged in, admin) and
        can be affected by the user's current session settings. Use this property only to check for permissions.
        """

        def effective_real_groups():
            res = {g.id for g in self.groups}
            res.add(get_anonymous_group_id())
            if self.logged_in:
                res.add(get_logged_in_group_id())
            return res

        if not self.is_current_user:
            return effective_real_groups()
        if self.skip_access_lock:
            return effective_real_groups()
        locked_groups = get_locked_active_groups()
        return locked_groups if locked_groups is not None else effective_real_groups()

    @property
    def is_admin(self):
        return get_admin_group_id() in self.effective_group_ids

    @property
    def is_email_user(self):
        """Returns whether the user signed up via email and has not been "upgraded" to Korppi or Sisu user."""
        return "@" in self.name or self.name.startswith("testuser")

    @property
    def is_current_user(self):
        """Returns whether the user is the one currently in session."""
        from timApp.auth.sessioninfo import get_current_user_id

        return has_request_context() and get_current_user_id() == self.id

    @property
    def pretty_full_name(self) -> str:
        """Returns the user's full name."""
        if self.is_name_hidden:
            return f"User {self.id}"
        if self.given_name and self.last_name:
            return f"{self.given_name} {self.last_name}"
        if self.real_name is None:
            return "(real_name is null)"
        parts = self.real_name.split(" ")
        if len(parts) == 1:
            return self.real_name
        return " ".join(parts[1:]) + " " + parts[0]

    @staticmethod
    def create_with_group(
        info: UserInfo,
        is_admin: bool = False,
        uid: int | None = None,
    ) -> tuple["User", UserGroup]:
        p_hash = (
            create_password_hash(info.password)
            if info.password is not None
            else info.password_hash
        )
        user = User(
            id=uid,
            name=info.username,
            real_name=info.full_name,
            last_name=info.last_name,
            given_name=info.given_name,
            _email=info.email,
            pass_=p_hash,
            origin=info.origin,
        )
        if info.email:
            primary_email = UserContact(
                contact=info.email,
                contact_origin=info.origin.to_contact_origin()
                if info.origin
                else ContactOrigin.Custom,
                verified=True,
                primary=PrimaryContact.true,
                channel=Channel.EMAIL,
            )
            user.contacts.append(primary_email)
            # Mark the email also primary for possible checks within the same session
            user.primary_email_contact = primary_email
        db.session.add(user)
        user.set_unique_codes(info.unique_codes)
        group = UserGroup.create(info.username)
        user.groups.append(group)
        if is_admin:
            user.make_admin()
        return user, group

    @staticmethod
    def get_by_name(name: str) -> Optional["User"]:
        return (
            run_sql(user_query_with_joined_groups().filter_by(name=name).limit(1))
            .scalars()
            .first()
        )

    @staticmethod
    def get_by_id(uid: int) -> Optional["User"]:
        return db.session.get(User, uid, options=[selectinload(User.groups)])

    @staticmethod
    def get_by_email(email: str) -> Optional["User"]:
        if email is None:
            raise Exception("Tried to find an user by null email")
        return (
            run_sql(user_query_with_joined_groups().filter_by(email=email).limit(1))
            .scalars()
            .first()
        )

    @staticmethod
    def get_by_email_case_insensitive(email: str) -> list["User"]:
        return (
            run_sql(
                user_query_with_joined_groups().filter(
                    func.lower(User.email).in_([email])
                )
            )
            .scalars()
            .all()
        )

    @staticmethod
    def get_by_email_case_insensitive_or_username(
        email_or_username: str,
    ) -> list["User"]:
        users = User.get_by_email_case_insensitive(email_or_username)
        if users:
            return users
        u = User.get_by_name(email_or_username)
        if u:
            return [u]
        return []

    @property
    def verified_email_name_parts(self) -> list[str]:
        email_parts = [
            uc.contact.split("@")
            for uc in run_sql(
                select(UserContact).filter_by(
                    user=self, channel=Channel.EMAIL, verified=True
                )
            ).scalars()
        ]
        return [parts[0].lower() for parts in email_parts]

    @property
    def is_special(self):
        return self.name in SPECIAL_USERNAMES

    # Used by authlib mixins (e.g. authlib.integrations.sqla_oauth2.create_save_token_func)
    def get_user_id(self):
        return self.id

    def check_password(
        self, password: str, allow_old=False, update_if_old=True
    ) -> bool:
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

    def get_home_org_student_id(self):
        home_org_id = get_home_organization_id()
        puc = self.uniquecodes.get((home_org_id, "studentID"), None)
        return puc.code if puc is not None else None

    def get_personal_group(self) -> UserGroup:
        return self.personal_group_prop

    @cached_property
    def personal_group_prop(self) -> UserGroup:
        group_to_find = self.name
        if self.name == ANONYMOUS_USERNAME:
            group_to_find = ANONYMOUS_GROUPNAME
        elif self.name == LOGGED_IN_USERNAME:
            group_to_find = LOGGED_IN_GROUPNAME
        for g in self.groups:
            if g.name == group_to_find:
                return g
        raise TimDbException(f"Personal usergroup for user {self.name} was not found!")

    def derive_personal_folder_name(self):
        real_name = self.real_name
        if not real_name:
            real_name = "anonymous"
        basename = remove_path_special_chars(real_name).lower()
        index = ""
        while Folder.find_by_path("users/" + basename + index):
            index = str(int(index or 1) + 1)
        return basename + index

    def get_personal_folder(self) -> Folder:
        return self.personal_folder_prop

    def _get_personal_folders(self) -> list[Folder]:
        if self.logged_in:
            group_condition = UserGroup.name == self.name
        else:
            group_condition = UserGroup.name == ANONYMOUS_GROUPNAME

        stmt = (
            select(Folder)
            .join(BlockAccess, BlockAccess.block_id == Folder.id)
            .join(UserGroup, UserGroup.id == BlockAccess.usergroup_id)
            .filter(
                (Folder.location == "users")
                & group_condition
                & (BlockAccess.type == AccessType.owner.value)
            )
            .with_only_columns(Folder)
            .options(
                defaultload(Folder._block)
                .selectinload(Block.accesses)
                .joinedload(BlockAccess.usergroup)
            )
        )

        return run_sql(stmt).scalars().all()

    @cached_property
    def personal_folder_prop(self) -> Folder:
        folders = self._get_personal_folders()
        if len(folders) >= 2:
            raise TimDbException(
                f"Found multiple personal folders for user {self.name}: {[f.name for f in folders]}"
            )
        if not folders:
            with filelock.FileLock(f"/tmp/tim_personal_folder_create_{self.id}.lock"):
                # It could be that the folder already exists because another call already created it
                # So, retry the query (recursive call might cause overflow, so we do a manual query
                folders = self._get_personal_folders()
                if folders:
                    return folders[0]
                f = Folder.create(
                    f"users/{self.derive_personal_folder_name()}",
                    self.get_personal_group(),
                    title=f"{self.real_name}",
                    creation_opts=FolderCreationOptions(apply_default_rights=True),
                )
                db.session.commit()
                return f
        return folders[0]

    def get_prefs(self) -> Preferences:
        prefs = json.loads(self.prefs or "{}")
        try:
            return Preferences.from_json(prefs)
        except TypeError:
            return Preferences()

    def set_prefs(self, prefs: Preferences):
        self.prefs = json.dumps(prefs, cls=TimJsonEncoder)

    def get_groups(
        self, include_special: bool = True, include_expired: bool = True
    ) -> Select:
        special_groups = [ANONYMOUS_GROUPNAME]
        if self.logged_in:
            special_groups.append(LOGGED_IN_GROUPNAME)
        member_condition = UserGroupMember.user_id == self.id
        if not include_expired:
            member_condition = member_condition & membership_current
        group_condition = UserGroup.id.in_(
            select(UserGroupMember.usergroup_id).filter(member_condition)
        )
        if include_special:
            group_condition = group_condition | UserGroup.name.in_(special_groups)
        return select(UserGroup).filter(group_condition)

    def add_to_group(
        self,
        ug: UserGroup,
        added_by: Optional["User"],
        sync_mailing_lists: bool = True,
    ) -> bool:
        """
        Adds the user to a group.

        :param ug: The user group to add the user to.
        :param added_by: Optionally, the user that added this user to the group.
        :param sync_mailing_lists: If True, automatically notifies message lists about the added user.
        :return: True, if the added user is a "new" member (i.e. never was a member of the group before).
        """
        # Avoid cyclical importing.
        from timApp.messaging.messagelist.messagelist_utils import (
            sync_message_list_on_add,
        )
        from timApp.notification.group_notification import send_group_join_message

        add_membership = False
        existing: UserGroupMember = (
            self.id is not None and self.memberships_dyn.filter_by(group=ug).first()
        )
        if existing:
            if existing.membership_end is not None:
                existing.membership_added = get_current_time()
                existing.adder = added_by
                add_membership = True

            existing.membership_end = None
            new_add = False
        else:
            ugm = UserGroupMember(group=ug, adder=added_by)
            self.memberships.append(ugm)
            db.session.add(ugm)
            new_add = True
            add_membership = True

        if add_membership:
            send_group_join_message(self, ug)

        # On changing of group, sync this person to the user group's message lists.
        if sync_mailing_lists:
            sync_message_list_on_add(self, ug)
        return new_add

    def get_contact(
        self,
        channel: Channel,
        contact: str,
        options: list[LoaderOption] | None = None,
    ) -> UserContact | None:
        """Find user's contact by channel and contact contents.

        :param channel: Contact channel.
        :param contact: Contact contents.
        :param options: Additional DB load options.
        :return: UserContact if found, otherwise None.
        """

        stmt = select(UserContact).filter(
            (UserContact.user == self)
            & (UserContact.channel == channel)
            & (UserContact.contact == contact)
        )
        if options:
            stmt = stmt.options(*options)
        return run_sql(stmt).scalars().one_or_none()

    @staticmethod
    def get_scimuser() -> "User":
        u = User.get_by_name(SCIM_USER_NAME)
        if not u:
            u, _ = User.create_with_group(
                UserInfo(
                    username=SCIM_USER_NAME,
                    full_name="Scim User",
                    email="scimuser@example.com",
                )
            )
        return u

    @staticmethod
    def get_anon() -> "User":
        return User.get_by_id(0)

    def update_info(self, info: UserInfo, sync_mailing_lists: bool = True) -> None:
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
            email_origin = (
                info.origin.to_contact_origin() if info.origin else ContactOrigin.Custom
            )
            self.set_emails(
                [info.email],
                email_origin,
                force_verify=True,
                force_primary=True,
                remove=False,
                notify_message_lists=sync_mailing_lists,
            )
        if info.password:
            self.pass_ = create_password_hash(info.password)
        elif info.password_hash:
            self.pass_ = info.password_hash
        self.set_unique_codes(info.unique_codes)

    def set_emails(
        self,
        emails: list[str],
        origin: ContactOrigin,
        force_verify: bool = False,
        force_primary: bool = False,
        can_update_primary: bool = False,
        add: bool = True,
        remove: bool = True,
        notify_message_lists: bool = True,
    ) -> None:
        """Sets emails for the given origin.

        Existing emails for the given origin are overwritten.
        If the user's primary email is removed,
        it is changed to the next verified email of the same origin.

        :param emails: List of emails to set to the given origin.
        :param origin: Emails' origin
        :param force_verify: If True, all emails are marked as verified.
                             Otherwise, origins in NO_AUTO_VERIFY_ORIGINS are not verified.
        :param force_primary: If True, forces to update the primary address
        :param can_update_primary: If True, allows to "update" the primary email address.
                                If the user's primary email is custom and a new email is added from the integration,
                                set that email as primary.
                                Also, if user's primary email is custom and a new email is added is also custom,
                                set the first email address in the list as primary.
        :param add: If True, adds new emails in the list to the user.
        :param remove: If True, removes emails not present in emails list.
        :param notify_message_lists: If True, notifies the message lists about the change.
        """

        if not emails:
            return

        with db.session.no_autoflush:
            # Get emails for the current origin
            # We use self.contacts for now because the only contacts we save are email contacts and because it allows
            # to call this method multiple times within the same transaction
            current_email_contacts = [
                uc
                for uc in self.contacts
                if uc.channel == Channel.EMAIL and uc.contact_origin == origin
            ]
            current_email_dict: dict[str, UserContact] = {
                uc.contact: uc for uc in current_email_contacts
            }

            new_emails = set(emails)
            current_emails = {uc.contact for uc in current_email_contacts}

            to_add = new_emails - current_emails if add else set()
            to_remove = current_emails - new_emails if remove else set()
            not_removed = set() if remove else current_emails - new_emails
            new_email_contacts: dict[str, UserContact] = {
                email: current_email_dict[email]
                for email in ((new_emails & current_emails) | not_removed)
            }

            change_primary_email = force_primary or (
                self.primary_email_contact is None
                or (
                    self.primary_email_contact.contact_origin == origin
                    and self.primary_email_contact.contact in to_remove
                )
                or (origin == ContactOrigin.Custom and can_update_primary)
            )

            if to_add:
                verified = force_verify or origin not in NO_AUTO_VERIFY_ORIGINS
                # Resolve all emails to add that don't belong to the current origin
                added_email_contacts = [
                    uc
                    for uc in self.contacts
                    if uc.channel == Channel.EMAIL
                    and uc.contact_origin != origin
                    and uc.contact in to_add
                ]
                added_email_contacts_set = {uc.contact for uc in added_email_contacts}
                other_origin_email_contacts: dict[str, UserContact] = {
                    uc.contact: uc for uc in added_email_contacts
                }

                # If this is a new integration and actual new emails are added (and not just updated),
                # we can update the primary email
                change_primary_email = change_primary_email or (
                    can_update_primary
                    and origin != ContactOrigin.Custom
                    and not current_email_contacts
                    and len(to_add - added_email_contacts_set) > 0
                )

                for email in to_add:
                    # If the email is already registered to the user, adjust its origin
                    if email in other_origin_email_contacts:
                        uc = other_origin_email_contacts[email]
                        uc.contact_origin = origin
                    else:
                        uc = UserContact(
                            channel=Channel.EMAIL,
                            verified=verified,
                            contact_origin=origin,
                            contact=email,
                        )
                        self.contacts.append(uc)
                    new_email_contacts[email] = uc

            for email in to_remove:
                uc = current_email_dict[email]
                db.session.delete(uc)
                self.contacts.remove(uc)

            if change_primary_email:
                # Ensure no email is primary
                if self.primary_email_contact:
                    self.primary_email_contact.primary = None

                if emails:
                    new_primary = new_email_contacts[emails[0]]
                elif new_email_contacts:
                    new_primary = next(uc for uc in new_email_contacts.values())
                else:
                    new_primary = next(
                        (uc for uc in self.contacts if uc.channel == Channel.EMAIL),
                        None,
                    )

                    if not new_primary:
                        # Special case: this update operation ends up deleting all emails of the user
                        # This is not desired in most cases, so we will have to adjust by taking
                        # one of the deleted emails and re-adding it as a Custom email
                        new_primary = UserContact(
                            user=self,
                            contact_origin=ContactOrigin.Custom,
                            verified=True,
                            contact=next(u for u in current_email_contacts).contact,
                        )
                        self.contacts.append(new_primary)

                new_primary.primary = PrimaryContact.true
                self.update_email(
                    new_primary.contact,
                    create_contact=False,
                    notify_message_lists=notify_message_lists,
                )

    def set_unique_codes(self, codes: list[SchacPersonalUniqueCode]):
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

    @property
    def skip_access_lock(self):
        """If set, access any access locking is skipped when checking for permissions."""
        return getattr(self, "bypass_access_lock", False)

    @skip_access_lock.setter
    def skip_access_lock(self, value):
        setattr(self, "bypass_access_lock", value)

    def _downgrade_access(
        self, access_vals: set[int], access: BlockAccess | None
    ) -> BlockAccess | None:
        if access is None:
            return None
        if not self.is_current_user:
            return access
        if access.require_confirm:
            return access
        if self.skip_access_lock:
            return access

        max_access = get_locked_access_type()
        if not max_access:
            return access

        max_access_val = max_access.value

        if max_access_val not in access_vals:
            return None

        if access.type <= max_access_val:
            return access

        access = BlockAccess(
            block_id=access.block_id,
            usergroup_id=access.usergroup_id,
            type=max_access_val,
            accessible_from=access.accessible_from,
            accessible_to=access.accessible_to,
            duration=access.duration,
            duration_from=access.duration_from,
            duration_to=access.duration_to,
        )

        return access

    def has_some_access(
        self,
        i: ItemOrBlock,
        vals: set[int],
        allow_admin: bool = True,
        grace_period: timedelta = timedelta(seconds=0),
        duration: bool = False,
    ) -> BlockAccess | None:
        """
        Check if the user has any possible access to the given item or block.

        :param i: The item or block to check
        :param vals: Access types to check. See AccessType for available values.
        :param allow_admin: If True, allow admins to bypass the access check
        :param grace_period: Grace period for the access check.
                             If the user has access to the item, extends the end date of the access by this amount.
        :param duration: If True checks for duration access instead of active accesses.
        :return: The best access object that user currently has for the given item or block and access types.
        """
        curr_group_ids = self.effective_group_ids
        admin_group_id = get_admin_group_id()
        if allow_admin and admin_group_id in curr_group_ids:
            result = BlockAccess(
                block_id=i.id,
                accessible_from=datetime.min.replace(tzinfo=timezone.utc),
                type=AccessType.owner.value,
                usergroup_id=self.get_personal_group().id,
            )
            return self._downgrade_access(vals, result)

        from timApp.auth.session.util import session_has_access

        if not session_has_access(i, self):
            return None

        if isinstance(i, ItemBase):
            b = i.block
        else:
            b = i
        if not b:
            return None
        now = get_current_time()
        best_access = None
        for a in b.accesses.values():  # type: BlockAccess
            if a.usergroup_id not in curr_group_ids:
                continue
            if a.type not in vals:
                continue
            to_time = a.accessible_to
            if to_time is not None:
                to_time += grace_period
            if (a.accessible_from or maxdate) <= now < (to_time or maxdate):
                # If the end time of the access is unrestricted, there is no better access.
                if to_time is None:
                    return self._downgrade_access(vals, a)
                # If the end time of the access is restricted, there might be a better access,
                # so we'll continue looping.
                if best_access is None or best_access.accessible_to < a.accessible_to:
                    best_access = a
            if (
                duration
                and a.unlockable
                and ((a.duration_from or maxdate) <= now < (a.duration_to or maxdate))
            ):
                return self._downgrade_access(vals, a)
        return self._downgrade_access(vals, best_access)

    def has_access(
        self,
        i: ItemOrBlock,
        access: AccessType,
        grace_period: timedelta = timedelta(seconds=0),
        duration: bool = False,
    ) -> BlockAccess | None:
        """
        Check if the user has access to the given item or block.

        :param i: Item or block to check
        :param access: Access type to check. See AccessType for available values.
        :param grace_period: Grace period for the access check.
                             If the user has access to the item, extends the end date of the access by this amount.
        :param duration: If True checks for duration access instead of active accesses.
        :return: The best access object that user currently has for the given item or block and access type.
                 Otherwise, if user has no access, None.
        """
        from timApp.auth.accesshelper import check_inherited_right

        return check_inherited_right(
            self, i, access, grace_period
        ) or self.has_some_access(
            i, access_sets[access], grace_period=grace_period, duration=duration
        )

    def has_view_access(
        self, i: ItemOrBlock, duration: bool = False
    ) -> BlockAccess | None:
        return self.has_some_access(i, view_access_set, duration=duration)

    def has_edit_access(
        self, i: ItemOrBlock, duration: bool = False
    ) -> BlockAccess | None:
        return self.has_some_access(i, edit_access_set, duration=duration)

    def has_manage_access(
        self, i: ItemOrBlock, duration: bool = False
    ) -> BlockAccess | None:
        return self.has_some_access(i, manage_access_set, duration=duration)

    def has_teacher_access(
        self, i: ItemOrBlock, duration: bool = False
    ) -> BlockAccess | None:
        return self.has_some_access(i, teacher_access_set, duration=duration)

    def has_seeanswers_access(
        self, i: ItemOrBlock, duration: bool = False
    ) -> BlockAccess | None:
        return self.has_some_access(i, seeanswers_access_set, duration=duration)

    def has_copy_access(
        self, i: ItemOrBlock, duration: bool = False
    ) -> BlockAccess | None:
        return self.has_some_access(i, copy_access_set, duration=duration)

    def has_ownership(
        self, i: ItemOrBlock, allow_admin: bool = True
    ) -> BlockAccess | None:
        return self.has_some_access(i, owner_access_set, allow_admin)

    def can_write_to_folder(self, f: Folder):
        # not even admins are allowed to create new items in 'users' folder
        if f.path == "users":
            return False
        return self.has_edit_access(f)

    def grant_access(
        self,
        block: ItemOrBlock,
        access_type: AccessType,
        accessible_from: datetime | None = None,
        accessible_to: datetime | None = None,
        duration_from: datetime | None = None,
        duration_to: datetime | None = None,
        duration: timedelta | None = None,
        require_confirm: bool | None = None,
    ):
        return grant_access(
            group=self.get_personal_group(),
            block=block,
            access_type=access_type,
            accessible_from=accessible_from,
            accessible_to=accessible_to,
            duration_from=duration_from,
            duration_to=duration_to,
            duration=duration,
            require_confirm=require_confirm,
        )

    def remove_access(self, block_id: int, access_type: str | AccessType) -> None:
        """Remove user's permissions to the specified item (block)"""
        if isinstance(access_type, AccessType):
            access_type = access_type.value
        stmt = delete(BlockAccess).where(
            (BlockAccess.block_id == block_id)
            & (BlockAccess.usergroup_id == self.get_personal_group().id)
            & (BlockAccess.type == get_access_type_id(access_type))
        )
        run_sql(stmt)

    def get_notify_settings(self, item: DocInfo | Folder) -> dict:
        # TODO: Instead of conversion, expose all notification types in UI
        n: list[Notification] = self.notifications.filter_by(block_id=item.id).all()

        result = {
            "email_doc_modify": False,
            "email_comment_add": False,
            "email_comment_modify": False,
            "email_answer_add": False,
        }

        for nn in n:
            if nn.notification_type in (
                NotificationType.DocModified,
                NotificationType.ParAdded,
                NotificationType.ParDeleted,
                NotificationType.ParModified,
            ):
                result["email_doc_modify"] = True
            if nn.notification_type in (NotificationType.CommentAdded,):
                result["email_comment_add"] = True
            if nn.notification_type in (
                NotificationType.CommentModified,
                NotificationType.CommentDeleted,
            ):
                result["email_comment_modify"] = True
            if nn.notification_type in (NotificationType.AnswerAdded,):
                result["email_answer_add"] = True

        return result

    def set_notify_settings(
        self,
        item: DocInfo | Folder,
        doc_modify: bool,
        comment_add: bool,
        comment_modify: bool,
        answer_add: bool,
    ):
        # TODO: Instead of conversion, expose all notification types in UI
        notification_types = []
        if doc_modify:
            notification_types.extend(
                (
                    NotificationType.DocModified,
                    NotificationType.ParAdded,
                    NotificationType.ParDeleted,
                    NotificationType.ParModified,
                )
            )
        if comment_add:
            notification_types.extend((NotificationType.CommentAdded,))
        if comment_modify:
            notification_types.extend(
                (
                    NotificationType.CommentModified,
                    NotificationType.CommentDeleted,
                )
            )
        if answer_add:
            notification_types.extend((NotificationType.AnswerAdded,))

        self.notifications.filter((Notification.block_id == item.id)).delete(
            synchronize_session=False
        )
        for nt in notification_types:
            db.session.add(
                Notification(
                    user=self,
                    block_id=item.id,
                    notification_type=nt,
                )
            )

    def get_answers_for_task(self, task_id: str):
        return (
            self.answers.options(selectinload(Answer.users_all))
            .order_by(Answer.id.desc())
            .filter_by(task_id=task_id)
        )

    @property
    def is_name_hidden(self):
        """Hides names and email of the user, but not user ID"""
        return getattr(self, "hide_name", False)

    @is_name_hidden.setter
    def is_name_hidden(self, value):
        setattr(self, "hide_name", value)

    @property
    def is_anonymized(self):
        """Hides names, email and ID of the user"""
        return getattr(self, "anonymize", False)

    @is_anonymized.setter
    def is_anonymized(self, value):
        setattr(self, "anonymize", value)

    @property
    def is_anonymous_guest_user(self):
        return self.id < 0

    @property
    def is_sisu_teacher(self) -> bool:
        """Whether the user belongs to at least one Sisu teacher group"""
        if self.is_special:
            return False
        teacher_group_id = (
            run_sql(
                select(ScimUserGroup.group_id)
                .join(UserGroup)
                .join(UserGroupMember)
                .filter(
                    (UserGroupMember.user_id == self.id)
                    & ScimUserGroup.external_id.like("%-teachers")
                )
            )
            .scalars()
            .first()
        )
        return teacher_group_id is not None

    @property
    def basic_info_dict(self):
        if self.is_anonymized:
            info_dict = {
                "id": 1,
                "name": f"Anonymous",
                "real_name": f"Anonymous",
                "email": f"Anonymous@example.com",
            }
        elif self.is_name_hidden:
            info_dict = {
                "id": self.id,
                "name": f"user{self.id}",
                "real_name": f"User {self.id}",
                "email": f"user{self.id}@example.com",
            }
        else:
            info_dict = {
                "id": self.id,
                "name": self.name,
                "real_name": self.real_name,
                "email": self.email,
            }

        if is_attribute_loaded("uniquecodes", self):
            info_dict["student_id"] = self.get_home_org_student_id()

        return info_dict

    def to_json(self, full: bool = False, contacts: bool = False) -> dict:
        external_ids: dict[int, str] = (
            {
                s.group_id: s.external_id
                for s in run_sql(
                    select(ScimUserGroup).filter(
                        ScimUserGroup.group_id.in_([g.id for g in self.groups])
                    )
                )
                .scalars()
                .all()
            }
            if full
            else []
        )

        result = self.basic_info_dict

        if full:
            groups = self.groups
            if (
                self.logged_in
                and self.is_current_user
                and get_locked_active_groups() is not None
            ):
                groups = self.effective_groups
            result |= {
                "group": self.get_personal_group(),
                "groups": [
                    {**g.to_json(), "external_id": external_ids.get(g.id, None)}
                    for g in groups
                ],
                "folder": self.get_personal_folder() if self.logged_in else None,
                "consent": self.consent,
                "last_name": self.last_name,
            }

        if contacts:
            result |= {"contacts": self.contacts}

        if self.logged_in and self.is_current_user:
            if locked_access := get_locked_access_type():
                result |= {"locked_access": locked_access.value}
            if (active_groups := get_locked_active_groups()) is not None:
                result |= {"locked_active_groups": list(active_groups)}

        return result

    @cached_property
    def bookmarks(self):
        from timApp.bookmark.bookmarks import Bookmarks

        return Bookmarks(self)

    def belongs_to_any_of(self, *groups: UserGroup):
        return bool(set(groups) & set(self.groups))

    @staticmethod
    def get_model_answer_user() -> Optional["User"]:
        # TODO: think other languages also
        return User.get_by_name(current_app.config["MODEL_ANSWER_USER_NAME"])


def get_membership_end(u: User, group_ids: set[int]) -> datetime | None:
    """
    Get the end of the membership of the user in the given groups.

    .. note:: If the user's membership ended in multiple groups, the latest end date is returned.

    :param u: The user
    :param group_ids: The IDs of the groups
    :return: The end of the membership or
             None if the user is not a member of the groups or if the user's membership hasn't ended yet
    """

    relevant_memberships: list[UserGroupMember] = [
        m for m in u.memberships if m.usergroup_id in group_ids
    ]
    membership_end = None
    # If the user is not active in any of the groups, we'll show the lastly-ended membership.
    # TODO: It might be possible in the future that the membership_end is in the future.
    if relevant_memberships and all(
        m.membership_end is not None for m in relevant_memberships
    ):
        membership_end = max(m.membership_end for m in relevant_memberships)
    return membership_end


def get_membership_added(u: User, group_ids: set[int]) -> datetime | None:
    """
    Get the earliest time the user was added to the given groups.

    :param u: The user
    :param group_ids: The IDs of the groups
    :return: The earliest time the user was added to the given groups
             or None if the user is not a member of the groups
    """

    relevant_memberships: list[UserGroupMember] = [
        m for m in u.memberships if m.usergroup_id in group_ids
    ]
    membership_added_times = [
        m.membership_added
        for m in relevant_memberships
        if m.membership_added is not None
    ]
    return min(membership_added_times) if membership_added_times else None


def has_no_higher_right(access_type: str | None, rights: UserItemRights) -> bool:
    """
    Checks whether the given access type (view, edit, ...) has no higher match in the given UserItemRights.
    For example, if rights has {'edit': True}, then has_no_higher_right('view', rights) is False.
    For now, only works for view, edit, see_answers and teacher access types.

    :param access_type The access type to check.
    :param rights The UserItemRights to consider.
    :return True if access_type is one of view, edit, see_answers or teacher and there is no higher right in the
     UserItemRights, False otherwise.
    """
    if not access_type:
        return False
    return {
        "view": not rights["editable"] and not rights["see_answers"],
        "edit": not rights["see_answers"],
        "see_answers": not rights["teacher"],
        "teacher": not rights["manage"],
    }.get(access_type, False)


def get_owned_objects_query(u: User):
    return (
        u.get_personal_group()
        .accesses.filter_by(type=AccessType.owner.value)
        .with_entities(BlockAccess.block_id)
    )
