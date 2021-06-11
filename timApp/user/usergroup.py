from __future__ import annotations

from functools import lru_cache
from typing import List, Dict, Tuple, TYPE_CHECKING, Optional

import attr
from sqlalchemy.orm import joinedload
from sqlalchemy.orm.collections import attribute_mapped_collection

from timApp.auth.auth_models import BlockAccess
from timApp.messaging.messagelist.messagelist_models import MessageListTimMember
from timApp.messaging.timMessage.internalmessage_models import InternalMessageDisplay, InternalMessageReadReceipt
from timApp.sisu.parse_display_name import parse_sisu_group_display_name
from timApp.sisu.scimusergroup import ScimUserGroup
from timApp.timdb.sqa import db, TimeStampMixin, include_if_exists, is_attribute_loaded
from timApp.user.scimentity import SCIMEntity
from timApp.user.special_group_names import ANONYMOUS_GROUPNAME, LOGGED_IN_GROUPNAME, \
    ADMIN_GROUPNAME, GROUPADMIN_GROUPNAME, TEACHERS_GROUPNAME, SPECIAL_GROUPS, FUNCTIONSCHEDULER_GROUPNAME
from timApp.user.usergroupdoc import UserGroupDoc
from timApp.user.usergroupmember import UserGroupMember, membership_current

if TYPE_CHECKING:
    from timApp.item.block import Block

# Prefix is no longer needed because scimusergroup determines the Sisu (SCIM) groups.
SISU_GROUP_PREFIX = ''


def tim_group_to_scim(tim_group: str) -> str:
    if not tim_group.startswith(SISU_GROUP_PREFIX):
        raise Exception(f"Group {tim_group} is not a Sisu group")
    return tim_group[len(SISU_GROUP_PREFIX):]


ORG_GROUP_SUFFIX = ' users'


class UserGroup(db.Model, TimeStampMixin, SCIMEntity):
    """A usergroup. Each User should belong to a personal UserGroup that has the same name as the User name. No one
    else should belong to a personal UserGroup.

    A User can additionally belong to any number of other UserGroups.

    Two special groups named 'Logged-in users' and 'Anonymous users' denote the set of all logged-in users and all
    users including anonymous (not logged-in) ones, respectively.

    In database, the User 'Anonymous user' belongs to 'Anonymous users' group. Other than that,
    the two groups are empty from the database's point of view.
    """
    __tablename__ = 'usergroup'
    id = db.Column(db.Integer, primary_key=True)
    """Usergroup identifier."""

    name = db.Column(db.Text, nullable=False, unique=True)
    """Usergroup name (textual identifier)."""

    display_name = db.Column(db.Text, nullable=True)
    """Usergroup display name. Currently only used for storing certain Sisu course properties:
     - course code
     - period (P1...P5)
     - date range
     - group description in Sisu
    """

    @property
    def scim_display_name(self):
        return self.display_name

    users = db.relationship(
        'User',
        UserGroupMember.__table__,
        primaryjoin=(id == UserGroupMember.usergroup_id) & membership_current,
        secondaryjoin="UserGroupMember.user_id == User.id",
        back_populates="groups",
    )
    memberships = db.relationship(
        UserGroupMember,
        back_populates="group",
        lazy='dynamic',
    )
    memberships_sel = db.relationship(
        UserGroupMember,
        back_populates="group",
        cascade='all, delete-orphan',
    )
    current_memberships = db.relationship(
        UserGroupMember,
        primaryjoin=(id == UserGroupMember.usergroup_id) & membership_current,
        collection_class=attribute_mapped_collection("user_id"),
        back_populates="group",
    )
    accesses = db.relationship(
        'BlockAccess',
        back_populates='usergroup',
        lazy='dynamic',
    )
    accesses_alt: Dict[Tuple[int, int], BlockAccess] = db.relationship(
        'BlockAccess',
        collection_class=attribute_mapped_collection('group_collection_key'),
        cascade='all, delete-orphan',
    )
    readparagraphs = db.relationship('ReadParagraph', back_populates='usergroup', lazy='dynamic')
    readparagraphs_alt = db.relationship('ReadParagraph')
    notes = db.relationship('UserNote', back_populates='usergroup', lazy='dynamic')
    notes_alt = db.relationship('UserNote')

    admin_doc: Block = db.relationship(
        'Block',
        secondary=UserGroupDoc.__table__,
        lazy='select',
        uselist=False,
    )

    # For groups created from SCIM API
    external_id: ScimUserGroup = db.relationship('ScimUserGroup', lazy='select', uselist=False)

    messagelist_membership: MessageListTimMember = db.relationship("MessageListTimMember", back_populates="user_group")

    internalmessage_display: Optional[InternalMessageDisplay] = db.relationship('InternalMessageDisplay',
                                                                                back_populates='usergroup')
    internalmessage_readreceipt: Optional[InternalMessageReadReceipt] = db.relationship('InternalMessageReadReceipt',
                                                                                        back_populates='recipient')

    def __repr__(self):
        return f'<UserGroup(id={self.id}, name={self.name})>'

    @property
    def scim_created(self):
        return self.created

    @property
    def scim_modified(self):
        return self.modified

    @property
    def scim_id(self):
        return self.external_id.external_id if self.external_id else None

    @property
    def scim_resource_type(self):
        return 'Group'

    def is_anonymous(self) -> bool:
        return self.name == ANONYMOUS_GROUPNAME

    def is_large(self) -> bool:
        return self.name.endswith(ORG_GROUP_SUFFIX)

    def load_personal_user(self):
        """If this is a personal usergroup, loads the user object to personal_user attribute."""
        from timApp.user.user import User
        self.personal_user = User.get_by_name(self.name)

    def to_json(self):
        r = {
            'id': self.id,
            'name': self.name,
            **include_if_exists('personal_user', self),
        }
        if is_attribute_loaded('admin_doc', self) and self.admin_doc and self.admin_doc.docentries:
            r['admin_doc_path'] = self.admin_doc.docentries[0].path
        return r

    @property
    def is_personal_group(self):
        self.load_personal_user()
        return self.personal_user is not None

    @property
    def pretty_full_name(self):
        return self.name

    @property
    def is_sisu(self):
        return self.external_id is not None

    @property
    def is_sisu_student_group(self):
        return self.is_sisu and self.external_id.external_id.endswith('-students')

    @staticmethod
    def create(name: str) -> 'UserGroup':
        """Creates a new user group.

        :param name: The name of the user group.
        :returns: The id of the created user group.

        """

        ug = UserGroup(name=name)
        db.session.add(ug)
        return ug

    @staticmethod
    def get_by_external_id(name: str) -> 'UserGroup':
        r = get_sisu_groups_by_filter(ScimUserGroup.external_id == name)
        return r[0] if r else None

    @staticmethod
    def get_by_name(name) -> 'UserGroup':
        return UserGroup.query.filter_by(name=name).first()

    @staticmethod
    def get_anonymous_group() -> 'UserGroup':
        return UserGroup.query.filter_by(name=ANONYMOUS_GROUPNAME).one()

    @staticmethod
    def get_admin_group() -> 'UserGroup':
        return UserGroup.query.filter_by(name=ADMIN_GROUPNAME).one()

    @staticmethod
    def get_groupadmin_group() -> 'UserGroup':
        return UserGroup.query.filter_by(name=GROUPADMIN_GROUPNAME).one()

    @staticmethod
    def get_organization_group(org: str) -> 'UserGroup':
        gname = org + ORG_GROUP_SUFFIX
        return UserGroup.get_or_create_group(gname)

    @staticmethod
    def get_haka_group() -> 'UserGroup':
        haka_group_name = 'Haka users'
        return UserGroup.get_or_create_group(haka_group_name)

    @staticmethod
    def get_organizations() -> List['UserGroup']:
        return UserGroup.query.filter(UserGroup.name.endswith(' users') & UserGroup.name.notin_(SPECIAL_GROUPS)).all()

    @staticmethod
    def get_teachers_group() -> 'UserGroup':
        return UserGroup.query.filter_by(name=TEACHERS_GROUPNAME).one()

    @staticmethod
    def get_user_creator_group() -> 'UserGroup':
        user_creator_group_name = 'User creators'
        return UserGroup.get_or_create_group(user_creator_group_name)

    @staticmethod
    def get_function_scheduler_group() -> 'UserGroup':
        return UserGroup.get_or_create_group(FUNCTIONSCHEDULER_GROUPNAME)

    @staticmethod
    def get_or_create_group(group_name: str) -> 'UserGroup':
        ug = UserGroup.get_by_name(group_name)
        if not ug:
            ug = UserGroup.create(group_name)
            db.session.add(ug)
        return ug

    @staticmethod
    def get_logged_in_group() -> 'UserGroup':
        return UserGroup.query.filter_by(name=LOGGED_IN_GROUPNAME).one()


@lru_cache()
def get_logged_in_group_id() -> int:
    return UserGroup.get_logged_in_group().id


@lru_cache()
def get_anonymous_group_id() -> int:
    return UserGroup.get_anonymous_group().id


def get_usergroup_eager_query():
    from timApp.item.block import Block
    return (
        UserGroup.query
            .options(joinedload(UserGroup.admin_doc)
                     .joinedload(Block.docentries))
            .options(joinedload(UserGroup.current_memberships))
    )


def get_sisu_groups_by_filter(f) -> List[UserGroup]:
    gs: List[UserGroup] = (
        get_usergroup_eager_query()
            .join(ScimUserGroup)
            .filter(f)
            .all()
    )
    return gs


# When a SCIM group is deleted, the group name gets this prefix.
DELETED_GROUP_PREFIX = 'deleted:'


@attr.s(auto_attribs=True)
class UserGroupWithSisuInfo:
    """Wrapper for UserGroup that reports the sisugroup path in to_json."""
    ug: UserGroup

    def to_json(self):
        return {
            **self.ug.to_json(),
            'admin_doc': self.ug.admin_doc.docentries[0] if self.ug.admin_doc else None,
            'sisugroup_path': parse_sisu_group_display_name(
                self.ug.display_name).sisugroups_doc_path if self.ug.display_name else None
        }
