from typing import List, Tuple, Optional

from sqlalchemy import func
from sqlalchemy.ext.associationproxy import association_proxy
from sqlalchemy.orm import joinedload

from timApp.item.tag import Tag, TagType
from timApp.sisu.scimusergroup import ScimUserGroup
from timApp.timdb.sqa import db, TimeStampMixin, include_if_loaded, include_if_exists, is_attribute_loaded
from timApp.user.scimentity import SCIMEntity
from timApp.user.special_group_names import ANONYMOUS_GROUPNAME, LARGE_GROUPS, KORPPI_GROUPNAME, LOGGED_IN_GROUPNAME, \
    ADMIN_GROUPNAME, GROUPADMIN_GROUPNAME, TEACHERS_GROUPNAME
from timApp.user.usergroupdoc import UserGroupDoc
from timApp.user.usergroupmember import UserGroupMember

# Prefix is no longer needed because scimusergroup determines the Sisu (SCIM) groups.
SISU_GROUP_PREFIX = ''


def tim_group_to_scim(tim_group: str):
    if not tim_group.startswith(SISU_GROUP_PREFIX):
        raise Exception(f"Group {tim_group} is not a Sisu group")
    return tim_group[len(SISU_GROUP_PREFIX):]


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
    """Usergroup display name."""

    @property
    def scim_display_name(self):
        return self.display_name

    # users = db.relationship('User', secondary=UserGroupMember.__table__,
    #                         back_populates='groups', lazy='dynamic')
    users = association_proxy('group_users', 'user', creator=lambda x: UserGroupMember(user=x))
    group_users = db.relationship(
        'UserGroupMember',
        primaryjoin=(id == UserGroupMember.usergroup_id)
                    & ((UserGroupMember.membership_end == None) | (
                func.current_timestamp() < UserGroupMember.membership_end)),
        cascade="all, delete-orphan",
        back_populates="group",
    )
    accesses = db.relationship('BlockAccess', back_populates='usergroup', lazy='dynamic')
    accesses_alt = db.relationship('BlockAccess')
    readparagraphs = db.relationship('ReadParagraph', back_populates='usergroup', lazy='dynamic')
    readparagraphs_alt = db.relationship('ReadParagraph')
    notes = db.relationship('UserNote', back_populates='usergroup', lazy='dynamic')
    notes_alt = db.relationship('UserNote')

    admin_doc = db.relationship(
        'Block',
        secondary=UserGroupDoc.__table__,
        lazy='select',
        uselist=False,
    )

    # For groups created from SCIM API
    external_id: ScimUserGroup = db.relationship('ScimUserGroup', lazy='select', uselist=False)

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
        return self.name in LARGE_GROUPS

    def to_json(self):
        r = {
            'id': self.id,
            'name': self.name,
            **include_if_exists('personal_user', self),
        }
        if is_attribute_loaded('admin_doc', self):
            if self.admin_doc:
                r['admin_doc'] = self.admin_doc.docentries[0]
        return r

    def get_cumulative(self):
        if not self.is_sisu:
            raise Exception('Tried to call get_cumulative for a non-Sisu group.')
        return UserGroup.get_by_name(CUMULATIVE_GROUP_PREFIX + self.external_id.external_id)

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
        db.session.flush()
        group_id = ug.id
        assert group_id is not None and group_id != 0, 'group_id was None'
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
    def get_korppi_group() -> 'UserGroup':
        return UserGroup.query.filter_by(name=KORPPI_GROUPNAME).one()

    @staticmethod
    def get_teachers_group() -> 'UserGroup':
        return UserGroup.query.filter_by(name=TEACHERS_GROUPNAME).one()

    @staticmethod
    def get_logged_in_group() -> 'UserGroup':
        return UserGroup.query.filter_by(name=LOGGED_IN_GROUPNAME).one()


def get_usergroup_eager_query():
    from timApp.item.block import Block
    return UserGroup.query.options(joinedload(UserGroup.admin_doc).joinedload(Block.docentries))


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

# Non-shrinking counterpart of a SCIM group. If a member is added to a SCIM group via SCIM route,
# it is also added to the corresponding cumulative group. No members are ever deleted from the
# cumulative group.
CUMULATIVE_GROUP_PREFIX = 'cumulative:'
