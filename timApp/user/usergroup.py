from typing import List

from timApp.user.special_group_names import ANONYMOUS_GROUPNAME, LARGE_GROUPS, KORPPI_GROUPNAME, LOGGED_IN_GROUPNAME, \
    ADMIN_GROUPNAME
from timApp.timdb.sqa import db
from timApp.user.usergroupmember import UserGroupMember


class UserGroup(db.Model):
    """A usergroup. Each User should belong to a personal UserGroup that has the same name as the User name. No one
    else should belong to a personal UserGroup.

    A User can additionally belong to any number of other UserGroups.

    Two special groups named 'Logged-in users' and 'Anonymous users' denote the set of all logged-in users and all
    users including anonymous (not logged-in) ones, respectively.

    In database, the User 'Anonymous user' belongs to 'Anonymous users' group. Other than that,
    the two groups are empty from the database's point of view.
    """
    __bind_key__ = 'tim_main'
    __tablename__ = 'usergroup'
    id = db.Column(db.Integer, primary_key=True)
    """Usergroup identifier."""

    name = db.Column(db.Text, nullable=False, unique=True)
    """Usergroup name."""

    users = db.relationship('User', secondary=UserGroupMember.__table__,
                            back_populates='groups', lazy='dynamic')
    accesses = db.relationship('BlockAccess', back_populates='usergroup', lazy='dynamic')
    readparagraphs = db.relationship('ReadParagraph', back_populates='usergroup', lazy='dynamic')
    notes = db.relationship('UserNote', back_populates='usergroup', lazy='dynamic')

    def is_anonymous(self) -> bool:
        return self.name == ANONYMOUS_GROUPNAME

    def is_large(self) -> bool:
        return self.name in LARGE_GROUPS

    def __json__(self) -> List[str]:
        return ['id', 'name']

    @property
    def pretty_full_name(self):
        return self.name

    @staticmethod
    def create(name: str, commit: bool = True) -> 'UserGroup':
        """Creates a new user group.

        :param name: The name of the user group.
        :returns: The id of the created user group.

        """

        ug = UserGroup(name=name)
        db.session.add(ug)
        db.session.flush()
        group_id = ug.id
        assert group_id is not None and group_id != 0, 'group_id was None'
        if commit:
            db.session.commit()
        return ug

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
    def get_korppi_group() -> 'UserGroup':
        return UserGroup.query.filter_by(name=KORPPI_GROUPNAME).one()

    @staticmethod
    def get_logged_in_group() -> 'UserGroup':
        return UserGroup.query.filter_by(name=LOGGED_IN_GROUPNAME).one()
