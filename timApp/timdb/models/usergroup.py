from typing import List

from timApp.timdb.tim_models import db, UserGroupMember
from timApp.timdb.special_group_names import ANONYMOUS_GROUPNAME, LARGE_GROUPS, KORPPI_GROUPNAME, LOGGED_IN_GROUPNAME, \
    ADMIN_GROUPNAME


class UserGroup(db.Model):
    __bind_key__ = 'tim_main'
    __tablename__ = 'usergroup'
    id = db.Column(db.Integer, primary_key=True)
    name = db.Column(db.Text, nullable=False, unique=True)

    users = db.relationship('User', secondary=UserGroupMember.__table__,
                            back_populates='groups', lazy='dynamic')
    accesses = db.relationship('BlockAccess', back_populates='usergroup', lazy='dynamic')

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
