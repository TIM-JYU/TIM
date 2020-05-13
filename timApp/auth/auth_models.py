from typing import Optional

from timApp.auth.accesstype import AccessType
from timApp.timdb.sqa import db, include_if_loaded
from timApp.util.utils import get_current_time


class AccessTypeModel(db.Model):
    """A kind of access that a UserGroup may have to a Block."""
    __tablename__ = 'accesstype'
    id = db.Column(db.Integer, primary_key=True)
    """Access type identifier."""

    name = db.Column(db.Text, nullable=False)
    """Access type name, such as 'view', 'edit', 'manage', etc."""

    accesses = db.relationship('BlockAccess', back_populates='atype')

    def __json__(self):
        return ['id', 'name']

    def to_enum(self):
        return AccessType(self.id)


class BlockAccess(db.Model):
    """A single permission. Relates a UserGroup with a Block along with an AccessType."""
    __tablename__ = 'blockaccess'
    block_id = db.Column(db.Integer, db.ForeignKey('block.id'), primary_key=True)
    usergroup_id = db.Column(db.Integer, db.ForeignKey('usergroup.id'), primary_key=True)
    type = db.Column(db.Integer, db.ForeignKey('accesstype.id'), primary_key=True)
    accessible_from = db.Column(db.DateTime(timezone=True))
    accessible_to = db.Column(db.DateTime(timezone=True))
    duration = db.Column(db.Interval)
    duration_from = db.Column(db.DateTime(timezone=True))
    duration_to = db.Column(db.DateTime(timezone=True))
    require_confirm = db.Column(db.Boolean)

    block = db.relationship('Block', back_populates='accesses')
    usergroup = db.relationship('UserGroup', back_populates='accesses')
    atype = db.relationship('AccessTypeModel', back_populates='accesses')

    @property
    def group_collection_key(self):
        return self.block_id, self.type

    @property
    def block_collection_key(self):
        return self.usergroup_id, self.type

    @property
    def future(self):
        return self.accessible_from is not None and get_current_time() < self.accessible_from

    @property
    def access_type(self):
        return AccessType(self.type)

    @property
    def expired(self):
        return self.accessible_to is not None and get_current_time() > self.accessible_to

    @property
    def unlockable(self):
        return (self.accessible_from is None and
                self.duration is not None and
                not self.duration_future and
                not self.duration_expired and
                not self.require_confirm)

    @property
    def duration_future(self):
        return self.duration_from and get_current_time() < self.duration_from

    @property
    def duration_expired(self):
        return self.duration_to and get_current_time() >= self.duration_to

    @property
    def time_until_access_start(self) -> Optional[float]:
        if self.accessible_from is None:
            return None
        return (self.accessible_from - get_current_time()).total_seconds()

    @property
    def seconds_left(self):
        if self.accessible_to is None:
            return None
        return (self.accessible_to - get_current_time()).total_seconds()

    @property
    def info_str(self):
        r = []
        if self.duration:
            r.append(f'duration={self.duration}')
        if self.duration_from:
            r.append(f'duration_from={self.duration_from.isoformat()}')
        if self.duration_to:
            r.append(f'duration_to={self.duration_to.isoformat()}')
        if self.require_confirm:
            r.append(f'require_confirm={self.require_confirm}')
        if self.accessible_from and self.future:
            r.append(f'accessible_from={self.accessible_from.isoformat()}')
        if self.expired:
            r.append('expired')
        elif self.accessible_to:
            r.append(f'accessible_to={self.accessible_to.isoformat()}')
        attrs = ','.join(r)
        return f'{self.access_type.name}({attrs})'

    def do_confirm(self):
        self.require_confirm = False
        if not self.duration:
            self.accessible_from = get_current_time()

    def __hash__(self):
        return hash((self.block_id,
                     self.usergroup_id,
                     self.type,
                     self.accessible_from,
                     self.accessible_to,
                     self.duration,
                     self.duration_from,
                     self.duration_to))

    def __eq__(self, other: 'BlockAccess'):
        return self.block_id == other.block_id and self.usergroup_id == other.usergroup_id and self.type == other.type

    def __ne__(self, other):
        return not self == other

    def to_json(self):
        return {
            'block_id': self.block_id,
            'usergroup_id': self.usergroup_id,
            'type': self.type,
            'accessible_from': self.accessible_from,
            'accessible_to': self.accessible_to,
            'duration': self.duration,
            'duration_from': self.duration_from,
            'duration_to': self.duration_to,
            'require_confirm': self.require_confirm,
            **include_if_loaded('usergroup', self),
        }
