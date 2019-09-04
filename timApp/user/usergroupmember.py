from sqlalchemy import func

from timApp.timdb.sqa import db
from timApp.util.utils import get_current_time


class UserGroupMember(db.Model):
    """Associates Users with UserGroups."""
    __tablename__ = 'usergroupmember'
    usergroup_id = db.Column(db.Integer, db.ForeignKey('usergroup.id'), primary_key=True)
    user_id = db.Column(db.Integer, db.ForeignKey('useraccount.id'), primary_key=True)
    membership_end = db.Column(db.DateTime(timezone=True))
    added_by = db.Column(db.Integer, db.ForeignKey('useraccount.id'))

    user = db.relationship(
        'User',
        foreign_keys=[user_id],
    )
    adder = db.relationship(
        'User',
        foreign_keys=[added_by],
    )
    group = db.relationship(
        'UserGroup',
    )

    def set_expired(self):
        self.membership_end = get_current_time()


membership_active = ((UserGroupMember.membership_end == None) | (
        func.current_timestamp() < UserGroupMember.membership_end))

membership_inactive = (func.current_timestamp() >= UserGroupMember.membership_end)
