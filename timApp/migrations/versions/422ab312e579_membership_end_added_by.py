"""Add membership_end and added_by columns to usergroupmember

Revision ID: 422ab312e579
Revises: ef104a711321
Create Date: 2019-09-04 06:39:22.902132

"""
from typing import Any

from sqlalchemy import select
from sqlalchemy.orm import scoped_session, sessionmaker

from timApp.sisu.scimusergroup import ScimUserGroup
from timApp.timdb.sqa import db
from timApp.user.user import User
from timApp.user.usergroup import UserGroup
from timApp.user.usergroupmember import UserGroupMember

revision = "422ab312e579"
down_revision = "ef104a711321"

from alembic import op
import sqlalchemy as sa


def upgrade():
    op.add_column("usergroupmember", sa.Column("added_by", sa.Integer(), nullable=True))
    op.add_column(
        "usergroupmember",
        sa.Column("membership_end", sa.DateTime(timezone=True), nullable=True),
    )
    op.create_foreign_key(None, "usergroupmember", "useraccount", ["added_by"], ["id"])
    bind = op.get_bind()
    tmp: Any = scoped_session(session_factory=sessionmaker(bind=bind))
    db.session = tmp
    ugs: list[tuple[UserGroup, ScimUserGroup]] = (
        db.session.execute(select(UserGroup, ScimUserGroup).join(ScimUserGroup))
        .scalars()
        .all()
    )
    su = User.get_scimuser()
    for ug, sg in ugs:
        # FIXME: SQLAlchemy dynamic
        ms = (
            ug.memberships.filter(
                UserGroupMember.user_id.in_(
                    UserGroup.query.filter_by(name="cumulative:" + sg.external_id)
                    .join(User, UserGroup.users)
                    .with_entities(User.id)
                    .subquery()
                )
            )
        ).all()
        for m in ms:
            m.adder = su
    db.session.flush()


def downgrade():
    op.drop_constraint(
        "usergroupmember_added_by_fkey", "usergroupmember", type_="foreignkey"
    )
    op.drop_column("usergroupmember", "membership_end")
    op.drop_column("usergroupmember", "added_by")
