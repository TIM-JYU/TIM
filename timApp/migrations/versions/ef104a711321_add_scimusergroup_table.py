"""Add scimusergroup table

Revision ID: ef104a711321
Revises: 002035b586ad
Create Date: 2019-08-19 15:14:05.991302

"""

# revision identifiers, used by Alembic.

from sqlalchemy import orm, any_, select

from timApp.sisu.scim import derive_scim_group_name
from timApp.sisu.scimusergroup import ScimUserGroup
from timApp.user.usergroup import UserGroup

revision = "ef104a711321"
down_revision = "002035b586ad"

from alembic import op
import sqlalchemy as sa


def upgrade():
    op.create_table(
        "scimusergroup",
        sa.Column("group_id", sa.Integer(), nullable=False),
        sa.Column("external_id", sa.Text(), nullable=False),
        sa.ForeignKeyConstraint(
            ["group_id"],
            ["usergroup.id"],
        ),
        sa.PrimaryKeyConstraint("group_id"),
        sa.UniqueConstraint("external_id"),
    )
    bind = op.get_bind()
    s = orm.Session(bind=bind)

    ugs: list[UserGroup] = (
        s.execute(select(UserGroup).filter(UserGroup.name.startswith("sisu:")))
        .scalars()
        .all()
    )
    used_names = set()
    for ug in ugs:
        external_id = ug.name.replace("sisu:", "")
        ug.external_id = ScimUserGroup(external_id=external_id)
        default_name = derive_scim_group_name(ug.display_name)
        if default_name in used_names:
            default_name += "-1"
        used_names.add(default_name)
        ug.name = default_name

    ugs: list[UserGroup] = (
        s.execute(
            select(UserGroup).filter(
                UserGroup.name.like(any_(["deleted:sisu:%", "cumulative:sisu:%"]))
            )
        )
        .scalars()
        .all()
    )
    for ug in ugs:
        ug.name = ug.name.replace("sisu:", "")
    s.flush()


def downgrade():
    raise NotImplementedError
