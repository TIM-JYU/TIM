"""Add usergroup_subgroups table

Revision ID: 9a878986c7d7
Revises: ced45d32bd7c
Create Date: 2026-09-03 00:00:00.000000

"""

# revision identifiers, used by Alembic.
revision = "9a878986c7d7"
down_revision = "ced45d32bd7c"

from alembic import op
import sqlalchemy as sa


def upgrade():
    op.create_table(
        "usergroup_subgroups",
        sa.Column("child_id", sa.Integer(), nullable=False),
        sa.Column("parent_id", sa.Integer(), nullable=False),
        sa.ForeignKeyConstraint(
            ["child_id"],
            ["usergroup.id"],
        ),
        sa.ForeignKeyConstraint(
            ["parent_id"],
            ["usergroup.id"],
        ),
        # child_id is the primary key: a group can be a subgroup of at most one parent.
        sa.PrimaryKeyConstraint("child_id"),
        sa.CheckConstraint(
            "parent_id <> child_id",
            name="usergroup_subgroups_no_self_reference",
        ),
    )
    op.create_index(
        op.f("ix_usergroup_subgroups_parent_id"),
        "usergroup_subgroups",
        ["parent_id"],
        unique=False,
    )


def downgrade():
    op.drop_index(
        op.f("ix_usergroup_subgroups_parent_id"),
        table_name="usergroup_subgroups",
    )
    op.drop_table("usergroup_subgroups")
