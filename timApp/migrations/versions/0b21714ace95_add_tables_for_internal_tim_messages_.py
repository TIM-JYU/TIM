"""Add tables for internal TIM messages, message displays and read receipts.

Revision ID: 0b21714ace95
Revises: 11bc200da4f8
Create Date: 2021-04-20 11:54:03.737256

"""

# revision identifiers, used by Alembic.
revision = "0b21714ace95"
down_revision = "11bc200da4f8"

from alembic import op
import sqlalchemy as sa

display_type_enum = sa.Enum("TOP_OF_PAGE", "STICKY", name="displaytype")


def upgrade():
    # display_type_enum.create(op.get_bind(), checkfirst=True)
    op.create_table(
        "internalmessage",
        sa.Column("id", sa.Integer(), nullable=False),
        sa.Column("doc_id", sa.Integer(), nullable=False),
        sa.Column("par_id", sa.Text(), nullable=False),
        sa.Column("can_mark_as_read", sa.Boolean(), nullable=False),
        sa.Column("reply", sa.Boolean(), nullable=False),
        sa.Column("display_type", display_type_enum, nullable=False),
        sa.ForeignKeyConstraint(
            ["doc_id"],
            ["block.id"],
        ),
        sa.PrimaryKeyConstraint("id"),
    )
    op.create_table(
        "internalmessage_display",
        sa.Column("id", sa.Integer(), nullable=False),
        sa.Column("message_id", sa.Integer(), nullable=False),
        sa.Column("usergroup_id", sa.Integer(), nullable=True),
        sa.Column("display_doc_id", sa.Integer(), nullable=True),
        sa.ForeignKeyConstraint(
            ["display_doc_id"],
            ["block.id"],
        ),
        sa.ForeignKeyConstraint(
            ["message_id"],
            ["internalmessage.id"],
        ),
        sa.ForeignKeyConstraint(
            ["usergroup_id"],
            ["usergroup.id"],
        ),
        sa.PrimaryKeyConstraint("id"),
    )
    op.create_table(
        "internalmessage_readreceipt",
        sa.Column("rcpt_id", sa.Integer(), nullable=False),
        sa.Column("message_id", sa.Integer(), nullable=False),
        sa.Column("user_id", sa.Integer(), nullable=False),
        sa.Column("marked_as_read_on", sa.DateTime(), nullable=True),
        sa.ForeignKeyConstraint(
            ["rcpt_id"],
            ["usergroup.id"],
        ),
        sa.ForeignKeyConstraint(
            ["message_id"],
            ["internalmessage.id"],
        ),
        sa.ForeignKeyConstraint(
            ["user_id"],
            ["useraccount.id"],
        ),
        sa.PrimaryKeyConstraint("rcpt_id", "message_id"),
    )


def downgrade():
    op.drop_table("internalmessage_readreceipt")
    op.drop_table("internalmessage_display")
    op.drop_table("internalmessage")
    display_type_enum.drop(op.get_bind())
