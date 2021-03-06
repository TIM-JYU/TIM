"""Add usersession

Revision ID: 923e66e7b8ae
Revises: c4b0954330a3
Create Date: 2022-04-15 07:56:43.354644

"""

# revision identifiers, used by Alembic.
revision = "923e66e7b8ae"
down_revision = "c22914d260b3"

import sqlalchemy as sa
from alembic import op


def upgrade():
    # ### commands auto generated by Alembic - please adjust! ###
    op.create_table(
        "usersession",
        sa.Column("user_id", sa.Integer(), nullable=False),
        sa.Column("session_id", sa.Text(), nullable=False),
        sa.Column("logged_in_at", sa.DateTime(), nullable=False),
        sa.Column("expired_at", sa.DateTime(), nullable=True),
        sa.Column("origin", sa.Text(), nullable=False),
        sa.ForeignKeyConstraint(
            ["user_id"],
            ["useraccount.id"],
        ),
        sa.PrimaryKeyConstraint("user_id", "session_id"),
    )
    # ### end Alembic commands ###


def downgrade():
    # ### commands auto generated by Alembic - please adjust! ###
    op.drop_table("usersession")
    # ### end Alembic commands ###
