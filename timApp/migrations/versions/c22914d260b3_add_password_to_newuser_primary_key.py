"""Add password to newuser primary key

Revision ID: c22914d260b3
Revises: c4b0954330a3
Create Date: 2022-05-09 10:57:06.837016

"""

# revision identifiers, used by Alembic.
from alembic import op

revision = "c22914d260b3"
down_revision = "c4b0954330a3"


def upgrade():
    op.drop_constraint("newuser_pkey", "newuser", type_="primary")
    op.create_primary_key("newuser_pkey", "newuser", ["email", "pass"])


def downgrade():
    op.drop_constraint("newuser_pkey", "newuser", type_="primary")
    op.create_primary_key("newuser_pkey", "newuser", ["email"])
