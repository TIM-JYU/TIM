"""Add UserOrigin.JSRunner

Revision ID: 14a649d37bc3
Revises: bccb1595773d
Create Date: 2023-03-08 13:17:27.647959

"""


# revision identifiers, used by Alembic.
revision = "14a649d37bc3"
down_revision = "bccb1595773d"

from alembic import op


def upgrade():
    with op.get_context().autocommit_block():
        op.execute("""ALTER TYPE userorigin ADD VALUE IF NOT EXISTS 'JSRunner'""")


def downgrade():
    pass
