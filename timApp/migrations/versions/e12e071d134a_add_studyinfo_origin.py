"""Add StudyInfo origin

Revision ID: e12e071d134a
Revises: a6614a0e59d3
Create Date: 2022-11-21 13:54:43.580329

"""
from alembic import op

# revision identifiers, used by Alembic.
revision = "e12e071d134a"
down_revision = "a6614a0e59d3"


def upgrade():
    with op.get_context().autocommit_block():
        op.execute("""ALTER TYPE userorigin ADD VALUE IF NOT EXISTS 'StudyInfo'""")
        op.execute("""ALTER TYPE contactorigin ADD VALUE IF NOT EXISTS 'StudyInfo'""")


def downgrade():
    pass
