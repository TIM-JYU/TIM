"""Add created and modified columns to usergroup

Revision ID: 17adb0f3aac7
Revises: 0bfe1ecb8638
Create Date: 2019-05-24 10:08:51.309803

"""

revision = '17adb0f3aac7'
down_revision = '0bfe1ecb8638'

import sqlalchemy as sa
from alembic import op


def upgrade():
    op.add_column('usergroup', sa.Column('created', sa.DateTime(timezone=True), nullable=True))
    op.add_column('usergroup', sa.Column('modified', sa.DateTime(timezone=True), nullable=True))


def downgrade():
    op.drop_column('usergroup', 'modified')
    op.drop_column('usergroup', 'created')
