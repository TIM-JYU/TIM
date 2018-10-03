"""Add index to pendingnotification.processed.

Revision ID: 27c624d9244a
Revises: dadad1cd3387
Create Date: 2018-10-03 11:54:39.565766

"""

revision = '27c624d9244a'
down_revision = 'dadad1cd3387'

from alembic import op


def upgrade():
    op.create_index(op.f('ix_pendingnotification_processed'), 'pendingnotification', ['processed'], unique=False)


def downgrade():
    op.drop_index(op.f('ix_pendingnotification_processed'), table_name='pendingnotification')
