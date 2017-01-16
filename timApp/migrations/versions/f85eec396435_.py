"""Adds duration_from and duration_to columns to blockaccess.

Revision ID: f85eec396435
Revises: c1cdbcbc3450
Create Date: 2017-01-16 09:38:25.369459

"""

# revision identifiers, used by Alembic.
revision = 'f85eec396435'
down_revision = 'c1cdbcbc3450'

from alembic import op
import sqlalchemy as sa


def upgrade():
    op.add_column('blockaccess', sa.Column('duration_from', sa.DateTime(timezone=True), nullable=True))
    op.add_column('blockaccess', sa.Column('duration_to', sa.DateTime(timezone=True), nullable=True))


def downgrade():
    op.drop_column('blockaccess', 'duration_to')
    op.drop_column('blockaccess', 'duration_from')
