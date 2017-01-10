"""Adds duration column to blockaccess.

Revision ID: c1cdbcbc3450
Revises: 88c3e145844b
Create Date: 2017-01-05 16:28:22.335641

"""

# revision identifiers, used by Alembic.
revision = 'c1cdbcbc3450'
down_revision = '88c3e145844b'

from alembic import op
import sqlalchemy as sa


def upgrade():
    op.add_column('blockaccess', sa.Column('duration', sa.Interval(), nullable=True))


def downgrade():
    op.drop_column('blockaccess', 'duration')
