"""Add column replies_to to table internalmessage

Revision ID: c00fceabf513
Revises: f8f14db7dc37
Create Date: 2021-05-10 08:14:04.419147

"""

# revision identifiers, used by Alembic.
revision = 'c00fceabf513'
down_revision = 'f8f14db7dc37'

from alembic import op
import sqlalchemy as sa


def upgrade():
    op.add_column('internalmessage', sa.Column('replies_to', sa.Integer(), nullable=True))


def downgrade():
    op.drop_column('internalmessage', 'replies_to')
