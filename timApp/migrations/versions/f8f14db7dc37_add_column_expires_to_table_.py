"""add column expires to table internalmessage

Revision ID: f8f14db7dc37
Revises: 9f18815ce8cb
Create Date: 2021-05-06 07:50:35.874633

"""

# revision identifiers, used by Alembic.
revision = 'f8f14db7dc37'
down_revision = '9f18815ce8cb'

from alembic import op
import sqlalchemy as sa


def upgrade():
    op.add_column('internalmessage', sa.Column('expires', sa.DateTime(), nullable=True))


def downgrade():
    op.drop_column('internalmessage', 'expires')
