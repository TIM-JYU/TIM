"""Add require_confirm to blockaccess

Revision ID: 6978d5413cef
Revises: a6021d1d2975
Create Date: 2019-11-25 08:55:03.686317

"""

# revision identifiers, used by Alembic.
revision = '6978d5413cef'
down_revision = 'a6021d1d2975'

from alembic import op
import sqlalchemy as sa


def upgrade():
    op.add_column('blockaccess', sa.Column('require_confirm', sa.Boolean(), nullable=True))


def downgrade():
    op.drop_column('blockaccess', 'require_confirm')
