"""Add created and modified columns to useraccount

Revision ID: 239628f4509d
Revises: 161acd6d2da9
Create Date: 2019-05-29 12:18:21.982308

"""

# revision identifiers, used by Alembic.
revision = '239628f4509d'
down_revision = '161acd6d2da9'

from alembic import op
import sqlalchemy as sa


def upgrade():
    op.add_column('useraccount', sa.Column('created', sa.DateTime(timezone=True), nullable=True))
    op.add_column('useraccount', sa.Column('modified', sa.DateTime(timezone=True), nullable=True))


def downgrade():
    op.drop_column('useraccount', 'modified')
    op.drop_column('useraccount', 'created')
