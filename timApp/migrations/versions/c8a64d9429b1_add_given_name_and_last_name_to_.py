"""Add given_name and last_name to useraccount

Revision ID: c8a64d9429b1
Revises: 422ab312e579
Create Date: 2019-09-10 10:18:32.193828

"""

revision = 'c8a64d9429b1'
down_revision = '422ab312e579'

from alembic import op
import sqlalchemy as sa


def upgrade():
    op.add_column('useraccount', sa.Column('given_name', sa.Text(), nullable=True))
    op.add_column('useraccount', sa.Column('last_name', sa.Text(), nullable=True))


def downgrade():
    op.drop_column('useraccount', 'last_name')
    op.drop_column('useraccount', 'given_name')
