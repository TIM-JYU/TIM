"""Add display_name column to usergroup

Revision ID: 6c0f35cfea91
Revises: 17adb0f3aac7
Create Date: 2019-05-27 12:15:47.454541

"""

# revision identifiers, used by Alembic.
revision = '6c0f35cfea91'
down_revision = '17adb0f3aac7'

from alembic import op
import sqlalchemy as sa


def upgrade():
    op.add_column('usergroup', sa.Column('display_name', sa.Text(), nullable=True))


def downgrade():
    op.drop_column('usergroup', 'display_name')
