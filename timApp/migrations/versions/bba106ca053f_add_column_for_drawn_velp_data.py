"""Add column for drawn velp data

Revision ID: bba106ca053f
Revises: 6c42973d8ea7
Create Date: 2020-08-19 20:13:41.611061

"""

# revision identifiers, used by Alembic.
revision = 'bba106ca053f'
down_revision = '6c42973d8ea7'

from alembic import op
import sqlalchemy as sa


def upgrade():
    op.add_column('annotation', sa.Column('draw_data', sa.Text(), nullable=True))


def downgrade():
    op.drop_column('annotation', 'draw_data')
