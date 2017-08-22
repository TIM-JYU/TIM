"""Add color table.

Revision ID: f469aa644049
Revises: 209fc28604ba
Create Date: 2016-12-27 19:18:43.665359

"""

# revision identifiers, used by Alembic.
revision = 'f469aa644049'
down_revision = '209fc28604ba'

from alembic import op
import sqlalchemy as sa


def downgrade():
    op.drop_table('color')


def upgrade():
    op.create_table('color',
                    sa.Column('id', sa.INTEGER(), nullable=False),
                    sa.PrimaryKeyConstraint('id', name='color_pkey')
                    )
