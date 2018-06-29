"""Create blockassociation table

Revision ID: 801381ef8cfc
Revises: b90f139826dc
Create Date: 2018-06-29 07:41:13.981072

"""

# revision identifiers, used by Alembic.
revision = '801381ef8cfc'
down_revision = '010b7b2d8521'

import sqlalchemy as sa
from alembic import op


def upgrade():
    op.create_table(
        'blockassociation',
        sa.Column('parent', sa.Integer(), nullable=False),
        sa.Column('child', sa.Integer(), nullable=False),
        sa.ForeignKeyConstraint(['child'], ['block.id'], ),
        sa.ForeignKeyConstraint(['parent'], ['block.id'], ),
        sa.PrimaryKeyConstraint('parent', 'child')
    )


def downgrade():
    op.drop_table('blockassociation')
