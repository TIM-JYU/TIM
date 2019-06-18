"""Add answersaver table

Revision ID: 002035b586ad
Revises: 239628f4509d
Create Date: 2019-06-18 11:56:34.700108

"""

revision = '002035b586ad'
down_revision = '239628f4509d'

from alembic import op
import sqlalchemy as sa


def upgrade():
    op.create_table(
        'answersaver',
        sa.Column('answer_id', sa.Integer(), nullable=False),
        sa.Column('user_id', sa.Integer(), nullable=False),
        sa.ForeignKeyConstraint(['answer_id'], ['answer.id'], ),
        sa.ForeignKeyConstraint(['user_id'], ['useraccount.id'], ),
        sa.PrimaryKeyConstraint('answer_id', 'user_id')
    )


def downgrade():
    op.drop_table('answersaver')
