"""Add peer review table

Revision ID: ec854efee168
Revises: bba106ca053f
Create Date: 2020-08-27 10:45:01.304867

"""

# revision identifiers, used by Alembic.
revision = 'ec854efee168'
down_revision = 'bba106ca053f'

from alembic import op
import sqlalchemy as sa


def upgrade():
    op.create_table('peer_review',
    sa.Column('id', sa.Integer(), nullable=False),
    sa.Column('answer_id', sa.Integer(), nullable=True),
    sa.Column('task_name', sa.Text(), nullable=False),
    sa.Column('block_id', sa.Integer(), nullable=False),
    sa.Column('reviewer_id', sa.Integer(), nullable=False),
    sa.Column('reviewable_id', sa.Integer(), nullable=False),
    sa.Column('start_time', sa.DateTime(timezone=True), nullable=False),
    sa.Column('end_time', sa.DateTime(timezone=True), nullable=False),
    sa.Column('reviewed', sa.Boolean(), nullable=True),
    sa.ForeignKeyConstraint(['answer_id'], ['answer.id'], ),
    sa.ForeignKeyConstraint(['block_id'], ['block.id'], ),
    sa.ForeignKeyConstraint(['reviewable_id'], ['useraccount.id'], ),
    sa.ForeignKeyConstraint(['reviewer_id'], ['useraccount.id'], ),
    sa.PrimaryKeyConstraint('id'),
    sa.UniqueConstraint('answer_id', 'block_id', 'reviewer_id', 'reviewable_id'),
    sa.UniqueConstraint('task_name', 'block_id', 'reviewer_id', 'reviewable_id')
    )


def downgrade():
    op.drop_table('peer_review')
