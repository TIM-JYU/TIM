"""db migrations for badge

Revision ID: c96a3013da5d
Revises: a1485d740d43
Create Date: 2025-03-26 11:56:42.074267

"""

# revision identifiers, used by Alembic.
revision = 'c96a3013da5d'
down_revision = 'a1485d740d43'

from alembic import op
import sqlalchemy as sa


def upgrade():
    # ### commands auto generated by Alembic - please adjust! ###
    op.create_table('badge',
    sa.Column('id', sa.Integer(), nullable=False),
    sa.Column('title', sa.Text(), nullable=False),
    sa.Column('description', sa.Text(), nullable=False),
    sa.Column('color', sa.Text(), nullable=False),
    sa.Column('shape', sa.Text(), nullable=False),
    sa.Column('image', sa.Integer(), nullable=False),
    sa.Column('context_group', sa.Integer(), nullable=False),
    sa.Column('active', sa.Boolean(), nullable=False),
    sa.Column('created_by', sa.Integer(), nullable=False),
    sa.Column('created', sa.DateTime(timezone=True), nullable=False),
    sa.Column('modified_by', sa.Integer(), nullable=True),
    sa.Column('modified', sa.DateTime(timezone=True), nullable=True),
    sa.Column('deleted_by', sa.Integer(), nullable=True),
    sa.Column('deleted', sa.DateTime(timezone=True), nullable=True),
    sa.Column('restored_by', sa.Integer(), nullable=True),
    sa.Column('restored', sa.DateTime(timezone=True), nullable=True),
    sa.ForeignKeyConstraint(['context_group'], ['usergroup.id'], ),
    sa.ForeignKeyConstraint(['created_by'], ['useraccount.id'], ),
    sa.ForeignKeyConstraint(['deleted_by'], ['useraccount.id'], ),
    sa.ForeignKeyConstraint(['modified_by'], ['useraccount.id'], ),
    sa.ForeignKeyConstraint(['restored_by'], ['useraccount.id'], ),
    sa.PrimaryKeyConstraint('id')
    )
    op.create_table('badgegiven',
    sa.Column('id', sa.Integer(), nullable=False),
    sa.Column('message', sa.Text(), nullable=False),
    sa.Column('badge_id', sa.Integer(), nullable=False),
    sa.Column('group_id', sa.Integer(), nullable=False),
    sa.Column('active', sa.Boolean(), nullable=False),
    sa.Column('given_by', sa.Integer(), nullable=False),
    sa.Column('given', sa.DateTime(timezone=True), nullable=False),
    sa.Column('withdrawn_by', sa.Integer(), nullable=True),
    sa.Column('withdrawn', sa.DateTime(timezone=True), nullable=True),
    sa.Column('undo_withdrawn_by', sa.Integer(), nullable=True),
    sa.Column('undo_withdrawn', sa.DateTime(timezone=True), nullable=True),
    sa.ForeignKeyConstraint(['badge_id'], ['badge.id'], ),
    sa.ForeignKeyConstraint(['given_by'], ['useraccount.id'], ),
    sa.ForeignKeyConstraint(['group_id'], ['usergroup.id'], ),
    sa.ForeignKeyConstraint(['undo_withdrawn_by'], ['useraccount.id'], ),
    sa.ForeignKeyConstraint(['withdrawn_by'], ['useraccount.id'], ),
    sa.PrimaryKeyConstraint('id')
    )
    # ### end Alembic commands ###


def downgrade():
    # ### commands auto generated by Alembic - please adjust! ###
    op.drop_table('badgegiven')
    op.drop_table('badge')
    # ### end Alembic commands ###
