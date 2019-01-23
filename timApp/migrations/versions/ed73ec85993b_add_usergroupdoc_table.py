"""Add usergroupdoc table

Revision ID: ed73ec85993b
Revises: 2b268a2d1cc2
Create Date: 2019-01-23 16:22:04.869777

"""

# revision identifiers, used by Alembic.
revision = 'ed73ec85993b'
down_revision = '2b268a2d1cc2'

from alembic import op
import sqlalchemy as sa


def upgrade():
    op.create_table('usergroupdoc',
    sa.Column('group_id', sa.Integer(), nullable=False),
    sa.Column('doc_id', sa.Integer(), nullable=False),
    sa.ForeignKeyConstraint(['doc_id'], ['block.id'], ),
    sa.ForeignKeyConstraint(['group_id'], ['usergroup.id'], ),
    sa.PrimaryKeyConstraint('group_id', 'doc_id')
    )


def downgrade():
    op.drop_table('usergroupdoc')
