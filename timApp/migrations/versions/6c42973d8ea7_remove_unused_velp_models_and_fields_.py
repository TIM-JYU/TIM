"""Remove unused velp models and fields, add creator to velp label

Revision ID: 6c42973d8ea7
Revises: f2db23a4a4ad
Create Date: 2020-02-25 21:53:50.443342

"""

revision = '6c42973d8ea7'
down_revision = 'f2db23a4a4ad'

from alembic import op
import sqlalchemy as sa
from sqlalchemy.dialects import postgresql


def upgrade():
    op.drop_constraint('annotation_icon_id_fkey', 'annotation', type_='foreignkey')
    op.drop_column('annotation', 'icon_id')
    op.drop_constraint('velp_icon_id_fkey', 'velp', type_='foreignkey')
    op.drop_column('velp', 'icon_id')
    op.create_foreign_key(None, 'velpgroup', 'block', ['id'], ['id'])
    op.drop_column('velpingroup', 'points')
    op.add_column('velplabel', sa.Column('creator_id', sa.Integer(), nullable=True))
    op.create_foreign_key(None, 'velplabel', 'useraccount', ['creator_id'], ['id'])
    op.drop_table('labelinvelpgroup')
    op.drop_table('importedvelpgroups')
    op.drop_table('icon')


def downgrade():
    op.create_table('icon',
    sa.Column('id', sa.INTEGER(), autoincrement=True, nullable=False),
    sa.Column('filename', sa.TEXT(), autoincrement=False, nullable=True),
    sa.PrimaryKeyConstraint('id', name='icon_pkey')
    )
    op.create_table('importedvelpgroups',
    sa.Column('user_group', sa.INTEGER(), autoincrement=False, nullable=False),
    sa.Column('doc_id', sa.INTEGER(), autoincrement=False, nullable=False),
    sa.Column('target_type', sa.INTEGER(), autoincrement=False, nullable=False),
    sa.Column('target_id', sa.TEXT(), autoincrement=False, nullable=False),
    sa.Column('velp_group_id', sa.INTEGER(), autoincrement=False, nullable=False),
    sa.ForeignKeyConstraint(['doc_id'], ['block.id'], name='importedvelpgroups_doc_id_fkey'),
    sa.ForeignKeyConstraint(['user_group'], ['usergroup.id'], name='importedvelpgroups_user_group_fkey'),
    sa.ForeignKeyConstraint(['velp_group_id'], ['velpgroup.id'], name='importedvelpgroups_velp_group_id_fkey'),
    sa.PrimaryKeyConstraint('user_group', 'doc_id', 'target_id', 'velp_group_id', name='importedvelpgroups_pkey')
    )
    op.create_table('labelinvelpgroup',
    sa.Column('velp_group_id', sa.INTEGER(), autoincrement=False, nullable=False),
    sa.Column('group_label_id', sa.INTEGER(), autoincrement=False, nullable=False),
    sa.ForeignKeyConstraint(['group_label_id'], ['velpgrouplabel.id'], name='labelinvelpgroup_group_label_id_fkey'),
    sa.ForeignKeyConstraint(['velp_group_id'], ['velpgroup.id'], name='labelinvelpgroup_velp_group_id_fkey'),
    sa.PrimaryKeyConstraint('velp_group_id', 'group_label_id', name='labelinvelpgroup_pkey')
    )

    op.drop_constraint('velplabel_creator_id_fkey', 'velplabel', type_='foreignkey')
    op.drop_column('velplabel', 'creator_id')
    op.add_column('velpingroup', sa.Column('points', postgresql.DOUBLE_PRECISION(precision=53), autoincrement=False, nullable=True))
    op.drop_constraint('velpgroup_id_fkey', 'velpgroup', type_='foreignkey')
    op.add_column('velp', sa.Column('icon_id', sa.INTEGER(), autoincrement=False, nullable=True))
    op.create_foreign_key('velp_icon_id_fkey', 'velp', 'icon', ['icon_id'], ['id'])
    op.add_column('annotation', sa.Column('icon_id', sa.INTEGER(), autoincrement=False, nullable=True))
    op.create_foreign_key('annotation_icon_id_fkey', 'annotation', 'icon', ['icon_id'], ['id'])
