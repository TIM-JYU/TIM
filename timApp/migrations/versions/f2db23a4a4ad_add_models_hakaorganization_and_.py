"""Add models HakaOrganization and PersonalUniqueCode

Revision ID: f2db23a4a4ad
Revises: 6978d5413cef
Create Date: 2020-01-23 12:09:03.711175

"""

# revision identifiers, used by Alembic.
revision = 'f2db23a4a4ad'
down_revision = '6978d5413cef'

from alembic import op
import sqlalchemy as sa


def upgrade():
    op.create_table('haka_organization',
    sa.Column('id', sa.Integer(), nullable=False),
    sa.Column('name', sa.Text(), nullable=False),
    sa.PrimaryKeyConstraint('id'),
    sa.UniqueConstraint('name')
    )
    op.create_table('personal_unique_code',
    sa.Column('user_id', sa.Integer(), nullable=False),
    sa.Column('org_id', sa.Integer(), nullable=False),
    sa.Column('code', sa.Text(), nullable=False),
    sa.Column('type', sa.Text(), nullable=False),
    sa.ForeignKeyConstraint(['org_id'], ['haka_organization.id'], ),
    sa.ForeignKeyConstraint(['user_id'], ['useraccount.id'], ),
    sa.PrimaryKeyConstraint('user_id', 'org_id', 'type'),
    sa.UniqueConstraint('org_id', 'code', 'type')
    )
    op.create_index(op.f('ix_personal_unique_code_code'), 'personal_unique_code', ['code'], unique=False)


def downgrade():
    op.drop_index(op.f('ix_personal_unique_code_code'), table_name='personal_unique_code')
    op.drop_table('personal_unique_code')
    op.drop_table('haka_organization')
