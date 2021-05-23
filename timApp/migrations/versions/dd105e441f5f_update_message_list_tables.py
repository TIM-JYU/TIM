"""Update message list tables.

Revision ID: dd105e441f5f
Revises: 38e714c6b20d
Create Date: 2021-04-22 19:32:49.960011

"""

# revision identifiers, used by Alembic.
revision = 'dd105e441f5f'
down_revision = '38e714c6b20d'

from alembic import op
import sqlalchemy as sa

messagelist_distribution_enum = sa.Enum('TIM_MESSAGE', 'EMAIL_LIST', name='channel')


def upgrade():
    messagelist_distribution_enum.create(op.get_bind(), checkfirst=True)
    op.add_column('messagelist', sa.Column('email_list_domain', sa.Text(), nullable=True))
    op.add_column('messagelist', sa.Column('removed', sa.DateTime(timezone=True), nullable=True))
    op.add_column('messagelist_distribution', sa.Column('channel', messagelist_distribution_enum, nullable=True))
    op.drop_column('messagelist_distribution', 'channel_id')
    op.add_column('messagelist_member', sa.Column('membership_ended', sa.DateTime(timezone=True), nullable=True))
    op.add_column('messagelist_member', sa.Column('membership_verified', sa.DateTime(timezone=True), nullable=True))


def downgrade():
    op.drop_column('messagelist_member', 'membership_verified')
    op.drop_column('messagelist_member', 'membership_ended')
    op.add_column('messagelist_distribution', sa.Column('channel_id', messagelist_distribution_enum,
                                                        autoincrement=False, nullable=True))
    op.drop_column('messagelist_distribution', 'channel')
    op.drop_column('messagelist', 'removed')
    op.drop_column('messagelist', 'email_list_domain')
    messagelist_distribution_enum.drop(op.get_bind())
