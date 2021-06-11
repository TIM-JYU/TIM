"""Add message list models

Revision ID: 11bc200da4f8
Revises: 0bb9e6d20006
Create Date: 2021-04-15 06:51:33.023232

"""

# revision identifiers, used by Alembic.
revision = '11bc200da4f8'
down_revision = '0bb9e6d20006'

from alembic import op
import sqlalchemy as sa

archive_enum = sa.Enum('NONE', 'SECRET', 'GROUPONLY', 'UNLISTED', 'PUBLIC', name='archivetype')
channel_id_enum = sa.Enum('TIM_MESSAGE', 'EMAIL_LIST', name='channel')


def upgrade():
    # archive_enum.create(op.get_bind(), checkfirst=True)
    # channel_id_enum.create(op.get_bind(), checkfirst=True)
    op.create_table('messagelist',
                    sa.Column('id', sa.Integer(), nullable=False),
                    sa.Column('manage_doc_id', sa.Integer(), nullable=True),
                    sa.Column('name', sa.Text(), nullable=True),
                    sa.Column('can_unsubscribe', sa.Boolean(), nullable=True),
                    sa.Column('archive', archive_enum, nullable=True),
                    sa.Column('notify_owner_on_change', sa.Boolean(), nullable=True),
                    sa.Column('description', sa.Text(), nullable=True),
                    sa.Column('info', sa.Text(), nullable=True),
                    sa.ForeignKeyConstraint(['manage_doc_id'], ['block.id'], ),
                    sa.PrimaryKeyConstraint('id')
                    )
    op.create_table('messagelist_member',
                    sa.Column('id', sa.Integer(), nullable=False),
                    sa.Column('message_list_id', sa.Integer(), nullable=True),
                    sa.Column('send_right', sa.Boolean(), nullable=True),
                    sa.Column('delivery_right', sa.Boolean(), nullable=True),
                    sa.Column('member_type', sa.Text(), nullable=True),
                    sa.ForeignKeyConstraint(['message_list_id'], ['messagelist.id'], ),
                    sa.PrimaryKeyConstraint('id')
                    )
    op.create_table('messagelist_distribution',
                    sa.Column('id', sa.Integer(), nullable=False),
                    sa.Column('channel_id', channel_id_enum, nullable=True),
                    sa.ForeignKeyConstraint(['id'], ['messagelist_member.id'], ),
                    sa.PrimaryKeyConstraint('id')
                    )
    op.create_table('messagelist_external_member',
                    sa.Column('id', sa.Integer(), nullable=False),
                    sa.Column('email_address', sa.Text(), nullable=True),
                    sa.ForeignKeyConstraint(['id'], ['messagelist_member.id'], ),
                    sa.PrimaryKeyConstraint('id'),
                    sa.UniqueConstraint('email_address')
                    )
    op.create_table('messagelist_tim_member',
                    sa.Column('id', sa.Integer(), nullable=False),
                    sa.Column('group_id', sa.Integer(), nullable=True),
                    sa.ForeignKeyConstraint(['group_id'], ['usergroup.id'], ),
                    sa.ForeignKeyConstraint(['id'], ['messagelist_member.id'], ),
                    sa.PrimaryKeyConstraint('id')
                    )


def downgrade():
    op.drop_table('messagelist_tim_member')
    op.drop_table('messagelist_external_member')
    op.drop_table('messagelist_distribution')
    op.drop_table('messagelist_member')
    op.drop_table('messagelist')
    channel_id_enum.drop(op.get_bind())
    archive_enum.drop(op.get_bind())
