"""Update message list related tables.

Revision ID: 5512ad2fb9f2
Revises: c00fceabf513
Create Date: 2021-05-07 08:17:26.542982

The creation date is before the previous migration file, because the development got forked from a common file.
"""

# revision identifiers, used by Alembic.
revision = '5512ad2fb9f2'
down_revision = 'c00fceabf513'

from alembic import op
import sqlalchemy as sa

replytolistchanges_enum = sa.Enum('NOCHANGES', 'ADDLIST', name='replytolistchanges')
memberjoinmethod_enum = sa.Enum('DIRECT_ADD', 'INVITED', 'JOINED', name='memberjoinmethod')


def upgrade():
    replytolistchanges_enum.create(op.get_bind(), checkfirst=True)
    # memberjoinmethod_enum.create(op.get_bind(), checkfirst=True)
    op.add_column('messagelist', sa.Column('allow_attachments', sa.Boolean(), nullable=True))
    op.add_column('messagelist', sa.Column('default_delivery_right', sa.Boolean(), nullable=True))
    op.add_column('messagelist', sa.Column('default_reply_type', replytolistchanges_enum, nullable=True))
    op.add_column('messagelist', sa.Column('default_send_right', sa.Boolean(), nullable=True))
    op.add_column('messagelist', sa.Column('non_member_message_pass', sa.Boolean(), nullable=True))
    op.add_column('messagelist', sa.Column('only_text', sa.Boolean(), nullable=True))
    op.add_column('messagelist', sa.Column('subject_prefix', sa.Text(), nullable=True))
    op.add_column('messagelist', sa.Column('tim_user_can_join', sa.Boolean(), nullable=True))
    op.add_column('messagelist_distribution', sa.Column('message_list_id', sa.Integer(), nullable=True))
    op.add_column('messagelist_distribution', sa.Column('user_id', sa.Integer(), nullable=True))
    op.drop_constraint('messagelist_distribution_id_fkey', 'messagelist_distribution', type_='foreignkey')
    op.create_foreign_key(None, 'messagelist_distribution', 'messagelist', ['message_list_id'], ['id'])
    op.create_foreign_key(None, 'messagelist_distribution', 'messagelist_member', ['user_id'], ['id'])
    op.add_column('messagelist_external_member', sa.Column('display_name', sa.Text(), nullable=True))
    op.add_column('messagelist_member', sa.Column('join_method', memberjoinmethod_enum, nullable=True))


def downgrade():
    op.drop_column('messagelist_member', 'join_method')
    op.drop_column('messagelist_external_member', 'display_name')
    op.drop_constraint(None, 'messagelist_distribution', type_='foreignkey')
    op.drop_constraint(None, 'messagelist_distribution', type_='foreignkey')
    op.create_foreign_key('messagelist_distribution_id_fkey', 'messagelist_distribution', 'messagelist_member', ['id'], ['id'])
    op.drop_column('messagelist_distribution', 'user_id')
    op.drop_column('messagelist_distribution', 'message_list_id')
    op.drop_column('messagelist', 'tim_user_can_join')
    op.drop_column('messagelist', 'subject_prefix')
    op.drop_column('messagelist', 'only_text')
    op.drop_column('messagelist', 'non_member_message_pass')
    op.drop_column('messagelist', 'default_send_right')
    op.drop_column('messagelist', 'default_reply_type')
    op.drop_column('messagelist', 'default_delivery_right')
    op.drop_column('messagelist', 'allow_attachments')
    replytolistchanges_enum.drop(op.get_bind())
    memberjoinmethod_enum.drop(op.get_bind())
