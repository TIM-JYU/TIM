"""Remove not-null constraint from InternalMessageReadReceipt

Revision ID: 9f18815ce8cb
Revises: dd105e441f5f
Create Date: 2021-04-26 09:08:43.497876

"""

# revision identifiers, used by Alembic.
revision = '9f18815ce8cb'
down_revision = 'dd105e441f5f'

from alembic import op
import sqlalchemy as sa


def upgrade():
    op.alter_column('internalmessage_readreceipt', 'user_id',
                    existing_type=sa.INTEGER(),
                    nullable=True)


def downgrade():
    op.alter_column('internalmessage_readreceipt', 'user_id',
                    existing_type=sa.INTEGER(),
                    nullable=False)
