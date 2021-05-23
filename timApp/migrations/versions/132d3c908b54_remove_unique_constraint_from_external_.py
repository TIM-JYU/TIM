"""Remove unique constraint from external member's email address.

Revision ID: 132d3c908b54
Revises: 5512ad2fb9f2
Create Date: 2021-05-23 14:58:18.367535

"""
from alembic import op

# revision identifiers, used by Alembic.
revision = '132d3c908b54'
down_revision = '5512ad2fb9f2'


def upgrade():
    op.drop_constraint('messagelist_external_member_email_address_key', 'messagelist_external_member', type_='unique')


def downgrade():
    op.create_unique_constraint('messagelist_external_member_email_address_key', 'messagelist_external_member',
                                ['email_address'])
