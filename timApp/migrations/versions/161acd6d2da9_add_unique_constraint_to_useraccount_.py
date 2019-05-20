"""Add unique constraint to useraccount.email

Revision ID: 161acd6d2da9
Revises: 6c0f35cfea91
Create Date: 2019-05-28 11:08:13.431061

"""

# revision identifiers, used by Alembic.
revision = '161acd6d2da9'
down_revision = '6c0f35cfea91'

from alembic import op


def upgrade():
    op.create_unique_constraint('useraccount_email_uc', 'useraccount', ['email'])


def downgrade():
    op.drop_constraint('useraccount_email_uc', 'useraccount', type_='unique')
