"""Add consent columns to useraccount and remove unused yubikey.

Revision ID: 32936448a2a1
Revises: 27c624d9244a
Create Date: 2018-11-07 10:17:30.446453

"""

# revision identifiers, used by Alembic.
revision = '32936448a2a1'
down_revision = '27c624d9244a'

from alembic import op
import sqlalchemy as sa


e = sa.Enum('CookieOnly', 'CookieAndData', name='consent')


def upgrade():
    e.create(op.get_bind(), checkfirst=True)
    op.add_column('useraccount', sa.Column('consent', e, nullable=True))
    op.add_column('useraccount', sa.Column('consent_time', sa.DateTime(timezone=True), nullable=True))
    op.drop_column('useraccount', 'yubikey')


def downgrade():
    op.add_column('useraccount', sa.Column('yubikey', sa.TEXT(), autoincrement=False, nullable=True))
    op.drop_column('useraccount', 'consent_time')
    op.drop_column('useraccount', 'consent')
