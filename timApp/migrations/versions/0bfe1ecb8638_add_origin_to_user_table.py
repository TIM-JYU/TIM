"""Add origin to user table

Revision ID: 0bfe1ecb8638
Revises: ed73ec85993b
Create Date: 2019-05-17 14:12:43.323406

"""

# revision identifiers, used by Alembic.
revision = '0bfe1ecb8638'
down_revision = 'ed73ec85993b'

from alembic import op
import sqlalchemy as sa


def upgrade():
    e = sa.Enum(
        'Email',
        'Korppi',
        'Sisu',
        'Haka',
        'OpenID',
        'OpenIDConnect',
        'Facebook',
        'Google',
        'Twitter',
        name='userorigin',
    )
    e.create(op.get_bind(), checkfirst=True)
    op.add_column('useraccount', sa.Column('origin', e, nullable=True))


def downgrade():
    op.drop_column('useraccount', 'origin')
