"""Add consentchange table

Revision ID: 55d075fdf05b
Revises: 32936448a2a1
Create Date: 2018-11-08 08:56:29.830995

"""

# revision identifiers, used by Alembic.
from sqlalchemy.dialects.postgresql import ENUM

revision = '55d075fdf05b'
down_revision = '32936448a2a1'

from alembic import op
import sqlalchemy as sa
from sqlalchemy.dialects import postgresql


def upgrade():
    op.create_table(
        'consentchange',
        sa.Column('id', sa.Integer(), nullable=False),
        sa.Column('user_id', sa.Integer(), nullable=False),
        sa.Column('time', sa.DateTime(timezone=True), nullable=False),
        sa.Column('consent', ENUM('CookieOnly', 'CookieAndData', name='consent', create_type=False), nullable=False),
        sa.ForeignKeyConstraint(['user_id'], ['useraccount.id'], ),
        sa.PrimaryKeyConstraint('id')
    )
    op.drop_column('useraccount', 'consent_time')


def downgrade():
    op.add_column('useraccount',
                  sa.Column('consent_time', postgresql.TIMESTAMP(timezone=True), autoincrement=False, nullable=True))
    op.drop_table('consentchange')
