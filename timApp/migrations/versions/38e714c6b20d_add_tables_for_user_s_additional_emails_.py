"""Add tables for user's additional emails and verifications.

Revision ID: 38e714c6b20d
Revises: 0b21714ace95
Create Date: 2021-04-22 19:13:55.805398

"""

# revision identifiers, used by Alembic.
revision = '38e714c6b20d'
down_revision = '0b21714ace95'

from alembic import op
import sqlalchemy as sa

verification_type_enum = sa.Enum('LIST_JOIN', 'EMAIL_OWNERSHIP', name='verificationtype')


def upgrade():
    verification_type_enum.create(op.get_bind(), checkfirst=True)
    op.create_table('verifications',
                    sa.Column('id', sa.Integer(), nullable=False),
                    sa.Column('verification_type', verification_type_enum, nullable=True),
                    sa.Column('verification_pending', sa.DateTime(timezone=True), nullable=True),
                    sa.Column('verification_link', sa.Text(), nullable=True),
                    sa.Column('verified', sa.DateTime(timezone=True), nullable=True),
                    sa.PrimaryKeyConstraint('id')
                    )
    op.create_table('user_emails',
                    sa.Column('id', sa.Integer(), nullable=False),
                    sa.Column('additional_email', sa.Text(), nullable=True),
                    sa.Column('is_primary_email', sa.Boolean(), nullable=True),
                    sa.Column('address_verified', sa.DateTime(timezone=True), nullable=True),
                    sa.ForeignKeyConstraint(['id'], ['useraccount.id'], ),
                    sa.PrimaryKeyConstraint('id'),
                    sa.UniqueConstraint('additional_email')
                    )


def downgrade():
    op.drop_table('user_emails')
    op.drop_table('verifications')
    verification_type_enum.drop(op.get_bind())
